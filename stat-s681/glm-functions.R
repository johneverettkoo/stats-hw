#' @title read T1-weighted FMRI data and surrounding metadata
#' @description note that this assumes a very specific file structure
#'   for now, this discards task metainformation
#' @param subject (numeric/character) subject ID 1-10
#' @param session (numeric/character) session ID 1-10
#' @param task (character) the task performed by subject in experiment
#' @return (list) list of the BOLD timeseries, brain mask, events description,
#'   and additional regressors/metadata
load.fmri.data <- function(subject, session = 1, 
                           task = 'memorywords', 
                           combine.subtask = TRUE,
                           home.dir = '/N/project/clubneuro/MSC',
                           data.dir = 'fmriprep_ver1p3p2',
                           events.dir = 'washu_preproc/surface_pipeline') {
  # reformat subject and session ids to match file structure
  subject <- paste0('sub-MSC', 
                    stringr::str_pad(subject, 2, pad = '0', side = 'left'))
  session <- paste0('ses-func', 
                    stringr::str_pad(session, 2, pad = '0', side = 'left'))
  task <- paste('task', task, sep = '-')
  
  # paths for the BOLD data, brain mask, events, regressors/metadata
  data.path <- file.path(home.dir, data.dir, subject, session, 'func', 
                         paste(subject, session, task, 
                               'space-T1w', 'desc-preproc', 'bold.nii.gz',
                               sep = '_'))
  mask.path <- file.path(home.dir, data.dir, subject, session, 'func',
                         paste(subject, session, task,
                               'space-T1w', 'desc-brain', 'mask.nii.gz',
                               sep = '_'))
  events.path <- file.path(home.dir, events.dir, 
                           subject, 'task_timecourses', session, 
                           paste(subject, session, task, 'events.tsv',
                                 sep = '_'))
  reg.path <- file.path(home.dir, data.dir, subject, session, 'func',
                        paste(subject, session, task, 
                              'desc-confounds', 'regressors.tsv',
                              sep = '_'))
  
  # read the data
  t1w_bold.nifti <- neurobase::readNIfTI2(data.path)
  t1w_mask.nifti <- neurobase::readNIfTI2(mask.path)
  events.df <- readr::read_tsv(events.path) %>% 
    dplyr::filter(trial_type != 'err') %>% 
    dplyr::mutate(trial_type = strsplit(trial_type, '_') %>% 
                    sapply(function(x) x[1]) %>% 
                    tolower())
  reg.df <- readr::read_tsv(reg.path)
  
  # time between scans
  timestep.size <- t1w_bold.nifti@pixdim[5]
  # number of scans
  scans <- nrow(reg.df)
  
  # construct reference bold signal
  # create vector of reference BOLD responses
  # ref.vec <- fmri::fmri.stimulus(scans = scans + 1,
  #                                onsets = events.df$onset / timestep.size,
  #                                durations = events.df$duration,
  #                                TR = timestep.size)
  # ref.vec <- fmri::fmri.stimulus(scans = scans,
  #                                onsets = events.df$onset / timestep.size,
  #                                durations = events.df$duration,
  #                                TR = timestep.size)
  # ref.vec <- c(ref.vec[1], ref.vec)
  if (combine.subtask) {
    ref.vec <- fmri::fmri.stimulus(scans = scans, 
                                   onsets = events.df$onset / timestep.size,
                                   durations = events.df$duration)
    ref.df <- dplyr::tibble(ref = ref.vec)
  } else {
    ref.df <- plyr::ddply(events.df, 'trial_type', function(temp.df) {
      ref.vec <- fmri::fmri.stimulus(scans = scans,
                                     onsets = temp.df$onset / timestep.size,
                                     durations = temp.df$duration)
      dplyr::tibble(t = seq(scans),
                    ref = ref.vec)
    }) %>% 
      tidyr::spread(trial_type, ref) %>% 
      dplyr::select(-t)
  }
  
  # and attach to reg.df
  reg.df %<>% 
    cbind(ref.df)
  
  # add linear and quadratic drift terms to reg.df
  # and timepoints
  reg.df %<>% 
    dplyr::mutate(drift = seq(dplyr::n()),
                  drift = drift / max(drift),
                  drift2 = drift ** 2,
                  time.s = seq(timestep.size, timestep.size * scans, 
                               length.out = scans))
  
  # return
  return(list(t1w_bold.nifti = t1w_bold.nifti, 
              t1w_mask.nifti = t1w_mask.nifti,
              reg.df = reg.df))
}

construct.TxV <- function(bold, mask, 
                          remove.global = FALSE, 
                          global.signal = NULL) {
  # construct a TxV matrix
  # as well as a reference array of voxels to map back to
  # there is a more efficient way of doing this
  
  # dimensions of scans in voxels
  dims <- dim(mask)
  
  # construct empty array of voxel indices
  voxel.ind.array <- array(rep(NA, dims[1] * dims[2] * dims[3]),
                           dim = dims)
  # construct empty TxV array
  Y <- matrix(rep(NA, dim(bold)[4] * sum(mask)),
              nrow = dim(bold)[4],
              ncol = sum(mask))
  
  # valid voxel counter
  v <- 0
  
  # loop over the voxel dimensions
  for (i in seq(dims[1])) {
    for (j in seq(dims[2])) {
      for (k in seq(dims[3])) {
        if (mask[i, j, k] == 1) {
          v <- v + 1
          voxel.ind.array[i, j, k] <- v
          Y[, v] <- bold[i, j, k, ]
        }
      }
    }
  }
  
  if (remove.global) {
    Y <- sweep(Y, 1, global.signal, '-')
  }
  
  return(list(Y = Y,
              voxel.ind.array = voxel.ind.array))
}

.txv.check <- function(y, voxel.ind.array, tstep = 1) {
  voxel.array <- voxel.ind.array
  for (i in seq(ncol(y))) {
    voxel.array[voxel.ind.array == i] <- y[tstep, i]
  }
  return(voxel.array)
}

construct.t.map <- function(t.stats, voxel.ind.array, alpha = .01, p = 10) {
  # again, this is not a good/efficient way to do this ...
  T <- length(t.stats)
  cv <- qt(1 - alpha / 2, T - p)
  voxel.array <- voxel.ind.array
  for (i in seq_along(t.stats)) {
    if (abs(t.stats[i]) >= cv) {
      voxel.array[voxel.ind.array == i] <- t.stats[i]
    } else {
      voxel.array[voxel.ind.array == i] <- 0
    }
  }
  return(voxel.array)
}

plot.tmap <- function(tmap.slice, title = NULL, legend = 't-stat') {
  tmap.slice %>%
    dplyr::as_tibble() %>% 
    tibble::rowid_to_column(var = 'x') %>% 
    tidyr::gather(key = 'y', value = 'z', -1) %>% 
    dplyr::mutate(y = as.numeric(gsub('V', '', y))) %>% 
    na.omit() %>% 
    ggplot() + 
    geom_tile(aes(x = x, y = y, fill = z)) + 
    # viridis::scale_fill_viridis() + 
    scale_fill_gradient2(mid = 'grey90') + 
    coord_fixed() + 
    labs(fill = legend, title = title) + 
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(), 
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank())
}

create.contrast.map <- function(beta.hat, 
                                voxel.ind.array, 
                                alpha = .01,
                                params = c('abstract', 'concrete'),
                                contrast.param = c(1, -1)) {
  T <- ncol(beta.hat)
  p <- nrow(beta.hat)
  cutoff <- qt(1 - alpha / 2, T - p)
  beta.hat <- beta.hat[params, ]
  sp2 <- pooled.var(beta.hat[params[1], ],
                    beta.hat[params[2], ])
  contrast.vec <- as.vector(contrast.param %*% beta.hat) / sqrt(sp2)
  
  voxel.array <- voxel.ind.array
  for (i in seq(T)) {
    if (abs(contrast.vec[i]) > cutoff) {
      voxel.array[voxel.ind.array == i] <- contrast.vec[i]
    } else {
      voxel.array[voxel.ind.array == i] <- 0
    }
  }
  
  return(voxel.array)
}

plot.contrast.map <- function(contrast.map.slice, title = 't-map',
                              params = c('abstract', 'concrete'),
                              op = '-') {
  contrast.map.slice %>%
    dplyr::as_tibble() %>% 
    tibble::rowid_to_column(var = 'x') %>% 
    tidyr::gather(key = 'y', value = 'z', -1) %>% 
    dplyr::mutate(y = as.numeric(gsub('V', '', y))) %>% 
    na.omit() %>% 
    ggplot() + 
    geom_tile(aes(x = x, y = y, fill = z)) + 
    scale_fill_gradient2(mid = 'grey90') + 
    coord_fixed() + 
    labs(fill = paste(params, collapse = paste0('\n', op, '\n')), 
         title = title) + 
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(), 
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank())
}

pooled.var <- function(x1, x2) {
  n1 <- length(x1)
  n2 <- length(x2)
  n <- n1 + n2
  sp2 <- ((n1 - 1) * var(x1) + (n2 - 1) * var(x2)) / (n - 2)
  return(sp2)
}

fit.glm <- function(bold.array,
                    mask.array,
                    reg.df, 
                    V = NULL,
                    task.cols = c('abstract', 'concrete'),
                    remove.global = FALSE) {
  Y.list <- construct.TxV(bold.array,
                          mask.array, 
                          remove.global = remove.global,
                          global.signal = reg.df$global_signal)
  model.formula <- paste('~', paste(task.cols, collapse = ' + '), '+', 
                         'trans_x + trans_y + trans_z + ',
                         'rot_x + rot_y + rot_z') %>% 
    as.formula()
  
  X <- model.matrix(model.formula, data = reg.df)
  Y <- Y.list$Y
  if (!is.null(V)) {
    Y <- V %*% Y
    X <- V %*% X
  }
  
  T <- nrow(Y)
  V <- ncol(Y)
  p <- ncol(X)
  
  # compute beta hat
  XtX.inv <- solve(t(X) %*% X)
  beta.hat <- XtX.inv %*% t(X) %*% Y
  
  # compute y hat
  Y.hat <- X %*% beta.hat
  
  # compute residuals
  E <- Y - Y.hat
  RSS <- apply(E, 2, function(x) sum(x ** 2))
  sigma2.hat <- RSS / (T - p)
  
  # t statistics for the betas of interest
  XtX.inv.diag <- diag(XtX.inv)
  se.betahat.abs <- sqrt(sigma2.hat * XtX.inv.diag[task.cols[1]])
  se.betahat.con <- sqrt(sigma2.hat * XtX.inv.diag[task.cols[2]])
  t.stats <- cbind(beta.hat[task.cols[1], ] / se.betahat.abs,
                   beta.hat[task.cols[2], ] / se.betahat.con)
  colnames(t.stats) = task.cols
  
  return(list(beta = beta.hat,
              resid = E,
              sigma2 = sigma2.hat,
              t.stats = t.stats,
              voxel.ind.array = Y.list$voxel.ind.array))
}

estimate.V <- function(start.vec, ndim) {
  if (length(start.vec) > ndim) {
    stop('bad parameters')
  }
  
  k <- length(start.vec) - 1
  
  first.row <- c(start.vec, rep(NA, ndim - k - 1))
  for (i in seq(k + 2, ndim)) {
    first.row[i] <- sum(sapply(seq(k), function(j) {
      start.vec[j + 1] * first.row[i - j]
    }))
  }
  
  V <- toeplitz(first.row)
  return(V)
}

estimate.W <- function(start.vec, ndim) {
  V <- estimate.V(start.vec, ndim)
  W <- chol(solve(V))
  return(W)
}
