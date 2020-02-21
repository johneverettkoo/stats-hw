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
    # construct reference bold signal
    ref.vec <- fmri::fmri.stimulus(scans = scans, 
                                   onsets = events.df$onset / timestep.size,
                                   durations = events.df$duration)
    ref.df <- dplyr::tibble(ref = ref.vec)
  } else {
    ref.df <- plyr::ddply(events.df, 'trial_type', function(temp.df) {
      # construct reference bold signal
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

construct.t.map <- function(t.stats, voxel.ind.array, alpha = .01) {
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

plot.tmap <- function(tmap.slice, title = NULL) {
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
    labs(fill = 't-statistic', title = title) + 
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(), 
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank())
}

create.contrast.map <- function(beta.hat, 
                                voxel.ind.array, 
                                normalize = FALSE,
                                params = c('abstract', 'concrete')) {
  contrast.param <- c(1, -1)
  beta.hat <- beta.hat[params, ]
  contrast.vec <- as.vector(contrast.param %*% beta.hat)
  
  T <- length(contrast.vec)
  voxel.array <- voxel.ind.array
  for (i in seq_along(t.stats)) {
    voxel.array[voxel.ind.array == i] <- contrast.vec[i]
  }
  
  return(voxel.array)
}

plot.contrast.map <- function(contrast.map.slice, title = NULL,
                              params = c('abstract', 'concrete')) {
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
    labs(fill = paste(params, collapse = '\nvs\n'), title = title) + 
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(), 
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank())
}