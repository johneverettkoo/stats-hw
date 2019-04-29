sbm.gibbs <- function(A, k = 2, alpha = NULL, 
                      a.q = 1, 
                      b.q = 1, 
                      iter = 1e3, z.init = NULL, 
                      plot.last = TRUE) {
  # validity checks
  
  # make sure A is a matrix
  if (!is.matrix(A)) {
    stop(simpleError('affinity matrix must be a matrix'))
  }
  
  # make sure A is symmetric
  if (any(A != t(A))) { 
    stop(simpleError('affinity matrix must be symmetric'))
  }
  
  # make sure number of clusters makes sense
  if ((k != round(k)) | (k < 2)) {
    stop(simpleError('number of clusters must be an integer of at least 2'))
  }
  
  # number of nodes
  n <- nrow(A)
  
  # if no initial cluster configuration is specified, randomly initialize
  if (is.null(z.init)) {
    z <- sample(seq(k), n, replace = TRUE)
  } else {
    z <- z.init
  }
  
  # if no alpha specified, use "default"
  if(is.null(alpha)) alpha <- rep(1, k)
  
  # make sure alpha is of dimension k
  if (length(alpha) != k) {
    stop(simpleError('alpha must be of length k'))
  }
  
  # preallocate
  Q.out <- array(dim = c(iter, k, k))
  p.out <- matrix(nrow = iter, ncol = k)
  z.out <- matrix(nrow = iter, ncol = n)
  p.z.out <- array(dim = c(iter, n, k))
  
  # mcmc
  for (S in seq(iter)) {
    # sample statistics
    n.r <- sapply(seq(k), function(r) {
      sum(z == r)
    })
    n.rs <- sapply(seq(k), function(r) {
      sapply(seq(k), function(s) {
        n.r[r] * n.r[s] - n.r[r] * (r == s)
      })
    })
    A.rs <- sapply(seq(k), function(r) {
      sapply(seq(k), function(s) {
        sum(A[z == r, z == s])
      })
    })
    
    # draw p
    p <- as.vector(gtools::rdirichlet(1, alpha + n.r))
    
    # draw Q
    # Q <- sapply(seq(k), function(r) {
    #   sapply(seq(k), function(s) {
    #     # rbeta(1, a.q + A.rs[r, s], b.q + n.rs[r, s] - A.rs[r, s])
    #     a <- rgamma(1, a.q + A.rs[r, s], 1)
    #     b <- rgamma(1, b.q + n.rs[r, s] - A.rs[r, s], 1)
    #     a / (a + b)
    #   })
    # })
    log.Q <- matrix(nrow = k, ncol = k)
    log.1.minus.Q <- matrix(nrow = k, ncol = k)
    k. <- 1
    for (r in seq(k)) {
      for (s in seq(k)) {
        a <- rgamma(1, a.q + A.rs[r, s], k.)
        b <- rgamma(1, b.q + n.rs[r, s] - A.rs[r, s], k.)
        log.Q[r, s] <- log(a) - log(a + b)
        log.1.minus.Q[r, s] <- log(b) - log(a + b)
      }
    }
    
    # draw z
    # for (i in seq(n)) {
    #   log.p.z.unnorm <- sapply(seq(k), function(l) {
    #     log(p[l]) +
    #       sum(A[i, -i] * log(Q[l, z[-i]]) +
    #             (1 - A[i, -i]) * log(1 - Q[l, z[-i]])) +
    #       sum(A[-i, i] * log(Q[z[-i], l]) +
    #             (1 - A[-i, i]) * log(1 - Q[z[-i], l]))
    #   })
    p.z <- matrix(nrow = n, ncol = k)
    for (i in seq(n)) {
      log.p.z.unnorm <- sapply(seq(k), function(l) {
        log(p[l]) +
          sum(A[i, -i] * log.Q[l, z[-i]] +
                (1 - A[i, -i]) * log.1.minus.Q[l, z[-i]]) +
          sum(A[-i, i] * log.Q[z[-i], l] +
                (1 - A[-i, i]) * log.1.minus.Q[z[-i], l])
      })
      log.p.z.unnorm <- log.p.z.unnorm - max(log.p.z.unnorm)
      p.z.unnorm <- exp(log.p.z.unnorm)
      p.z.norm <- p.z.unnorm / sum(p.z.unnorm)
      z[i] <- which(rmultinom(1, 1, p.z.norm) == 1)
      p.z[i, ] <- p.z.norm
    }
    
    # store
    Q.out[S, , ] <- exp(log.Q)
    p.out[S, ] <- p
    z.out[S, ] <- z
    p.z.out[S, , ] <- p.z
  }
  
  # plot final configuration
  if (plot.last) qgraph::qgraph(A, groups = factor(z))
  
  # return values
  return(list(Q = Q.out, 
              p = p.out, 
              z = z.out, 
              p.z = p.z.out))
}


p.Q.trace.plot <- function(sbm.result, k = 2) {
  iter <- nrow(sbm.result$p)
  if (k == 2) {
    p.plot <- ggplot() + 
      geom_line(aes(x = seq(iter), y = sbm.result$p[, 1])) + 
      labs(y = expression(P(z[i]*'='*1)), x = 'iteration')
  } else if (k == 3) {
    p.plot <- ggplot() + 
      geom_line(aes(x = seq(iter), y = sbm.result$p[, 1], 
                    colour = 'P(z = 1)')) + 
      geom_line(aes(x = seq(iter), y = sbm.result$p[, 2], 
                    colour = 'P(z = 2)')) + 
      geom_line(aes(x = seq(iter), y = sbm.result$p[, 3], 
                    colour = 'P(z = 3)')) + 
      scale_colour_brewer(palette = 'Set1') + 
      labs(y = expression(P(z[i])), x = 'iteration', colour = NULL) + 
      theme(legend.position = 'bottom')
  } else {
    stop(simpleError('only k = 2 or k = 3 currently supported'))
  }
  
  Q.plot <- ggplot() + 
    geom_line(aes(x = seq(iter), y = sbm.result$Q[, 1, 1], 
                  colour = 'within cluster 1')) + 
    geom_line(aes(x = seq(iter), y = sbm.result$Q[, 2, 2], 
                  colour = 'within cluster 2')) + 
    geom_line(aes(x = seq(iter), y = sbm.result$Q[, 1, 2], 
                  colour = 'between clusters 1 and 2')) + 
    labs(x = 'iteration', y = 'edge probability', colour = NULL) + 
    theme(legend.position = 'bottom') 
  
  if (k == 3) {
    Q.plot <- Q.plot + 
      geom_line(aes(x = seq(iter), y = sbm.result$Q[, 3, 3], 
                    colour = 'within cluster 3')) + 
      geom_line(aes(x = seq(iter), y = sbm.result$Q[, 1, 3], 
                    colour = 'between clusters 1 and 3')) + 
      geom_line(aes(x = seq(iter), y = sbm.result$Q[, 2, 3], 
                    colour = 'between clusters 2 and 3')) + 
      scale_colour_brewer(palette = 'Set1')
  } else if (k > 3) {
    stop(simpleError('only k = 2 k = 3 currently supported'))
  }
  
  gridExtra::grid.arrange(p.plot, Q.plot)
}

vertex.quantile.plot <- function(sbm.result, 
                                 k = 2, 
                                 burn.in = 200) {
  if (k == 2) {
    plyr::adply(sbm.result$p.z, 2, function(p) {
      quantile(p[-seq(burn.in), 1], c(.025, .5, .975))
    }) %>% 
      ggplot() + 
      geom_errorbar(aes(x = X1, ymin = `2.5%`, ymax = `97.5%`)) + 
      geom_point(aes(x = X1, y = `50%`)) +
      labs(x = 'vertex')
  } else if (k == 3) {
    dplyr::bind_rows(
      plyr::adply(sbm.result$p.z, 2, function(p) {
        quantile(p[-seq(burn.in), 1], c(.025, .5, .975))
      }) %>% 
        dplyr::mutate(k = 1), 
      plyr::adply(sbm.result$p.z, 2, function(p) {
        quantile(p[-seq(burn.in), 2], c(.025, .5, .975))
      }) %>% 
        dplyr::mutate(k = 2), 
      plyr::adply(sbm.result$p.z, 2, function(p) {
        quantile(p[-seq(burn.in), 3], c(.025, .5, .975))
      }) %>% 
        dplyr::mutate(k = 3)
    ) %>% 
      dplyr::mutate(k = factor(k)) %>% 
      ggplot() + 
      geom_errorbar(aes(x = X1, ymin = `2.5%`, ymax = `97.5%`, colour = k)) + 
      labs(x = 'vertex', colour = 'cluster') + 
      scale_colour_brewer(palette = 'Set1')
  } else {
    stop(simpleError('k = 2 or k = 3'))
  }
}

spectral.clustering <- function(W, k = 2, d = 2, plot.cluster = TRUE) {
  L <- graph.laplacian(W)
  L.dagger <- MASS::ginv(L)
  L.eigen <- eigen(L)
  n <- nrow(L)
  X <- apply(L.eigen$vectors[, seq(n - 1)], 1, function(x) {
    x / sqrt(L.eigen$values[seq(n - 1)])
  }) %>% 
    t()
  X.d <- X[, seq(n - 1, n - d)]
  
  clusters <- kmeans(X.d, k)$cluster
  
  if (plot.cluster) {
    if (d != 2) {
      stop(simpleError('plotting not supported for d > 2'))
    }
    out.plot <- ggplot() + 
      geom_text(aes(x = X.d[, 1], y = X.d[, 2], colour = factor(clusters), 
                    label = seq(n))) + 
      labs(x = 'PC1', y = 'PC2', colour = NULL) + 
      coord_fixed()
    out.plot
  }
  
  return(list(X = X.d, cluster = clusters, plot = out.plot))
}

.construct.double.spiral.graph <- function() {
  set.seed(112358)
  eps <- 2 ** -2
  K <- 10  # for constructing the knn graph
  rad.max <- 10
  ang.max <- 2 * pi
  angles <- seq(0, ang.max, length.out = 100)
  radii <- seq(1, sqrt(rad.max), length.out = 100) ** 2
  
  # data
  spiral.df <- dplyr::data_frame(X = radii * cos(angles), 
                                 Y = radii * sin(angles))
  spiral.df <- dplyr::data_frame(X = radii * cos(angles), 
                                 Y = radii * sin(angles))
  neg.spiral.df <- dplyr::mutate(spiral.df, 
                                 X = -X, Y = -Y, 
                                 id = '2')
  spiral.df %<>% 
    dplyr::mutate(id = '1') %>% 
    dplyr::bind_rows(neg.spiral.df) %>% 
    dplyr::mutate(X = X + rnorm(n = n(), sd = eps), 
                  Y = Y + rnorm(n = n(), sd = eps))
  n <- nrow(spiral.df)
  
  # construct similarity graph
  W <- spiral.df %>% 
    dplyr::select(X, Y) %>% 
    as.matrix() %>% 
    mds.edm1() %>% 
    graph.knn(K) %>% 
    graph.adj()
  
  return(W)
}