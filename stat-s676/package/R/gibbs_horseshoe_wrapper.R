#' @title Horseshoe Gibbs sampler for linear models
#' @description This is just a wrapper for the C++ code.
#' @param y (numeric) An n-dimensional vector of responses
#' @param X (numeric) An n by p dimensional matrix of inputs
#' @param n.iter (numeric) The number of iterations to output
#' @param burn (numeric) Number of iterations for burn-in
#' @param thin (numeric) Thinning rate
#' @param seed (numeric) RNG seed (defaults to system time)
#' @return (list) The estimates for the covariates, sigma^2, and tau
#' @export gibbs.horseshoe
gibbs.horseshoe <- function(y, X, 
                            n.iter = 1e3, burn = 1e2, thin = 1e2, 
                            seed = NULL) {
  # rng stuff
  if (is.null(seed)) seed <- as.numeric(Sys.time())
  
  # pass to C++ code
  out <- lm_mcmc_horseshoe(y, X, burn + n.iter * thin, seed)
  
  # take out burn-in
  beta. <- out$beta[-seq(burn), ]
  sigma_sq <- out$sigma_sq[-seq(burn)]
  tau <- out$tau[-seq(burn), ]
  
  # thin
  beta. <- beta.[seq(to = n.iter * thin, by = thin), ]
  sigma_sq <- sigma_sq[seq(to = n.iter * thin, by = thin)]
  tau <- tau[seq(to = n.iter * thin, by = thin), ]
  
  return(list(beta = beta., 
              sigma_sq = sigma_sq, 
              tau = tau))
}