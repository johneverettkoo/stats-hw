// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(RcppGSL)]]

#include<iostream>
#include<armadillo>
#include<RcppArmadillo.h>
#include<gsl/gsl_rng.h>
#include<gsl/gsl_randist.h>


void pseudo_inverse_and_rank
(
  const arma::mat &W,
  arma::mat& p_inv,
  arma::mat& sqrt_pinv_mat,
  unsigned int& rank
)
{
  unsigned int dims = W.n_cols;
  arma::vec EVAL(dims);
  arma::mat EVEC(dims,dims);
  double tol = static_cast<double>(dims)*EVAL.max()*arma::datum::eps;
  arma::eig_sym(EVAL,EVEC,W);
  arma::uvec inds1 = arma::find(EVAL>tol);
  arma::uvec inds0 = arma::find(EVAL<=tol);
  EVAL.elem(inds0).fill(0);
  arma::vec EE=arma::ones<arma::vec>(inds1.n_elem);
  EVAL.elem(inds1)=EE/EVAL.elem(inds1);
  rank = inds1.n_elem;
  sqrt_pinv_mat = EVEC*arma::diagmat(arma::sqrt(EVAL));
  p_inv = sqrt_pinv_mat*sqrt_pinv_mat.t();
}

arma::vec rand_norm(const gsl_rng * r,unsigned int d){
  arma::vec out(d);
  for(unsigned int i=0;i<d;++i){
    out(i)=gsl_ran_ugaussian_ratio_method(r);
  }
  return(out);
}

// http://gallery.rcpp.org/articles/simulate-multivariate-normal/
arma::mat mvrnormArma(int n, arma::vec mu, arma::mat sigma) {
  int ncols = sigma.n_cols;
  arma::mat Y = arma::randn(n, ncols);
  return arma::repmat(mu, 1, n).t() + Y * arma::chol(sigma);
}

//[[Rcpp::export]]
Rcpp::List lm_mcmc_horseshoe
(
  const arma::vec &y,
  const arma::mat &X_covariates,
  const unsigned int& N_sims,
  const unsigned long int& seed
)
{
  unsigned int p = X_covariates.n_cols;
  unsigned int n = X_covariates.n_rows;
  double p_double = static_cast<double>(p);
  double n_double = static_cast<double>(n);
  double gamma_shape = (n_double + p_double + 2.0) * 0.5;
  double gamma_rate;
  
  arma::mat X(n,p+1);
  X.col(0) = arma::ones<arma::vec>(n);
  X.cols(1,p) = X_covariates;
  
  // precompute stuff
  arma::mat XtX = X.t()*X;
  arma::vec Xty = X.t()*y;
  double yty = arma::as_scalar(y.t()*y);
  arma::mat XtX_pinv;
  arma::mat XtX_pinv_sqrt;
  unsigned int rank;
  pseudo_inverse_and_rank(XtX,XtX_pinv,XtX_pinv_sqrt,rank);
  
  // init
  arma::vec beta = XtX_pinv*Xty;
  // arma::vec beta_mean = beta*n_double/(1+n_double);
  double sigma_sq = yty - arma::as_scalar(Xty.t()*beta);
  arma::vec tau = arma::ones<arma::vec>(p+1);
  arma::vec lambda = arma::ones<arma::vec>(p+1);
  double phi = 1.0;
  double eta = 1.0;
  
  // outputs
  arma::vec sigma_sq_out(N_sims);
  arma::mat beta_out(N_sims, p+1);
  arma::mat tau_out(N_sims, p+1);
  
  // set up rng
  const gsl_rng_type * T;
  gsl_rng * r;
  gsl_rng_env_setup();
  T = gsl_rng_mt19937;
  r = gsl_rng_alloc (T);
  gsl_rng_set(r,seed);
  
  for(unsigned int i = 0; i < N_sims; ++i) {
    // draw beta
    beta = mvrnormArma(
      1, 
      arma::vectorise(arma::inv(XtX + arma::diagmat(tau)) * Xty), 
      sigma_sq * arma::inv(XtX + arma::diagmat(tau))
    ).t();
    beta_out.row(i) = beta.t();
    
    // draw sigma_sq
    gamma_rate = arma::as_scalar(
      0.5 * dot(y - X * beta, y - X * beta) + 
      dot(tau % beta, beta)
    ) + 1.0;
    sigma_sq = 1.0 / R::rgamma(gamma_shape, 1.0 / gamma_rate);
    sigma_sq_out(i) = sigma_sq;
    
    // draw tau and lambda
    for (unsigned int j = 0; j < p+1; ++j) {
      tau(j) = arma::as_scalar(
        R::rexp(1.0) / 
          arma::as_scalar(beta(j) * beta(j) / 2.0 / sigma_sq + lambda(j) / 2.0)
      );
      lambda(j) = R::rexp(1.0) / arma::as_scalar(tau(j) / 2.0 + phi / 2.0);
    }
    tau_out.row(i) = tau.t();
    
    // draw phi
    phi = R::rgamma((p_double + 1.0) / 2.0, 
                    1.0 / (arma::sum(lambda) / 2.0 + eta / 2.0));
    
    // draw eta
    eta = R::rexp(phi / 2.0 + 0.5);
  }
  
  gsl_rng_free(r);
  
  return(Rcpp::List::create(Rcpp::Named("beta")=beta_out,
                            Rcpp::Named("sigma_sq")=sigma_sq_out,
                            Rcpp::Named("tau")=tau_out));
}