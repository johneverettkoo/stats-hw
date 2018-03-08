// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(RcppGSL)]]

#define _USE_MATH_DEFINES
#include<iostream>
#include<armadillo>
#include<math.h>
#include<vector>
#include<gsl/gsl_sf_gamma.h>
#include<RcppArmadillo.h>
#include<RcppGSL.h>

void pseudo_inverse_and_rank
(
  const arma::mat &W,
  arma::mat& p_inv,
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
  p_inv = EVEC*arma::diagmat(EVAL)*EVEC.t();
}

void hat_diag
(
  const arma::mat& X,
  const arma::mat& XtX_pinv,
  arma::vec& h
){
  arma::mat h_hold(1,1);
  for(unsigned int i=0; i<X.n_rows; ++i){
    h_hold = X.row(i) * XtX_pinv * (X.row(i).t());
    h(i) = h_hold(0,0);
  }
}

void bit_increase
(
 std::vector<bool>& model_bool
)
{
  unsigned int i=1;
  while(model_bool[i]==true && i<model_bool.size()){
    model_bool[i] = false;
    i += 1;
  }
  model_bool[i] = true;
}

  

arma::uvec model_bool_indices_convert
(
 const std::vector<bool>& model_bool
)
{
  std::vector<unsigned int> model_indices_hold;
  for(unsigned int i=0; i<model_bool.size(); ++i){
    if(model_bool[i]){
      model_indices_hold.push_back(i);
    }
  }
  arma::uvec model_indices(model_indices_hold.size());
  for(unsigned int i=0;i<model_indices_hold.size();++i){
    model_indices(i) = model_indices_hold[i];
  }
  return(model_indices);
}

void compute_beta_sigmasq_hat
(
 const arma::vec& Xty_loc,
 const arma::mat& XtX_pinv_loc,
 const double& yty,
 const unsigned int& n,
 arma::vec& beta_hat,
 double& sigmasq_hat,
 double& marg_RSS
){
  beta_hat = XtX_pinv_loc * Xty_loc;
  arma::mat RST_hold = (Xty_loc.t() * beta_hat);
  double RST=RST_hold(0,0);
  sigmasq_hat = (yty - RST)/n;
  marg_RSS = (yty - n*RST/(n+1));
}

//[[Rcpp::export]]
Rcpp::List search_algo
(
 const arma::vec &y,
 const arma::mat &X_covariates
)
{
  unsigned int p = X_covariates.n_cols;
  unsigned int n = X_covariates.n_rows;
  
  arma::mat X(n,p+1);
  X.col(0) = arma::ones<arma::vec>(n);
  X.cols(1,p) = X_covariates;
  
  arma::mat XtX = X.t()*X;
  arma::vec Xty = X.t()*y;
  arma::mat yty_hold = (y.t()*y);
  double yty = yty_hold(0,0);
  
  const double alpha = 1;
  const double beta = 1;
  const double logn = std::log(static_cast<double>(n));
  const double lognp1 = std::log(static_cast<double>(n+1));
  const double np1over2 = static_cast<double>(n+1)/2;
  const double nover2 = static_cast<double>(n)/2;
  const double log_density_at_MLE_const = -nover2*std::log(2*M_PI*M_E);
  const double log_MARG_const = gsl_sf_lngamma((n+alpha)/2)-nover2*std::log(M_PI)-gsl_sf_lngamma(alpha/2)+alpha*std::log(beta);
  
  
  unsigned int N_mod = std::pow(2,p);

  std::vector<bool> model_bool(p+1,false);
  model_bool[0] = true;

  arma::uvec model_indices;
  arma::mat XtX_loc;
  arma::mat XtX_pinv_loc;
  arma::mat X_loc;
  arma::vec Xty_loc;
  arma::vec beta_hat_loc;
  arma::vec h_loc(n);
  arma::vec residuals_loc(n);
  arma::vec residuals_sq_loc(n);
  double sigmasq_hat_loc;
  double marg_RSS_loc;
  double neg_log_dens_loc;
  unsigned int rank_loc;
  
  arma::vec AIC(N_mod);
  arma::vec BIC(N_mod);
  arma::vec TIC(N_mod);
  arma::vec MARG(N_mod);
  
  Rcpp::List model_id(N_mod);

  arma::mat TICm1(1,1), TICm2(1,1);
  
  for(unsigned int i=0;i<N_mod;++i){
    model_indices=model_bool_indices_convert(model_bool);
    model_id[i] = model_indices;
    XtX_loc = XtX(model_indices,model_indices);
    Xty_loc = Xty(model_indices);
    X_loc = X.cols(model_indices);
    pseudo_inverse_and_rank(XtX_loc,XtX_pinv_loc,rank_loc);
    compute_beta_sigmasq_hat(Xty_loc,XtX_pinv_loc,yty,
                             static_cast<double>(n),beta_hat_loc,
                             sigmasq_hat_loc,marg_RSS_loc);
    residuals_loc = y-X_loc*beta_hat_loc;
    residuals_sq_loc = residuals_loc%residuals_loc;
    hat_diag(X_loc,XtX_pinv_loc,h_loc);
    neg_log_dens_loc = nover2*std::log(sigmasq_hat_loc);
    AIC(i) = 2*neg_log_dens_loc+2*rank_loc;
    BIC(i) = 2*neg_log_dens_loc+rank_loc*logn;
    MARG(i) = np1over2*std::log(beta+marg_RSS_loc)-rank_loc*lognp1/2;
    TICm1=residuals_sq_loc.t()*h_loc;
    TICm2=residuals_sq_loc.t()*residuals_sq_loc/n;
    TIC(i) =  2*neg_log_dens_loc + 2*(TICm1(0,0))/sigmasq_hat_loc+
      (TICm2(0,0))/std::pow(sigmasq_hat_loc,2)-1;
    bit_increase(model_bool);
  }

  return(Rcpp::List::create(Rcpp::Named("model_id")=model_id,
                            Rcpp::Named("AIC")=AIC,
                            Rcpp::Named("BIC")=BIC,
                            Rcpp::Named("MARG")=MARG,
                            Rcpp::Named("TIC")=TIC));
}
