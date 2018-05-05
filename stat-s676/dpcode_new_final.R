#new prior stuff
#used fixed theta, d, lambda and don't sample it if you can't deal with it

log_prior <- function(C,log_theta=0, d=.5, lambda=1, type="DP"){
  summary_C <- summary(reduce_C(as.factor(C)))
  theta <- exp(log_theta)
  k <- length(summary_C)
  if(type == "DP"){
    # https://en.wikipedia.org/wiki/Chinese_restaurant_process
    lgamma(exp(log_theta)) - 
      lgamma(exp(log_theta) + length(C)) + 
      sum(log_theta + lgamma(summary_C))
  }else if(type == "PY"){
    # https://en.wikipedia.org/wiki/Chinese_restaurant_process
    # our d is their alpha
    theta <- exp(log_theta)
    lgamma(theta) - lgamma(theta + length(C)) + 
      k * log(d) + lgamma(theta / d + k) - lgamma(theta / d) + 
      sum(summary_C - d) - lgamma(1 - d)
  }else{
    # problem 1
    log_lambda <- log(lambda)
    n <- length(C)
    log_M <- log_M_fun(n, log_lambda)
    log_D <- log_D_fun(seq(n), n, log_lambda)
    log_prior_multdir(C, log_M, log_D, log_lambda)
  }
}

#note all this code is for the dirichlet process

sample_theta = function(K,q,a=1,b=1){
  rgamma(1,K+a,-log(q)+b)
}#who cares
sample_q = function(theta,n){
  rbeta(1,theta,n)
}#who cares
reduce_C = function(C){
  C=C[,drop=TRUE]
  X = rep(NA,length(C))
  numC = as.numeric(C)
  unC = unique(numC)
  
  for(i in 1:length(unC)){
    X[numC==unC[i]] = i
  }
  as.factor(X)
}
log_dens_y_cond_C = function(y,C){
  C = reduce_C(C)
  ysqbar = ybarsq = out = n = 0
  for(clus in levels(C)){
    y_loc = y[C==clus]
    ysqbar = mean(y_loc^2)
    ybarsq = (mean(y_loc))^2
    m = length(y_loc)
    out = out+lgamma((m+1)/2)-(m+1)/2*log(pi)-(m+1)/2*log(1+(ysqbar-m/(m+1)*ybarsq)*m)
  }
  out
}
log_prior = function(C,log_theta){#prior
  summary_C = summary(reduce_C(as.factor(C)))
  lgamma(exp(log_theta))-lgamma(exp(log_theta)+length(C))+sum(log_theta+lgamma(summary_C))
}#prior
log_dens_rat_y_C_old_gibbs = function(y,C_old,j,k){
  #j is index of observation getting changed
  #k is index of cluster getting changed
  #C_old is old C, already in reduced form
  ind = which(C_old == k)
  L_ind = length(ind)
  ind_other = which(C_old == C_old[j])
  L_ind_other = length(ind_other)
  if(j%in%ind){return(0)}else{
    y_old_k = y[ind]
    y_old_other = y[ind_other]
    y_new_k = y[c(ind,j)]
    y_new_other = y[ind_other[ind_other!=j]]
    log_dens_new = log_dens_y_cond_C(c(y_new_k,y_new_other),as.factor(c(rep(1,L_ind+1),rep(2,L_ind_other-1))))
    log_dens_old = log_dens_y_cond_C(c(y_old_k,y_old_other),as.factor(c(rep(1,L_ind),rep(2,L_ind_other))))
    return(log_dens_new - log_dens_old)
  }
 
}
log_prior_rat_C_old_gibbs = function(C_old,j,k,log_theta){#prior
  #j is index of observation getting changed
  #k is index of cluster getting changed
  #C_old is old C, already in reduced form
  ind = which(C_old == k)
  L_ind = length(ind)
  ind_other = which(C_old == C_old[j])
  L_ind_other = length(ind_other)
  if(j%in%ind){return(0)}else{
    log_prior_old = ifelse(L_ind==0,0,lgamma(L_ind)+log_theta)+lgamma(L_ind_other)
    log_prior_new = lgamma(L_ind+1)+ifelse(L_ind_other==1,0,lgamma(L_ind_other-1)+log_theta)
    return(log_prior_new-log_prior_old)
  }
  
}#prior
sample_C_gibbs = function(C,y,log_theta){
  X = as.numeric(C)
  n = length(X)
  for(i in 1:n){
    K = length(unique(X))
    loc_num = sum(X==X[i])
    if(loc_num==1){
      j_max = K
    }else{
      j_max = K+1
    }
    log_prob = rep(NA,j_max)
    for(j in 1:j_max){
      log_prob[j] = log_dens_rat_y_C_old_gibbs(y,X,i,j) + log_prior_rat_C_old_gibbs(X,i,j,log_theta)#prior
    }
    prob = exp(log_prob-max(log_prob))
    prob = prob/sum(prob)
    j = sample(1:j_max,1,prob=prob)
    X[i] = j
    X = as.numeric(reduce_C(as.factor(X)))
  }
  return(as.factor(X))
}#prior
sample_C_gibbs_restricted = function(C_red,y,log_theta,S_not_ij){
  X = as.numeric(C_red)
  n = length(X)
  for(ell in S_not_ij){
    K = length(unique(X))
    log_prob = rep(NA,2)
    for(k in 1:2){
      log_prob[k] = log_dens_rat_y_C_old_gibbs(y,X,ell,k) + log_prior_rat_C_old_gibbs(X,ell,k,log_theta)#prior
    }
    prob = exp(log_prob-max(log_prob))
    prob = prob/sum(prob)
    k = sample(1:2,1,prob=prob)
    X[ell] = k
  }
  return(as.factor(X))
}#prior
split_log_prob_comp = function(C_launch,C_final,y_S,S_not_ij,log_theta){
  #note, we are in reduced space
  #call to prior works here because of the DP
  #it should also work for pitman-yor
  #it should work for any prior where the ratio of the probabilites of two partitions only depends
  #on the size of the clusters when the number of clusters is the same
  C_loc = C_launch
  C_other = C_loc
  log_prob_out = 0
  log_prob = rep(NA,1)
  for(k in S_not_ij){
    C_other[k] = as.numeric(C_loc[k])%%2+1
    log_prob[1] = log_dens_y_cond_C(y_S,C_loc)+log_prior(C_loc,log_theta)#prior
    log_prob[2] = log_dens_y_cond_C(y_S,C_other)+log_prior(C_other,log_theta)#prior
    log_prob = log_prob-max(log_prob)
    log_prob = log_prob-log(sum(exp(log_prob)))
    log_prob_out= log_prob_out + ifelse(C_final[k]==C_loc[k],log_prob[1],log_prob[2])
    C_other[k] = C_loc[k] = C_final[k]
  }
  return(log_prob_out)
}#prior
sample_C_split_merge = function(C,y,log_theta,N_launch = 1){
  K = length(levels(C))
  ij = sample(1:length(C),2,replace=FALSE)
  i = ij[1]
  j = ij[2]
  ij_C = C[ij]
  S = which(C%in%ij_C)
  S_not_ij = which(!(S%in%ij))
  #working in reduced space
  y_S = y[S]
  C_S = reduce_C(C[S,drop=TRUE])
  C_launch = C_S
  levels(C_launch) = c(1,2)
  if(ij_C[1]!=ij_C[2]){
    C_S[C_launch==C_launch[S==i]] = 1
    C_S[C_launch==C_launch[S==j]] = 2
    C_launch = C_S
  }else{
    C_launch[S==i] = 1
    C_launch[S==j] = 2
  }
  C_launch[S_not_ij] = sample(1:2,length(S_not_ij),replace=TRUE)
  C_launch = as.factor(C_launch)
  for(k in 1:N_launch){
    C_launch = sample_C_gibbs_restricted(C_launch,y_S,log_theta,S_not_ij)
  }
  if(ij_C[1] == ij_C[2]){#split
    split_merge = "split"
    C_prop_red = sample_C_gibbs_restricted(C_launch,y_S,log_theta,S_not_ij)
    log_prob_split = split_log_prob_comp(C_launch,C_prop_red,y_S,S_not_ij,log_theta)
    #go back to original space
    C_prop = C
    levels(C_prop) = c(levels(C_prop),K+1)
    C_prop[S[C_prop_red==C_prop_red[S==i]]]=C[i]
    C_prop[S[C_prop_red==C_prop_red[S==j]]]=K+1
    C_prop = reduce_C(C_prop)
    log_mh_alpha = log_dens_y_cond_C(y,C_prop) + log_prior(C_prop,log_theta)-log_dens_y_cond_C(y,C) - log_prior(C,log_theta) - log_prob_split#prior
  }else{#merge
    split_merge = "merge"
    log_prob_split = split_log_prob_comp(C_launch,C_S,y_S,S_not_ij,log_theta)
    #go back to original space
    C_prop = C
    C_prop[C_prop == C_prop[j]] = C_prop[i]
    C_prop = reduce_C(C_prop)
    log_mh_alpha = log_dens_y_cond_C(y,C_prop) + log_prior(C_prop,log_theta) - log_dens_y_cond_C(y,C) - log_prior(C,log_theta) + log_prob_split#prior
  }
  if(log(runif(1))<log_mh_alpha){
    return(C_prop)
    #return(list(C_new = C_prop, mh_prob = min(c(1,exp(log_mh_alpha))),split_merge = split_merge,accept=TRUE))
  }else{
    return(C)
    #return(list(C_new = C, mh_prob = min(c(1,exp(log_mh_alpha))),split_merge = split_merge,accept=FALSE))
  }
}



