
#Probability of Z_i = j given X_i = x_i for calculating Q
p_zi_j_xi = function(j,ws,mus,Sigmas,x){
  x=matrix(x,nrow=1)
  denom = mvnfast::dmixn(x,mu=mus,sigma=Sigmas,w=ws)
  return (mvnfast::dmvn(x,mu=mus[j,],sigma=Sigmas[[j]])*ws[j]/denom)
}

#Expected log-likelihood function for Gaussian mixture model
Q_theta_theta_t = function(ws,mus,Sigmas,ws_old,mus_old,Sigmas_old,X){
  running_total = 0
  K = length(ws)
  n = dim(X)[1]
  d = dim(X)[2]
  for (i in 1:n){
    for (j in 1:K){
      running_total = running_total + p_zi_j_xi(j,ws_old,mus_old,Sigmas_old,X[i,])*
        (log(ws[j]) - 0.5*log(det(Sigmas[[j]])) - 0.5*d*log(2*pi) - 0.5*((X[i,] - mus[j,]) %*% solve(Sigmas[[j]]) %*% (X[i,] - mus[j,]) ))
    }
  }
  return(running_total)
}

#' Gaussian Mixture Fitting via the EM Algorithm
#'
#' @param X Data matrix with observations as rows.
#' @param K Number of components the Gaussian mixture should use
#' @param mus Optional parameter that provides starting centres for the Gaussian components. Matrix whose rows are vectors giving the position of each centre. If not provided, centres will be chosen at random from a normal distribution based on the entire data matrix.
#'
#' @return A list containing the fitted parameters `ws`, `mus`, and `Sigmas`. `ws` is a vector containing the weights for each components of the mixture. `mus` is a matrix whose rows are the centres of each component of the mixture. `Sigmas` is a list containing the covariances matrices for each component of the mixture.
#'
#' @keywords internal
#'
#' @examples
#' X = rbind(c(0,0),c(-1,0),c(1,0),c(0,1),c(0,-1),c(5,5),c(4,5),c(5,6),c(6,5),c(5,4))
#' K = 2
#' start_centres = rbind(c(-1,-1),c(6,6))
#' par_list = chicagomodel:::gmm_em_fit(X,K,start_centres)
gmm_em_fit = function(X,K,mus=NULL){

  #Initialise parameters arbitrarily, except for mus if provided
  n = dim(X)[1]
  d = dim(X)[2]
  ws = rep(1/K,K)
  if(is.null(mus)){
    mus = replicate(K,mvnfast::rmvn(1,mu=apply(X,2,mean),sigma=diag(1,d))[1,]) #Get a matrix of mean vectors that are distributed like the entire dataset
  }
  sigmas = replicate(K,diag(1,d),simplify=FALSE) #Start all components with identity covariance

  old_Q = -Inf #Keep track of the previous iteration's expected log-likelihood for termination
  new_Q = Q_theta_theta_t(ws,mus,sigmas,ws,mus,sigmas,X)

  #Main loop of EM Algorithm
  while (new_Q > old_Q + 0.00001){

    old_Q = new_Q

    #Matrix of p_zi_j_xi values. Essentially the E step
    T = matrix(0,n,K)
    for (i in 1:n){
      for (j in 1:K){
        T[i,j] = p_zi_j_xi(j,ws,mus,sigmas,X[i,])
      }
    }

    #Update parameters - M step
    for (j in 1:K){
      ws[j] = sum(T[,j])/sum(T)

      mus[j,] = apply(X,2,stats::weighted.mean,w=T[,j])
      sigmas[[j]] = stats::cov.wt(X,wt=T[,j])$cov
    }

    new_Q = Q_theta_theta_t(ws,mus,sigmas,ws,mus,sigmas,X)
  }

  return(list(ws=ws,mus=mus,sigmas=sigmas))
}
