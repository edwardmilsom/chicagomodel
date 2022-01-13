
#' Title
#'
#' @param D Dataframe containing crimes to be used to train the model. Should be in format provided by the fix_date_column( function).
#' @param K Number of components the Gaussian mixture should use.
#'
#' @return A list containing the fitted rate function of the form `f(x,y,t_delta)`, the list of parameters that are used in the rate function, and the list parameters used to scale the data before fitting, so that they may be used on new data.
#' @export
#'
#'
fit_gmm_pp = function(D,K){

  X = cbind(D$Longitude,D$Latitude)
  scaled_X = scale(X)
  X_centre = attr(scaled_X, "scaled:center")
  X_scale = attr(scaled_X, "scaled:scale")

  t_delta = as.double(difftime(max(D$Date),min(D$Date)))

  init_centres = stats::kmeans(scaled_X,K)$centers
  dimnames(init_centres) = NULL

  param_list = gmm_em_fit(scaled_X,K,init_centres)

  unfitted_rate_func_cubature = function(x,mu,sigma,w,C,t_delta){
    return(C*t_delta*mvnfast::dmixn(cbind(x[1],x[2]),mu=mu,sigma=sigma,w=w))
  }

  min_x_y = c(min(scaled_X[,1]), min(scaled_X[,2]))
  max_x_y = c(max(scaled_X[,1]), max(scaled_X[,2]))
  C_hat = dim(scaled_X)[1] / cubature::cubintegrate(unfitted_rate_func_cubature, min_x_y, max_x_y, mu=param_list$mus, sigma=param_list$sigmas, w=param_list$ws, C=1, t_delta=t_delta)$integral

  fitted_rate_func = function(x,y,t_delta){
    return(C_hat*t_delta*mvnfast::dmixn(cbind(x,y), mu=param_list$mus, sigma=param_list$sigmas, w=param_list$ws))
  }

  full_param_list = list("C"=C_hat, "mus"=param_list$mus, "sigmas"=param_list$sigmas, "ws"=param_list$ws)
  scaling_params = list("centre"=X_centre, "scale"=X_scale)

  return(list("rate_func"=fitted_rate_func, "rate_params"=full_param_list, "scaling_params"=scaling_params, "date_range"=list("first"=min(D$Date),"last"=max(D$Date))))
}
