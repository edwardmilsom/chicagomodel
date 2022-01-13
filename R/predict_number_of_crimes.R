
cubature_rate_wrapper = function(x,gmm_pp,t_delta){
  return(gmm_pp$rate_func(x[1],x[2],t_delta))
}

#' Predict the number of crimes over a certain rectangular area and time period
#'
#' @param gmm_pp A fitted model given by the `fit_gmm_pp` function
#' @param lower The lower left vertex of the region as a vector, i.e. `c(x1,y1)`
#' @param upper The upper right vertex of the region as a vector, i.e. `c(x2, y2)`
#' @param time_delta How long in days the period to predict should be. For example, if you trained on February and wanted to predict the number of crimes in March,
#'
#' @return The predicted number of crimes.
#' @export
#'
#' @examples
predict_number_of_crimes = function(gmm_pp, lower, upper, t_delta){
    lower = as.vector(scale(matrix(lower,nrow=1), center=gmm_pp$scaling_params$centre, scale=gmm_pp$scaling_params$scale))
    upper = as.vector(scale(matrix(upper,nrow=1), center=gmm_pp$scaling_params$centre, scale=gmm_pp$scaling_params$scale))
    prediction = cubature::cubintegrate(cubature_rate_wrapper, lower, upper, gmm_pp=gmm_pp, t_delta=t_delta)$integral
    return(prediction)
}
