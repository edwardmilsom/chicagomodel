undo_scaling = function(X,centre,scale){
  return(sweep(sweep(X,MARGIN=2,scale,"*"),MARGIN=2,centre,"+"))
}


#' Generate new locations of crimes based on the fitted model
#'
#' @param gmm_pp A fitted poisson process returned from the fit_gmm_pp function.
#' @param t_delta The length of time in days that new points should be generated for. Crimes can be generated starting immediately after the last crime that was trained on.
#' @param area_bounds A list of two vertices that specify the box in which points are generated. The vertices are given in the form `c(Longitude, Latitude)`
#' The default is the area shown in the mapping functions, i.e. `list(c(-87.84918,41.6608), c(-87.40973,41.98827))`.
#'
#' @return A dataframe containing Longitude, Latitude, and Date columns for each generated crime. This is enough to be passed to the `map_crime_points` and `map_crime_kde` functions.
#' @export
generate_new_points = function(gmm_pp,t_delta=1,area_bounds=list(c(-87.84918,41.6608), c(-87.40973,41.98827))){
  scaled_lower = as.vector(scale(matrix(area_bounds[[1]],nrow=1),center=gmm_pp$scaling_params$centre,scale=gmm_pp$scaling_params$scale))
  scaled_upper = as.vector(scale(matrix(area_bounds[[2]],nrow=1),center=gmm_pp$scaling_params$centre,scale=gmm_pp$scaling_params$scale))
  points = spatstat.core::rpoispp(gmm_pp$rate_func,win=owin(c(scaled_lower[1],scaled_upper[1]),c(scaled_lower[2],scaled_upper[2])), t_delta=t_delta)
  if(points$n == 0){
    D = data.frame(matrix(ncol=3,nrow=0))
    colnames(D) = c("Longitude","Latitude","Date")
    return(D)
  }
  else{
    X = cbind(points$x,points$y)
    unscaled_X = undo_scaling(X,gmm_pp$scaling_params$centre,gmm_pp$scaling_params$scale)
    one_day = as_datetime("2019-01-02") - as_datetime("2019-01-01")
    times = runif(points$n)*one_day*t_delta + gmm_pp$date_range$last
    D = data.frame("Longitude"=unscaled_X[,1],"Latitude"=unscaled_X[,2],"Date"=times)
    return(D)
  }
}
