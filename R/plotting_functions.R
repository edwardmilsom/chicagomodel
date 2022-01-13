
#' Visualise Kernel Density Estimate of Crime on a Map
#'
#' @param data The crime dataframe, most likely fetched using the `load_crime_data` function.
#'
#' @return A `ggplot` object.
#' @export
map_crime_kde = function(data){

  density = ggmap::ggmap(map) + ggplot2::stat_density2d( data = data,
                                     ggplot2::aes(x = Longitude, y = Latitude, alpha = ..level..),
                                     bins = 10, geom = "polygon") + ggplot2::labs(title = "Kernel Density Estimate") + ggplot2::xlab("Longitude") + ggplot2::ylab("Latitude") + ggplot2::labs(alpha = "Level")

  return(density)
}


#' Visualise Crime Datapoints on a Map
#'
#' @param data The crime dataframe, most likely fetched using the `load_crime_data` function.
#'
#' @return A `ggplot` object.
#' @export
map_crime_points = function(data){

  incidence = nrow(data)

  points = ggmap::ggmap(map) + ggplot2::geom_point(data = data,
                                                   ggplot2::aes(x = Longitude, y = Latitude),
                                                   colour = "red",
                                                   alpha = 0.5,
                                                   size = 1) + ggplot2::labs(title = paste0("Incidence: ", incidence)) + ggplot2::xlab("Longitude") + ggplot2::ylab("Latitude")

  return(points)
}


#' Visualise Fitted Gaussian Mixture Poisson Process Model
#'
#' @param data The crime dataframe, most likely fetched using the `load_crime_data` function.
#' @param gmm_pp The list outputted by `fit_gmm_pp`, containing the necessary information and parameters for the model.
#'
#' @return A `ggplot` object.
#' @export
map_gmm_pp = function(data, gmm_pp){

  incidence = nrow(data)

  points = ggmap::ggmap(map) + ggplot2::geom_point(data = data,
                                                   ggplot2::aes(x = Longitude, y = Latitude),
                                                   colour = "red",
                                                   alpha = 0.5,
                                                   size = 1) + ggplot2::labs(title = paste0("Incidence: ", incidence)) + ggplot2::xlab("Longitude") + ggplot2::ylab("Latitude")

  t_delta = as.double(difftime(max(data$Date),min(data$Date), units="days"))

  bb = attr(map,"bb")
  x_grid = seq(bb[1,2],bb[1,4],length=100)
  y_grid = seq(bb[1,1],bb[1,3],length=100)
  z = outer(as.vector(scale(x_grid,center=gmm_pp$scaling_params$centre[1],scale=gmm_pp$scaling_params$scale[1])),scale(y_grid,center=gmm_pp$scaling_params$centre[2],scale=gmm_pp$scaling_params$scale[2]),FUN=purrr::partial(gmm_pp$rate_func),t_delta=t_delta)
  d = expand.grid(x_grid,y_grid)
  names(d) = c("x","y")
  d$z = matrix(z,ncol=1)
  fitted = points + ggplot2::geom_contour(d, mapping=ggplot2::aes(x=x,y=y,z=z))

  return(fitted)
}
