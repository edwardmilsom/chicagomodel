test_that("Prediction matches number of points on training set", {
  set.seed(42)
  X = rbind(c(0,0),c(-1,0),c(1,0),c(0,1),c(0,-1),c(5,5),c(4,5),c(5,6),c(6,5),c(5,4))
  D = data.frame(X)
  names(D) = c("Longitude","Latitude")
  D$Date = lubridate::mdy_hms(c("01/13/2019 08:30:00 PM", "01/17/2019 18:40:00 PM", "01/02/2019 05:35:00 PM", "01/20/2019 13:32:00 PM", "01/27/2019 22:35:00 PM", "01/30/2019 15:25:00 PM", "01/07/2019 02:35:00 PM", "01/03/2019 22:15:00 PM", "01/05/2019 05:35:00 PM", "01/09/2019 14:35:00 PM"))
  K = 2
  pp=fit_gmm_pp(D,K)
  min_x_y = c(min(D$Longitude),min(D$Latitude))
  max_x_y = c(max(D$Longitude),max(D$Latitude))
  t_delta = as.double(difftime(max(D$Date),min(D$Date)))
  expect_equal(predict_number_of_crimes(pp,min_x_y,max_x_y,t_delta), dim(D)[1], tolerance=1e-01)
})
