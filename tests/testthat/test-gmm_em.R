test_that("Gaussian mixture fitting runs as expected on simple example", {
  X = rbind(c(0,0),c(-1,0),c(1,0),c(0,1),c(0,-1),c(5,5),c(4,5),c(5,6),c(6,5),c(5,4))
  K = 2
  start_centres = rbind(c(-1,-1),c(6,6))
  par_list = gmm_em_fit(X,K,start_centres)
  expect_equal(par_list$ws, c(0.5,0.5), tolerance=1e-17)
  expect_equal(par_list$mus, rbind(c(0,0),c(5,5)), tolerance=1e-17)
  expect_equal(par_list$sigmas[[1]], rbind(c(0.5,0),c(0,0.5)), tolerance=1e-15)
})
