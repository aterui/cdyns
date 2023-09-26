
test_that("test equilibrium density; scaled", {

  k <- 100
  re1 <- cdynsim(n_species = 1,
                 sd_env = 0,
                 k = k)

  expect_equal(re1$df_community$mean_density, k)

  n_species <- round(runif(1, 1, 10))
  re2 <- cdynsim(n_species = n_species,
                 sd_env = 0,
                 alpha = 0,
                 k = k)

  expect_equal(re2$df_community$mean_density, n_species * k)

})

test_that("test equilibrium density; unscaled", {

  r <- runif(1, 0.5, 1.5)
  alpha <- runif(1, 0, 1)
  re1 <- cdynsim(n_species = 1,
                 sd_env = 0,
                 alpha = alpha,
                 r = r,
                 alpha_scale = "unscaled")

  expect_equal(round(re1$df_community$mean_density),
               round(r / alpha))

})

test_that("test competition", {

  n_species <- round(runif(1, 1, 10))
  alpha <- runif(1, 0.1, 1)
  k <- 100

  re <- cdynsim(n_species = n_species,
                sd_env = 0,
                alpha = alpha,
                k = k)

  y <- round(re$df_species$mean_density[1], 1)
  n_hat <- round(k - alpha * sum(re$df_species$mean_density[-1]), 1)

  expect_equal(y, n_hat)

})
