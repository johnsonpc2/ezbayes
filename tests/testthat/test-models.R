test_that("dichot model construction", {

  #### Read in the raw data ####
  data("mtcars")
  mtcars |> dplyr::mutate(am = factor(am)) |> dplyr::select(mpg, am) -> mtcars

  dichot_model(data = mtcars,
               f = mpg ~ 0 + am,
               family = NULL,
               prior = brms::prior(beta(4, 1), class = b, lb = 0, ub = 1),
               chains = 5,
               iter = 2000,
               graphs = FALSE
  )

  expect_s3_class(mtcars, "data.frame")
})
