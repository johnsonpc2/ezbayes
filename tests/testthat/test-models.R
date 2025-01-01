test_that("multiplication works", {

  #### Read in the raw data ####
  data("mtcars")
  mtcars %>% mutate(am = factor(am)) %>% select(mpg, am) -> mtcars

  dichot_model(data = mtcars,
               f = mpg ~ 0 + am,
               family = NULL,
               prior = prior(beta(4, 1), class = b, lb = 0, ub = 1),
               chains = 5,
               iter = 2000,
               cores = 5,
               graphs = FALSE
  )

  expect_s3_class(mtcars, "data.frame")
})
