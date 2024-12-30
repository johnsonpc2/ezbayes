# Example for using BRMS to do inference with dichotomous data from multiple sources

#### Load necessary packages ####
library(tidyverse)
library(brms)
library(tidybayes)

#### Read in the raw data ####
data("mtcars")
mtcars %>% mutate(am = factor(am)) %>% select(mpg, am) -> mtcars

# N.B.: We are assuming that the data contain a column called "y" which consists
# only of 0's and 1's representing different binary outcomes for each observation.
# There should also be a column called "s" that identifies the source of each
# trial (which may be, e.g., the ID of the participant that produced that trial).

dichot_model <- function(data, f, family, prior, chains, iter, cores){

  model <- brm(
    formula = f,
    data = data,
    family = family,
    prior = prior,
    chains = chains,
    iter = iter,
    cores = cores,
    save_pars = save_pars(all = TRUE),
    file = "brms_model_temp",
    file_refit = "on_change"
  )

  #### Inspect MCMC diagnostics to ensure convergence and mixing ####
  print(model)

  plot(model)

  pairs(model)


  #### Examine the samples from the posterior distribution ####

  var_names <- variables(model)    # The "variables" function returns a vector of the name of every parameter in the model, which can be useful as BRMS does not always assign things the best names!

  # Numerical summaries of the parameters for each subject
  model %>%
    gather_draws(`b_am(.*)`, regex = TRUE) %>%
    group_by(.variable) %>%
    mode_hdi(.value)

  # Visual summaries of the parameters for each subject
  model %>%
    gather_draws(`b_am(.*)`, regex = TRUE) %>%
    ggplot(aes(x = .value, y = .variable)) +
    stat_halfeye(point_interval = "mode_hdi")

  # Visual comparison of the parameters for each subject
  model %>%
    gather_draws(`b_am(.*)`, regex = TRUE) %>%
    compare_levels(.value, by = .variable) %>%
    ggplot(aes(x = .value, y = .variable)) +
    stat_halfeye(point_interval = "mode_hdi") +
    geom_vline(xintercept = 0, linetype = "dashed")
}

dichot_model(data = mtcars,
             f = mpg ~ 0 + am,
             family = NULL,
             prior = prior(beta(4, 1), class = b, lb = 0, ub = 1),
             chains = 5,
             iter = 2000,
             cores = 5
             )

# have the function report the BF and the ROPE
# have an option to have plots generated, or not, based on if its true or false

