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

  print(model)

  plot(model)

  pairs(model)
}

dichot_model(data = mtcars,
             f = mpg ~ 0 + am,
             family = NULL,
             prior = prior(beta(4, 1), class = b, lb = 0, ub = 1),
             chains = 5,
             iter = 2000,
             cores = 5
             )

#### Specify and run the model ####

model <- brm(
  formula = mpg ~ 0 + am,               # The formula specifies the relationship between an outcome variable and other variables in the data.  In this case, we are saying that the outcome "y" depends on who produced it ("s").  The "zero" means that we will estimate parameters for each "s" independently, without also trying to estimate a group-level average or "intercept".
  data = mtcars,               # We have to tell R the name of the data frame that contains the variables in our model
  prior = prior(beta(4, 1), class = b, lb = 0, ub = 1), # Specify our priors.  The "class = b" part comes from the fact that each value of "s" gets its own coefficient, which brms calls "b".  So this means that each "b" has a "beta(2, 2)" prior.  The "lb" and "ub" specify lower and upper bounds, respectively, on the allowed values.
  chains = 5,                    # How many separate MCMC chains should we run?
  iter = 2000,                   # For how long should we run each chain?  Note that, by default, the first half of the chain is used for adaptation/warm-up and only the second half is treated as representative samples from the posterior.
  cores = 5                      # If you are running multiple chains, it's good to do them in parallel; this specifies how many "cores" you will allow the model to use.
)

#### Inspect MCMC diagnostics to ensure convergence and mixing ####

summary(model)   # Provide R-hat and ESS values for the higher-level parameters in the model

plot(model)      # Shows histograms and traceplots for the samples of the higher-level parameters in the model

pairs(model)     # Shows histograms and scatterplots that allow us to see whether there is any covariance between the higher-level parameters in the model

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

