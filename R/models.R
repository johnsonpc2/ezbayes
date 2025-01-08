
#' Title
#'
#' @param data an object containing 2 variables, a dichotomous explanatory variable, and an independent variable. The data must include a column 'y' with binary outcomes (0/1) and a column 's' for grouping/hierarchical structure identifiers.
#' @param formula specifies the structure of the model by defining the relationship between a response variable and predictor variables. This function is customizable with various options and parameters which may be fine-tuned to satisfy different types of models.
#' @param family specifies the distribution family and link function for the response variable to determine the type of model being fit, e.g., 'bernoulli' for binary outcomes, 'gaussian' for continuous.
#' @param prior allows for the specification of prior distributions for model parameters. Priors represent existing beliefs about parameter values before seeing the data. Can be informative,weakly informative, or non-informative priors. Use NULL for default weakly informative priors.
#' @param chains specifies the number of Markov chains the model should run in parallel. Each chain represents an independent sampling sequence that starts at different initial values to explore the posterior distribution.
#' @param iter Specifies the number of iterations per chain during the MCMC sampling process. By default, half of the iterations are used for warm-up, i.e., burn-in. For example, iter=2000 yields 1000 warmup iterations which are discarded as well as 1000 iterations used for inferential purposes.
#' @param cores the number of CPU cores to use for parallel processing when fitting the model. This constrains the number of chains which can be run simultaneously.
#' @param graphs logical. Should diagnostic plots be printed?
#'
#' @returns descriptive statistics and plots of the Bayesian model specified in
#' the function call
#' @export
#' @importFrom brms brm variables
#' @importFrom tidybayes gather_draws mode_hdi stat_halfeye compare_levels
#' @importFrom dplyr group_by
#' @importFrom ggplot2 ggplot geom_vline
#' @importFrom package function
#'
#' @examples


dichot_model <- function(data, formula, family, prior, chains,
                         iter, cores, graphs = TRUE) {

# N.B.: We are assuming that the data contain a column called "y" which consists
# only of 0's and 1's representing different binary outcomes for each observation.
# There should also be a column called "s" that identifies the source of each
# trial (which may be, e.g., the ID of the participant that produced that trial).

  model <- brm(
    formula = formula,
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
  if (graphs) {
    print(model)
    plot(model)
    pairs(model)
  }

  #### Examine the samples from the posterior distribution ####
  var_names <- variables(model) # The "variables" function returns a vector of the name of every parameter in the model, which can be useful as BRMS does not always assign things the best names!

  # Numerical summaries of the parameters for each subject
  numerical_summary <- model %>%
    gather_draws(`b_am(.*)`, regex = TRUE) %>%
    group_by(.variable) %>%
    mode_hdi(.value)

  if (graphs) {
    # Visual summaries of the parameters for each subject
    param_summary_plot <- model %>%
      gather_draws(`b_am(.*)`, regex = TRUE) %>%
      ggplot(aes(x = .value, y = .variable)) +
      stat_halfeye(point_interval = "mode_hdi")
    print(param_summary_plot)

    # Visual comparison of the parameters for each subject
    param_comparison_plot <- model %>%
      gather_draws(`b_am(.*)`, regex = TRUE) %>%
      compare_levels(.value, by = .variable) %>%
      ggplot(aes(x = .value, y = .variable)) +
      stat_halfeye(point_interval = "mode_hdi") +
      geom_vline(xintercept = 0, linetype = "dashed")
    print(param_comparison_plot)
  }

  return(model)
}

# Incorporate ROPE from bayestestR
# Zero-inflated models (poisson, negative exponential)
# Hurdle models
# A function that will do a shrinkage comparison between an independent and hierarchical model
