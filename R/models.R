
#' Title
#'
#' @param data an object containing 2 variables, a dichotomous explanatory variable, and an independent variable
#' @param formula the formula of the
#' @param family the family
#' @param prior specify the priors to be used for the variables in the model
#' @param chains the number of chains the model should be
#' @param iter description of parameter
#' @param cores the number of cores used to process the brms model construction
#' @param graphs logical. Should diagnostic plots be printed?
#'
#' @returns descriptive statistics and plots of the bayesia model specified in
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
