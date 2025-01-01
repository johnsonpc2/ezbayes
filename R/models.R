
#' Title
#'
#' @param data an object containing 2 variables, a dichotomous explanatory variable, and an independent variable
#' @param f the formula of the
#' @param family the family
#' @param prior
#' @param chains the number of
#' @param iter
#' @param cores the number of cores used to process the brms model construction
#' @param graphs logical. Should diagnostic plots be printed?
#'
#' @returns
#' @export
#'
#' @examples


dichot_model <- function(data, f, family, prior, chains,
                         iter, cores, graphs = TRUE) {

# N.B.: We are assuming that the data contain a column called "y" which consists
# only of 0's and 1's representing different binary outcomes for each observation.
# There should also be a column called "s" that identifies the source of each
# trial (which may be, e.g., the ID of the participant that produced that trial).

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

# have the function report the BF and the ROPE
