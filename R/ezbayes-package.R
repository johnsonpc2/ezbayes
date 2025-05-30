#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom brms brm
#' @importFrom brms prior
#' @importFrom brms variables
#' @importFrom dplyr group_by
#' @importFrom ggplot2 geom_vline
#' @importFrom ggplot2 ggplot
#' @importFrom graphics pairs
#' @importFrom tidybayes compare_levels
#' @importFrom tidybayes gather_draws
#' @importFrom tidybayes mode_hdi
#' @importFrom tidybayes stat_halfeye
## usethis namespace: end
NULL

utils::globalVariables(c("b_am(.*)", ".variable", ".value"))
