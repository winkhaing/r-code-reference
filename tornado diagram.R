library(dplyr)
library(ggplot2)

#' Produce a tornado diagram with the results of multiple one-way sensitivity
#' analyses
#'
#' The result is a standard ggplot2 object, so you can add `scale_` and
#' `theme_` calls to the end to customise the look and feel. Note that this
#' includes flipped coordinates, so use `scale_x_discrete` for the parameters
#' and `scale_y_continuous` for the value of the outcome.
#' 
#' IMPORTANT! This function assumes that you have loaded the packages `dplyr`,
#' `forcats` and `ggplot2`
#' 
#' @param res       A `data.frame` with columns `.param` giving the names of
#'                  different parameters, `.dir` containing "lower" or "upper"
#'                  depending on which sensitivity analysis limit is given,
#'                  `.value` containing the value of `.param`, plus at least
#'                  one outcome column. For each parameter there should be two
#'                  rows, one for each limit.
#' @param outcome   The bare name of the column in `res` with the outcome of
#'                  interest, e.g., the incremental net monetary benefit,
#'                  incremental cost, incremental effects, or incremental
#'                  cost-effectiveness ratio
#' @param base_case The base case value of `outcome`
#' @param show_values How should the parameter values (`.value`) be included in
#'                  the plot? If "none" (the default) then they will not be
#'                  shown. If "axis" they will be included in the axis labels.
#'                  If "attached" they will be shown on the plot attached to
#'                  the bars.
#' @param outcome_labeller A suitable labeller for the outcome scale, e.g.,
#'                  `scales::dollar_format()`
tornado_diagram <- function(res, outcome, base_case, show_values = c("none", "axis", "attached"), outcome_labeller = scales::label_number()) {
  show_values <- match.arg(show_values)
  
  data_prep <- res |>
    group_by(.param) |>
    mutate(
      .OUTCOME_DIFF  = {{outcome}} - base_case,
      .OUTCOME_RANGE = max(c(base_case, {{outcome}})) - min(c(base_case, {{outcome}})),
      .POSITION      = if_else(prod(sign(.OUTCOME_DIFF)) == 1, "dodge", "stack"),
      .VALUE_FMT     = formatC(signif(.value, digits = 3), format = "f", drop0trailing = TRUE),
      .LABELS        = if (show_values == "axis") paste0(.param, " [", .VALUE_FMT[.dir == "lower"], ", ", .VALUE_FMT[.dir == "upper"], "]") else .param
    )
  
  shift_trans <- scales::trans_new(
    name      = "shift",
    transform = function(x) x - base_case,
    inverse   = function(x) x + base_case
  )
  
  p <- ggplot(
    data_prep,
    aes(
      x    = fct_reorder(.LABELS, .OUTCOME_RANGE), # Show in order of importance
      y    = {{outcome}},
      fill = .dir)
  ) +
    geom_blank() + # Necessary to preserve the order of parameters
    geom_col(
      data     = ~ filter(., .POSITION == "dodge"),
      position = "dodge"
    ) +
    geom_col(
      data     = ~ filter(., .POSITION == "stack"),
      position = "stack"
    ) +
    scale_y_continuous(trans = shift_trans, labels = outcome_labeller) +
    labs(x = "Parameter", y = "Outcome") +
    coord_flip()
  
  if (show_values == "attached")
    p +
    geom_text(
      aes(label = .VALUE_FMT)
    )
  else
    p
  
}

## EXAMPLE USAGE

inmb_base_case <- 7802.867

res <- data.frame(
  .param = rep(
    c(
      "p_Healthy_Diseased",
      "p_Healthy_Dead",
      "p_Diseased_Dead",
      "c_Healthy",
      "c_Diseased",
      "q_Healthy", 
      "q_Diseased",
      "hr_Healthy_Diseased"
    ),
    each = 2
  ),
  .dir = rep(c("lower", "upper"), times = 8),
  .value = c(
    0.0062933472741721,
    0.0710761224618776,
    0.000255702736665962,
    0.0365757449834789,
    0.0165999113298435,
    0.100227906568749,
    32.3573636956587,
    71.4201951875064,
    647.147273913173,
    1428.40390375013,
    0.83443102425799,
    0.950489127791337,
    0.502782486145993,
    0.693427470508174,
    0.673009119806321,
    0.996004402181866
  ),
  INMB = c(
    -8748.30416264548,
    14286.5601803342,
    12625.0021675139,
    159.783725954361,
    2001.57210692969,
    12057.56549217,
    7832.13443956539, 
    7767.33315927429,
    7452.54521267464,
    8228.19825850403,
    5627.42132379011,
    9477.99406001753,
    9733.27146605282,
    5947.71990347642,
    24588.5221102674,
    -9676.92058187226
  )
)

tornado_diagram(
  res,
  INMB,
  inmb_base_case,
  show_values = "axis",
  outcome_labeller = scales::dollar_format(prefix = "£", scale = 1e-3)
) +
  labs(y = "Incremental net monetary benefit (per patient)") +
  scale_fill_discrete("Direction", labels = c("Lower", "Upper")) +
  theme_minimal() +
  theme(legend.position = "top")
