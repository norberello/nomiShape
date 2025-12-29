#' Dominance Index for Nominal Variables
#'
#' Computes dominance for a nominal variable using the Simpson index,
#' quantifying the degree to which a few categories dominate the distribution.
#'
#' @param df A data.frame or tibble containing the nominal variable.
#' @param var Character. Name of the nominal variable in \code{df}.
#'
#' @return A numeric value representing dominance.
#'
#' @details
#' Dominance is calculated as:
#' \deqn{D = \sum p_i^2}
#' where \eqn{p_i} is the relative frequency of category \eqn{i}.
#'
#' Higher values indicate stronger dominance by fewer categories.
#'
#' @examples
#' dominance_index(categories, "animal")
#' dominance_index(categories2, "animal")
#' dominance_index(categories3, "animal")
#'
#' @export
dominance_index <- function(df, var) {

  # ---- Input checks ----
  if (!is.data.frame(df)) {
    stop("`df` must be a data.frame or tibble.")
  }

  if (!is.character(var) || length(var) != 1) {
    stop("`var` must be a single character string.")
  }

  if (!var %in% names(df)) {
    stop("`var` must be a column name in `df`.")
  }

  if (is.list(df[[var]])) {
    stop("`var` must refer to an atomic (non-list) column.")
  }

  # ---- Frequency table ----
  freq <- table(df[[var]])
  p <- freq / sum(freq)

  # ---- Simpson dominance ----
  dominance <- sum(p^2)

  return(as.numeric(dominance))
}
