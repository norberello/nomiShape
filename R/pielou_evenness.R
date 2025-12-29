#' Pielou's Evenness for Nominal Variables
#'
#' Computes Pielou's evenness index based on Shannon entropy for a
#' nominal variable recorded as individual-level observations.
#'
#' @param df A data.frame or tibble containing the nominal variable.
#' @param var Character string giving the name of the nominal variable in `df`.
#'
#' @details
#' Pielou's evenness is defined as:
#' \deqn{E = H / \log(S)}
#' where \eqn{H} is Shannon entropy and \eqn{S} is the number of observed categories.
#'
#' Values range from 0 (complete dominance by one category)
#' to 1 (perfectly even distribution).
#'
#' @return A numeric value representing Pielou's evenness.
#'
#' @examples
#' pielou_evenness(categories, "animal")
#' pielou_evenness(categories2, "animal")
#' pielou_evenness(categories3, "animal")
#'
#' @export
pielou_evenness <- function(df, var) {

  # Input checks
  if (!is.data.frame(df)) {
    stop("df must be a data.frame or tibble")
  }
  if (!var %in% names(df)) {
    stop("Variable not found in df")
  }

  # Extract variable
  x <- df[[var]]
  x <- x[!is.na(x)]

  # Frequencies
  freq <- table(x)
  p <- freq / sum(freq)

  # Shannon entropy
  H <- -sum(p * log(p))

  # Species / category richness
  S <- length(freq)

  # Pielou's evenness
  E <- H / log(S)

  return(as.numeric(E))
}
