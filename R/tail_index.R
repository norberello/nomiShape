#' Tail Index for Nominal Variables
#'
#' Computes the proportion of categories contributing to the lower part of the distribution.
#' Useful to quantify long-tail structure in nominal distributions.
#'
#' @param df A data.frame or tibble containing the variable.
#' @param var Character. Name of the nominal variable in `df`.
#' @param threshold Numeric. Cumulative proportion of counts defining the "dominant" categories (default 0.8).
#' @return Numeric between 0 and 1 representing the tail proportion.
#' @examples
#' tail_index(categories3, "animal")
#' tail_index(categories2, "animal", threshold = 0.9)
#' @export
tail_index <- function(df, var, threshold = 0.8) {
  if (!is.data.frame(df)) stop("`df` must be a data.frame or tibble.")
  if (!var %in% names(df)) stop("`var` must be a column name in `df`.")
  if (length(var) != 1) stop("`var` must be a single character string.")

  freq_table <- as.data.frame(table(df[[var]]), stringsAsFactors = FALSE)
  colnames(freq_table) <- c("Category", "Freq")
  freq_table <- freq_table[order(-freq_table$Freq), ]
  freq_table$cumperc <- cumsum(freq_table$Freq) / sum(freq_table$Freq)

  # Number of categories needed to reach threshold
  dominant_count <- which(freq_table$cumperc >= threshold)[1]

  # Tail proportion
  tail_prop <- (nrow(freq_table) - dominant_count) / nrow(freq_table)
  return(tail_prop)
}
