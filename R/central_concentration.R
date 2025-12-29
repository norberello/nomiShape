#' Central Concentration Index for Nominal Variables
#'
#' Computes a measure of how concentrated counts are around the center
#' of a nominal variable, based on the centered plotting order.
#'
#' @param df A data.frame or tibble containing the variable.
#' @param var Character. Name of the nominal variable in \code{df}.
#' @param top_k Numeric. Number of central categories to consider (default: 3).
#' @param weighted Logical. If TRUE, applies a weight decreasing with distance from center.
#'
#' @return A numeric value between 0 and 1 representing the central concentration.
#'
#' @examples
#' central_concentration(categories, "animal")
#' central_concentration(categories2, "animal", top_k = 5)
#' central_concentration(categories3, "animal", weighted = TRUE)
#'
#' @export
central_concentration <- function(df, var, top_k = 3, weighted = FALSE) {

  if (!is.data.frame(df)) stop("`df` must be a data.frame or tibble.")
  if (!is.character(var) || length(var) != 1) stop("`var` must be a single character string.")
  if (!var %in% names(df)) stop("`var` must be a column name in `df`.")
  if (is.list(df[[var]])) stop("`var` must refer to an atomic (non-list) column.")

  # Frequency table
  freq_table <- as.data.frame(table(df[[var]]), stringsAsFactors = FALSE)
  names(freq_table) <- c("Category", "Freq")

  # Sort descending
  freq_table <- freq_table[order(-freq_table$Freq), ]

  n <- nrow(freq_table)
  center <- ceiling(n/2)

  # Compute centered positions
  positions <- integer(n)
  positions[1] <- center
  left <- center
  right <- center
  side <- 1
  for (i in 2:n) {
    if (side == 1) {
      right <- right + 1
      positions[i] <- right
    } else {
      left <- left - 1
      positions[i] <- left
    }
    side <- -side
  }
  freq_table$PlotPos <- positions

  # Identify central positions
  central_indices <- order(abs(freq_table$PlotPos - center))[1:top_k]

  if (!weighted) {
    # Simple sum of counts in top_k central categories
    cci <- sum(freq_table$Freq[central_indices]) / sum(freq_table$Freq)
  } else {
    # Weighted: weight = 1 / (1 + distance from center)
    weights <- 1 / (1 + abs(freq_table$PlotPos - center))
    cci <- sum(freq_table$Freq * weights) / sum(freq_table$Freq * weights)
  }

  return(cci)
}
