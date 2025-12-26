# ---- Declare global variables for R CMD check ----
if (getRversion() >= "2.15.1") utils::globalVariables(c("Category", "Freq"))
#' Centered Frequency Bar Plot for Nominal Variables
#' Creates a centered bar plot for discrete nominal variables by placing the
#' most frequent category at the center and progressively less frequent
#' categories alternately to the left and right.
#'
#' @param df A data frame containing the nominal variable.
#' @param var A character string giving the name of the nominal variable in `df`.
#' @param title Optional character string specifying the plot title.
#' @param scale Character string specifying the scale of the frequencies:
#'   `"count"` (default) for raw counts or `"percent"` for percentages.
#'
#' @return A \code{ggplot2} object.
#'
#' @examples
#' centered_barplot(categories, "animal")
#' centered_barplot(categories, "animal", scale = "percent")
#'
#' @export
centered_barplot <- function(df,
                             var,
                             title = NULL,
                             scale = c("count", "percent")) {

  # ---- Input checks (CRAN + testthat friendly) ----
  if (!is.data.frame(df)) {
    stop("`df` must be a data.frame.")
  }

  if (!is.character(var) || length(var) != 1) {
    stop("`var` must be a single character string.")
  }

  if (!var %in% names(df)) {
    stop("`var` must be a column name in `df`.")
  }

  scale <- match.arg(scale)

  # ---- Frequency table (ROBUST version) ----
  freq_table <- as.data.frame(
    table(df[[var]]),
    stringsAsFactors = FALSE
  )
  names(freq_table) <- c("Category", "Freq")

  if (nrow(freq_table) < 2) {
    stop("The variable must have at least two levels.")
  }

  # ---- Optional scaling ----
  if (scale == "percent") {
    freq_table$Freq <- freq_table$Freq / sum(freq_table$Freq) * 100
  }

  # ---- Sort by frequency ----
  freq_table <- freq_table[order(-freq_table$Freq), ]

  # ---- Centered positioning algorithm ----
  f <- nrow(freq_table)
  center <- ceiling(f / 2)

  positions <- integer(f)
  positions[1] <- center

  right <- center
  left <- center
  side <- 1

  for (i in 2:f) {
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
  freq_table <- freq_table[order(freq_table$PlotPos), ]
  freq_table$Category <- factor(freq_table$Category,
                                levels = freq_table$Category)

  # ---- Plot ----
  ggplot2::ggplot(freq_table, ggplot2::aes(x = Category, y = Freq)) +
    ggplot2::geom_col() +
    ggplot2::labs(
      title = title,
      x = var,
      y = ifelse(scale == "percent", "Percentage", "Frequency")
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )
}
