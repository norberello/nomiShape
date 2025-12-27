if (getRversion() >= "2.15.1") utils::globalVariables(c("Category", "Freq"))

#' Ranked Bar Plot for Nominal Variables
#'
#' Creates a bar plot for a nominal variable, with categories ordered
#' from most frequent to least frequent.
#'
#' @param df A data.frame or tibble containing the variable.
#' @param var Character string giving the name of the variable in \code{df}.
#' @param scale Character; either \code{"count"} (default) or \code{"percent"}.
#' @param title Optional character string specifying the plot title.
#'
#' @return A \code{ggplot2} object representing the ranked bar plot.
#'
#' @examples
#' ranked_barplot(categories, "animal")
#' ranked_barplot(categories, "animal", scale = "percent")
#'
#' @export
#' @importFrom ggplot2 ggplot aes geom_col labs theme_bw theme element_text
ranked_barplot <- function(df,
                           var,
                           scale = c("count", "percent"),
                           title = NULL) {

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

  scale <- match.arg(scale)

  # ---- Frequency table ----
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
  freq_table$Category <- factor(freq_table$Category,
                                levels = freq_table$Category)

  # ---- Plot ----
  ggplot2::ggplot(freq_table,
                  ggplot2::aes(x = Category, y = Freq)) +
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
