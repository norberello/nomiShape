if (getRversion() >= "2.15.1") utils::globalVariables(
  c("Category", "Freq", "cumulative", "cumulative_percentage"))
#' Pareto Plot for Nominal Variables
#'
#' Creates a Pareto chart for a nominal variable, displaying frequencies
#' and cumulative percentages.
#' @importFrom stats reorder
#' @importFrom dplyr desc
#'
#' @name pareto
#' @param df A data.frame or tibble containing the variable.
#' @param var Character. Name of the variable in `df`.
#' @param show_table Logical; if TRUE, prints the frequency table. Default is FALSE.
#' @return A \code{ggplot2} object representing the Pareto chart.
#'
#' @examples
#' pareto(categories, "animal")
#' @export

pareto <- function(df, var, show_table = TRUE) {
  if (!is.data.frame(df)) {
    stop("`df` must be a data.frame or tibble.")
  }

  if (!var %in% names(df)) {
    stop("`var` must be a column name in `df`.")
  }

  if (is.list(df[[var]])) {
    stop("`var` must refer to an atomic (non-list) column.")
  }

  freq_table <- as.data.frame(table(df[[var]]))
  colnames(freq_table) <- c("Category", "Freq")

  freq_table <- freq_table |>
    dplyr::arrange(desc(Freq)) |>
    dplyr::mutate(
      cumulative = cumsum(Freq),
      cumulative_percentage = cumulative / sum(Freq) * 100
    )

  if (show_table) {
    print(freq_table)
  }

  max_freq <- max(freq_table$Freq)

  ggplot2::ggplot(freq_table,
                  ggplot2::aes(x = reorder(Category, -Freq), y = Freq)) +
    ggplot2::geom_col() +
    ggplot2::geom_line(
      ggplot2::aes(y = cumulative_percentage * max_freq / 100, group = 1),
      color = "red", linewidth = 1
    ) +
    ggplot2::geom_point(
      ggplot2::aes(y = cumulative_percentage * max_freq / 100),
      color = "red", size = 2
    ) +
    ggplot2::scale_y_continuous(
      sec.axis = ggplot2::sec_axis(~ . / max_freq * 100,
                                   name = "Cumulative frequency (%)")
    ) +
    ggplot2::labs(x = var, y = "Frequency") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x =
                     ggplot2::element_text(angle = 45, hjust = 1))
}
