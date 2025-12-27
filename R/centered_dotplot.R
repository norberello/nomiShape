if (getRversion() >= "2.15.1") utils::globalVariables(
  c("Category", "Freq", "PlotPos")
)

#' Centered Dot Plot for Nominal Variables
#'
#' Creates a centered dot plot for a nominal variable, ordering categories
#' from the most frequent at the center toward less frequent categories
#' on both sides. Optionally connects points with a line and shades
#' the area under the line.
#'
#' @param df A data.frame or tibble containing the variable.
#' @param var Character. Name of the nominal variable in \code{df}.
#' @param connect Logical; if TRUE, connects points with a line.
#' @param shade Logical; if TRUE, shades the area under the line (requires connect = TRUE).
#' @param scale Character; either \code{"count"} (default) or \code{"percent"}.
#'
#' @return A \code{ggplot2} object.
#'
#' @examples
#' centered_dotplot(categories, "animal")
#' centered_dotplot(categories, "animal", connect = TRUE)
#' centered_dotplot(categories, "animal", connect = TRUE, shade = TRUE)
#' centered_dotplot(mpg, "manufacturer", scale = "percent")
#'
#' @export
#' @importFrom ggplot2 ggplot aes geom_point geom_line geom_ribbon labs theme_bw theme element_text
centered_dotplot <- function(df,
                             var,
                             connect = FALSE,
                             shade = FALSE,
                             scale = c("count", "percent")) {

  # ---- Input checks ----
  if (!is.data.frame(df)) stop("`df` must be a data.frame or tibble.")
  if (!is.character(var) || length(var) != 1) stop("`var` must be a single character string.")
  if (!var %in% names(df)) stop("`var` must be a column name in `df`.")
  if (is.list(df[[var]])) stop("`var` must refer to an atomic (non-list) column.")

  scale <- match.arg(scale)

  # ---- Frequency table ----
  freq_table <- as.data.frame(table(df[[var]]), stringsAsFactors = FALSE)
  names(freq_table) <- c("Category", "Freq")
  if (nrow(freq_table) < 2) stop("The variable must have at least two categories.")

  # ---- Optional scaling ----
  if (scale == "percent") freq_table$Freq <- freq_table$Freq / sum(freq_table$Freq) * 100

  # ---- Sort by frequency (descending) ----
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
  freq_table$Category <- factor(freq_table$Category, levels = freq_table$Category)

  # ---- Plot ----
  p <- ggplot2::ggplot(freq_table, ggplot2::aes(x = Category, y = Freq, group = 1)) +
    ggplot2::geom_point(size = 3) +
    ggplot2::labs(x = var, y = ifelse(scale == "percent", "Percentage", "Frequency")) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  if (connect) {
    p <- p + ggplot2::geom_line()
    if (shade) {
      ymin_val <- min(freq_table$Freq)  # <- key change
      p <- p + ggplot2::geom_ribbon(aes(ymin = ymin_val, ymax = Freq), fill = "grey80", alpha = 0.5)
    }
  }

  return(p)
}


