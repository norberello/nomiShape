if (getRversion() >= "2.15.1") utils::globalVariables(c("Shape"))
#' Compare Observed Nominal Distribution with Theoretical Shapes
#'
#' Plots a centered dotplot of a nominal variable and overlays four
#' theoretical distributions: uniform, triangular, exponential (Pareto-like),
#' and normal-like.
#'
#' @importFrom stats dnorm
#' @name shape_comp_plot
#' @param df A data.frame or tibble containing the nominal variable.
#' @param var Character string giving the name of the nominal variable in `df`.
#' @param rate_exp Numeric. Rate parameter for the exponential distribution (Pareto-like). Default is 0.7.
#' @param scale Character. Whether to scale frequencies as counts ("count") or percentages ("percent"). Default is "count".
#'
#' @details
#' The function orders categories from most frequent at the center outwards.
#' Observed frequencies are plotted as points and lines, and each theoretical
#' distribution is overlaid with a different color and line type.
#'
#' @return A ggplot2 object.
#'
#' @examples
#' shape_comp_plot(categories, "animal")
#' shape_comp_plot(categories2, "animal")
#' shape_comp_plot(categories3, "animal")
#'
#' @export
shape_comp_plot <- function(df, var, rate_exp = 0.7, scale = c("count", "percent")) {

  scale <- match.arg(scale)

  # ---- Helper: compute centered positions ----
  .center_positions <- function(k) {
    center <- ceiling(k / 2)
    pos <- integer(k)
    pos[1] <- center
    r <- l <- center
    side <- 1
    for (i in 2:k) {
      if (side == 1) { r <- r + 1; pos[i] <- r }
      else { l <- l - 1; pos[i] <- l }
      side <- -side
    }
    pos
  }

  # ---- Helper: convert freq vector to centered df ----
  .centered_df <- function(freqs, categories = NULL) {
    k <- length(freqs)
    if (is.null(categories)) categories <- as.character(seq_len(k))
    df <- data.frame(Category = categories, Freq = freqs, stringsAsFactors = FALSE)
    df$PlotPos <- .center_positions(k)
    df <- df[order(df$PlotPos), ]
    df$Category <- factor(df$Category, levels = df$Category)
    df
  }

  # ---- Observed frequencies ----
  obs_table <- as.data.frame(table(df[[var]]), stringsAsFactors = FALSE)
  names(obs_table) <- c("Category", "Freq")

  if (scale == "percent") obs_table$Freq <- obs_table$Freq / sum(obs_table$Freq) * 100

  # Sort descending to match “most frequent at center” logic
  obs_table <- obs_table[order(-obs_table$Freq), ]
  obs <- .centered_df(obs_table$Freq, obs_table$Category)

  k <- nrow(obs)
  n <- sum(obs$Freq)

  # ---- Theoretical distributions ----
  # Uniform
  theo_uniform <- rep(n / k, k)

  # Triangular (centered)
  mid <- ceiling(k / 2)
  theo_triangular <- c(seq_len(mid), rev(seq_len(k - mid)))[1:k]
  theo_triangular <- theo_triangular / sum(theo_triangular) * n

  # Exponential (largest in center)
  raw_exp <- exp(-rate_exp * seq_len(k))
  raw_exp <- raw_exp / sum(raw_exp) * n
  slot_order <- .center_positions(k)
  theo_exponential <- numeric(k)
  theo_exponential[slot_order] <- sort(raw_exp, decreasing = TRUE)

  # Normal-like (centered)
  d <- abs(seq_len(k) - ceiling(k / 2))
  theo_normal <- dnorm(d, mean = 0, sd = k / 6)
  theo_normal <- theo_normal / sum(theo_normal) * n

  theo_list <- list(
    Uniform = theo_uniform,
    Triangular = theo_triangular,
    Exponential = theo_exponential,
    Normal = theo_normal
  )

  # ---- Convert theoretical to centered df using same categories ----
  theo_df <- lapply(names(theo_list), function(name) {
    df <- .centered_df(theo_list[[name]], obs$Category)
    df$Shape <- name
    df
  })
  theo_df <- do.call(rbind, theo_df)

  # ---- Plot ----
  ggplot2::ggplot(obs, ggplot2::aes(x = Category, y = Freq, group = 1)) +
    ggplot2::geom_point(size = 3) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_line(
      data = theo_df,
      ggplot2::aes(y = Freq, color = Shape, linetype = Shape, group = Shape),
      linewidth = 1
    ) +
    ggplot2::theme_bw() +
    ggplot2::labs(
      x = var,
      y = ifelse(scale == "percent", "Percentage", "Frequency"),
      color = "Theoretical shape",
      linetype = "Theoretical shape"
    ) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )
}
