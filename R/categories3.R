#' Categories3: Exponential/Dominance Distribution of Bikinibottom Species
#'
#' A dataset of dummy nominal data inspired by characters/species
#' from the Bikini Bottom universe (SpongeBob SquarePants).
#' This dataset simulates a highly skewed distribution where
#' a few species dominate most of the frequency (long-tail / exponential pattern).
#' It was intentionally designed for pedagogical purposes to demonstrate
#' dominance and Pareto-like behavior in nominal data.
#'
#' @format A data frame with 250 rows and 1 variable:
#' \describe{
#'   \item{animal}{Character. Species/animal names. 11 species inspired by Bikini Bottom.}
#' }
#'
#' @examples
#' categories3
#' # Centered dot plot showing exponential/long-tail pattern
#' shape_comp_plot(categories3, "animal")
#'
#' # Pareto chart highlighting cumulative frequency and dominance
#' pareto(categories3, "animal")
#'
#' # Optional: ranked or centered bar plots
#' ranked_barplot(categories3, "animal")
#' centered_barplot(categories3, "animal")
"categories3"
