#' Categories3: Exponential/Dominance Distribution of Bikinibottom Species
#'
#' A dataset of dummy nominal data inspired by characters/species
#' from the Bikini Bottom universe (SpongeBob SquarePants).
#' This dataset simulates a highly skewed distribution where
#' a few species dominate most of the frequency (long tail).
#'
#' @format A data frame with 11 rows and 2 variables:
#' \describe{
#'   \item{animal}{Character. Species/animal names.}
#'   \item{freq}{Integer. Frequency of each species, forming a long-tail/exponential pattern.}
#' }
#'
#' @examples
#' categories3
#' barplot(categories3$freq, names.arg = categories3$animal)
"categories3"
