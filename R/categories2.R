#' Categories2: Triangular Distribution of Bikinibottom Species
#'
#' A dataset of dummy nominal data inspired by characters/species
#' from the Bikini Bottom universe (SpongeBob SquarePants).
#' This dataset simulates a roughly triangular distribution of frequencies.
#'
#' @format A data frame with 11 rows and 2 variables:
#' \describe{
#'   \item{animal}{Character. Species/animal names.}
#'   \item{freq}{Integer. Frequency of each species, forming a triangular pattern.}
#' }
#'
#' @examples
#' categories2
#' barplot(categories2$freq, names.arg = categories2$animal)
"categories2"
