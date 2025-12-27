#' Categories: Uniform Distribution of Bikinibottom Species
#'
#' A dataset of dummy nominal data inspired by characters/species
#' from the Bikini Bottom universe (SpongeBob SquarePants).
#' This dataset simulates a roughly uniform distribution,
#' where all species have similar frequencies.
#'
#' @format A data frame with 11 rows and 2 variables:
#' \describe{
#'   \item{animal}{Character. Species/animal names.}
#'   \item{freq}{Integer. Frequency of each species, approximately uniform across species.}
#' }
#'
#' @examples
#' categories
#' barplot(categories$freq, names.arg = categories$animal)
"categories"
