#' Categories: Uniform Distribution of Bikinibottom Species
#'
#' A dataset of dummy nominal data inspired by characters/species
#' from the Bikini Bottom universe (SpongeBob SquarePants).
#' This dataset simulates a roughly uniform distribution across 11 species,
#' with a total of 250 observations. It was intentionally designed to be uniform-like
#' for testing nominal distribution visualization functions.
#'
#' @format A data frame with 250 rows and 1 variable:
#' \describe{
#'   \item{animal}{Character. Species/animal names. 11 species inspired by Bikini Bottom.}
#' }
#'
#' @examples
#' categories
#' # Ranked bar plot of species frequencies
#' ranked_barplot(categories, "animal")
#'
#' # Centered bar plot (most frequent in the center)
#' centered_barplot(categories, "animal")
#'
#' # Centered dot plot with theoretical shape overlays
#' shape_comp_plot(categories, "animal")
"categories"
