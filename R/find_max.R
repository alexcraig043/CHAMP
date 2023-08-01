#' Find Maxima in Modularity Matrix
#'
#' This function identifies points of change and corresponding partitions based on a given
#' modularity matrix and range of gamma values.
#'
#' @param mod_matrix A matrix containing 'base' and 'decrement' columns representing the modularity structure.
#' @param gammas A numeric vector representing the range of gamma values to consider.
#' @return A list with two elements: 'edges', containing the points of change, and 'partitions', containing the corresponding partitions.
#' @examples
#' # Assume mod_matrix is defined with appropriate 'base' and 'decrement' columns
#' # and gammas is defined as a range of values
#' find_max(mod_matrix, gammas)
find_max <- function(mod_matrix, gammas) {
  boost <- 10^-8

  points_of_change <- numeric(0)
  corresponding_partitions <- numeric(0)

  gam <- min(gammas)
  while (gam < max(gammas)) {
    spread <- mod_matrix["base"] - gam * mod_matrix["decrement"]
    part <- which.max(spread)

    tilmax <- (mod_matrix[part, "base"] - mod_matrix["base"]) /
      (mod_matrix[part, "decrement"] - mod_matrix["decrement"])

    tilmax[part] <- NaN
    tilmax[tilmax <= gam] <- NaN

    if (all(is.na(tilmax))) {
      break
    }

    gam <- min(tilmax, na.rm = TRUE) + boost
    points_of_change <- c(points_of_change, gam)
    corresponding_partitions <- c(corresponding_partitions, part)
  }

  return(list("edges" = points_of_change, "partitions" = corresponding_partitions))
}
