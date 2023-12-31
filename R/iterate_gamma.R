#' Iterate over a range of gamma values and return the partitions from `cluster_leiden`
#'
#' @description Calls `cluster_leiden` on the given network for a sequence of gamma
#'   values, returning the partitions.
#'
#' @param network The network to be clustered (community detection).
#' @return A list of partitions generated by `cluster_leiden` over the gamma range.
#' @examples
#' partitions <- iterate_gamma(network)
iterate_gamma <- function(network) {
  gamma <- seq(0.25, 2, 0.01)
  nc <- numeric(length(gamma))
  partitions <- list()

  for (i in seq_along(gamma)) {
    gc <- cluster_leiden(network,
      objective_function = "modularity",
      n_iterations = 3, resolution_parameter = gamma[i]
    )
    gc$gamma <- gamma[i]

    # Cluster number
    cn <- length(gc)
    current_partitions <- partitions[cn]

    # If there are no current partitions for this cn
    if (is.na(current_partitions) || is.null(current_partitions[[1]])) {
      partitions[[cn]] <- list(gc)
    } else {
      current_partitions <- partitions[[cn]]
      partitions[[cn]][[length(current_partitions) + 1]] <- gc
    }

    nc[i] <- cn
  }

  return(partitions)
}
