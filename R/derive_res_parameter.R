#' Derive Resolution Parameter
#'
#' This function computes the resolution parameter based on a given partition
#' and network. The resolution parameter is computed only if there is more
#' than one cluster in the partition.
#'
#' @param partition A list containing details of the partition. It should include
#'        'nb_clusters' for the number of clusters, and 'membership' for the membership of each element.
#' @param network The network graph on which the partition is placed.
#' @return A numeric value representing the resolution parameter, or NULL if there's only one cluster.
#' @examples
#' # Assume you have a partition object and a network object defined
#' derive_res_parameter(partition, network)
derive_res_parameter <- function(partition, network) {
  # Modify weight network for appropriate balance
  mw_network <- network
  E(mw_network)$weight <- E(mw_network)$weight / mean(E(mw_network)$weight)

  if (partition$nb_clusters > 1) {
    m <- sum(E(mw_network)$weight)
    m.in <- 0
    k.c <- 0
    for (i in seq_along(partition$nb_clusters)) {
      members <- partition$membership == i
      partition_graph <- induced_subgraph(mw_network, V(mw_network)[members])
      m.in <- m.in + sum(E(partition_graph)$weight)
      k.c <- k.c + sum(degree(mw_network)[members])^2
    }
    m.out <- m - m.in

    theta.in <- 2 * m.in / (k.c / (2 * m))
    theta.out <- 2 * m.out / (2 * m - k.c / (2 * m))

    parameter <- (theta.in - theta.out) / (log(theta.in) - log(theta.out))
    return(parameter)
  } else {
    return(NULL)
  }
}
