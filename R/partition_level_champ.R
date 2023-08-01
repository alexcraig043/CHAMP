#' Calculate partition level metrics for CHAMP
#'
#' @description Computes the metrics `a_partition` and `p_partition` based on the given network
#'   and partition.
#'
#' @param network The network to be analyzed.
#' @param partition A list representing the partitioning of the network's vertices.
#' @return A list containing the values for `a_partition` and `p_partition`.
#' @examples
#' result <- partition_level_champ(network, partition)
partition_level_champ <- function(network, partition) {
  edge_weight_sum <- 0
  strength_product_sum <- 0
  total_strength <- sum(strength(network))

  for (community in partition) { # for each community
    community_members <- community
    num_members <- length(community_members)
    community_strength <- strength(network, v = community_members)

    for (i in seq_len(num_members)) {
      for (j in seq_len(num_members)) { # iterate i and j through num_members in each community
        if (i != j && are_adjacent(network, community_members[i], community_members[j])) { # if connected
          edge_weight_sum <- edge_weight_sum +
            E(network, P = c(community_members[i], community_members[j]))$weight # add their edge_weight
        }
        strength_product_sum <- strength_product_sum + community_strength[i] * community_strength[j] # regardless, add their strength
      }
    }
  }

  normalized_strength_product <- strength_product_sum / total_strength # divided by total strength

  return(list("a_partition" = edge_weight_sum, "p_partition" = normalized_strength_product))
}
