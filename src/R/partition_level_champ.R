partition_level_champ <- function(network, partition) {
  a_partition <- 0
  p_partition <- 0
  n <- length(partition)
  for (community in 1:n) { # for each community
    community_members <- partition[[community]]
    num_members <- length(community_members)
    for (i in 1:num_members) { 
      for (j in 1:num_members) { # iterate i and j through num_members in each community
        if (i!=j && are_adjacent(network,community_members[i],
                                 community_members[j])) { # if connected
          a_partition <- a_partition + 
            E(network, P=c(community_members[i],
                         community_members[j]))$weight # add their edge_weight to a_partition
        }
        p_partition <- p_partition + strength(network, v=community_members[i])[[1]]*strength(network, v=community_members[j])[[1]] # regardless, add their strength to p_partition
      }
    }
  }
  p_partition <- p_partition/sum(strength(network)) # divided by strength
  return(list("a_partition" = a_partition, "p_partition" = p_partition))
}

