# Parameters: 
# - partition: a partition from which to compute the resolution parameter
# - network: the network on which the partition is placed
derive_res_parameter <- function(partition, network) {
  # modified weight network for appropriate balance
  mw_network <- network
  E(mw_network)$weight <- E(mw_network)$weight / mean(E(mw_network)$weight)
  
  if (partition$nb_clusters > 1) {
    m <- sum(E(mw_network)$weight)
    m.in <- 0
    k.c <- 0
    for (i in 1:partition$nb_clusters){
      partition_graph <- induced_subgraph(mw_network, V(mw_network)[partition$membership == i])
      m.in <- m.in + sum(E(partition_graph)$weight)
      k.c <- k.c + ( sum(degree(mw_network)[partition$membership == i]) )**2
    }
    m.out <- m - m.in
    
    theta.in <- 2*m.in / (k.c/(2*m))
    theta.out <- 2*m.out / (2*m - k.c/(2*m))
    
    parameter <- (theta.in - theta.out) / (log(theta.in) - log(theta.out))
    return (parameter)
  } else {
    return (NULL)
  }
}