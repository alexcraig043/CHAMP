# Returns the partitions as a result of calling cluster_leiden on network for a
# large sequence of gammas
iterate_gamma <- function(network) {
  gamma <- seq(0.25,2,0.01)
  nc <- vector("numeric",length(gamma))
  partitions <- list() 
  for (i in 1:length(gamma)){
    gc <- cluster_leiden(network, objective_function = "modularity",
                         n_iterations = 3, resolution_parameter = gamma[i])
    # change n_iterations to see more noise and bad partition
    gc$gamma <- gamma[i] 
    # cluster number 
    cn <- length(gc)
    cur <- partitions[cn] 
    # if there are no current partitions for this cn
    if (is.na(cur) || is.null(cur[[1]])) {
      partitions[[cn]] <- list(gc) 
    } else {
      cur <- partitions[[cn]] 
      partitions[[cn]][[length(cur)+1]] <- gc
    }
    nc[i] <- cn 
    partitions[[cn]]
  }
  return(partitions)
}
