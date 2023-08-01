# Given a list of partitions, finds the unique ones 
# and the number of times each unique partition appeared.
unique_partitions <- function(partitions) {
  # number of distinct partitions 
  partition_count <- vector("list", length = length(partitions)) 
  # which partitions are distinct
  distinct_partitions <- vector("list", length = length(partitions))
  for (i in 1:length(partitions)) {
    clustersi <- partitions[[i]] 
    if (!is.null(clustersi)) { 
      partition_count[i] <- list(rep(1,length(clustersi))) 
      distinct_partitions[[i]] <- clustersi
      # only want to iterate if length of clustersi is greater than 1
      if (length(clustersi)>1) {
        for (j in 1:(length(clustersi)-1)) {
          # if cluster j is not a duplicate
          if (partition_count[[i]][j]!=0) {
            for (k in length(clustersi):(j+1)) {
              # if cluster k is not a duplicate
              if (partition_count[[i]][k]!=0) { 
                cmp <- compare(clustersi[[j]],clustersi[[k]])
                if (cmp==0) {
                  partition_count[[i]][k] <- 0
                  partition_count[[i]][j] <- partition_count[[i]][j]+1
                  #distinct_partitions[[i]][k] <- NULL
                }
              }
            }
          }
        }
      }
      else {
        partition_count[[i]][1] <- 1
      }
    }
  }
  
  # clean up distinct_partitions and partition_count (remove 0s and NAs)
  for (i in 1:length(distinct_partitions)) {
    if (!is.null(distinct_partitions[[i]])) {
      distinct_partitions[[i]] <- distinct_partitions[[i]][c(partition_count[[i]]!=0)]
      partition_count[[i]] <- partition_count[[i]][partition_count[[i]]!=0] # eliminate zeros
    }
  }
  
  return(list("count" = partition_count, "partitions" = distinct_partitions))
}
