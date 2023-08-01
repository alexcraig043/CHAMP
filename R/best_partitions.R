# c = partition count, p = distinct partitions, min = minimum count value
# Eliminates partitions that have a partition count below the min
best_partitions <- function(c, p, min){
  final_count <- list()
  final_partitions <- list()
  for (i in 1:length(c)) {
    countsi <- c[[i]]
    if (!is.null(countsi)) {
      for (j in 1:length(countsi)) {
        if (countsi[[j]] >= min) {
          final_count <- append(final_count, countsi[[j]])
          final_partitions <- append(final_partitions, p[[i]][j])
        }
      }
    }
  }
  return(list("count" = final_count, "partitions" = final_partitions)) 
}