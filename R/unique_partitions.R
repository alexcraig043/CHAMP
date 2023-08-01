#' Find Unique Partitions and Their Counts
#'
#' @description Given a list of partitions, finds the unique partitions and the
#'   number of times each unique partition appeared.
#'
#' @param partitions A list of partitions.
#' @return A list containing two elements:
#'   - "count": A list containing the counts of each unique partition.
#'   - "partitions": A list containing the unique partitions.
#' @examples
#' result <- unique_partitions(partitions)
unique_partitions <- function(partitions) {
  partition_count <- vector("list", length = length(partitions))
  distinct_partitions <- vector("list", length = length(partitions))

  for (i in seq_along(partitions)) {
    clusters <- partitions[[i]]

    if (!is.null(clusters)) {
      partition_count[[i]] <- rep(1, length(clusters))
      distinct_partitions[[i]] <- clusters

      if (length(clusters) > 1) {
        for (j in seq_len(length(clusters) - 1)) {
          if (partition_count[[i]][j] != 0) {
            for (k in seq(j + 1, length(clusters))) {
              if (partition_count[[i]][k] != 0 && compare(clusters[[j]], clusters[[k]]) == 0) {
                partition_count[[i]][k] <- 0
                partition_count[[i]][j] <- partition_count[[i]][j] + 1
              }
            }
          }
        }
      }
    }
  }

  # Remove 0s and NAs
  for (i in seq_along(distinct_partitions)) {
    if (!is.null(distinct_partitions[[i]])) {
      valid_indices <- partition_count[[i]] != 0
      distinct_partitions[[i]] <- distinct_partitions[[i]][valid_indices]
      partition_count[[i]] <- partition_count[[i]][valid_indices]
    }
  }

  return(list("count" = partition_count, "partitions" = distinct_partitions))
}
