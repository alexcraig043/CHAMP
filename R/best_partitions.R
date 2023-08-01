#' Find Best Partitions
#'
#' This function identifies and returns partitions that have a partition count greater or equal to the specified minimum value.
#'
#' @param counts A list of partition counts.
#' @param partitions A list of distinct partitions corresponding to the counts.
#' @param min_value The minimum count value to be considered.
#' @return A list with two elements: 'count' containing the filtered counts, and 'partitions' containing the corresponding partitions.
#' @examples
#' counts_list <- list(c(3, 5), c(4, 2))
#' partitions_list <- list(c("A", "B"), c("C", "D"))
#' best_partitions(counts_list, partitions_list, min_value = 3)
best_partitions <- function(counts, partitions, min_value) {
  final_count <- numeric(0)
  final_partitions <- vector("list", length = 0)

  for (i in seq_along(counts)) {
    countsi <- counts[[i]]
    if (!is.null(countsi)) {
      valid_indices <- countsi >= min_value
      final_count <- c(final_count, countsi[valid_indices])
      final_partitions <- c(final_partitions, partitions[[i]][valid_indices])
    }
  }

  return(list("count" = final_count, "partitions" = final_partitions))
}
