#' CHAMP function
#'
#' This function identifies the Convex Hull of Admissible Modularity Partitions (CHAMP)
#' set of partitions and their corresponding domains of optimality.
#'
#' @param network The network to analyze.
#' @param partitions The list of partitions to consider.
#' @param plottitle The title of the plot to generate (optional).
#'
#' @return A summary of the CHAMP partitions, including the range of gamma values
#' for which each partition is part of the CHAMP set and the number of clusters in each partition.
#'
#' @export
#'
#' @examples
#' # The function takes a network and partitions as input. An example use could be:
#' # partition_summary <- CHAMP(network, partitions)
#' @author Code written in parts by Rachel Matthew, Ryan Rebne, Ava Scharfstein and PJM.
CHAMP <- function(network,
                  partitions,
                  plottitle = NULL) {
  "In the original CHAMP paper and python implementation, we use QHull to do
  the heavy lifting. But since we're only dealing with a 1D parameter space
  here (for single-layer networks; multilayer networks will be dealt with
  elsewhere), we can instead brute-force the identification of the upper
  envelope of the Q(gamma) lines. (See the paper for figures of this.)"

  # Creating a matrix to store values, more efficient than data frame
  mod_matrix <- matrix(0, nrow = length(partitions$partitions), ncol = 3)

  # Calculating base and decrement values using vectorized operations
  mod_matrix[, 1] <- sapply(partitions$partitions, function(p) {
    modularity(network, membership(p), resolution = 0, weights = E(network)$weight)
  })
  mod_matrix[, 2] <- mod_matrix[, 1] - sapply(partitions$partitions, function(p) {
    modularity(network, membership(p), resolution = 1, weights = E(network)$weight)
  })

  "To start, gamma is set to 0. The modularity of each partition at this point is calculated."
  # Initial calculations
  gam <- 0
  mods <- mod_matrix[, 1] - gam * mod_matrix[, 2]
  mmp <- which.max(mods)

  # Calculating tilmax using vectorized operations
  mod_matrix[, 3] <- (mod_matrix[mmp, 1] - mod_matrix[, 1]) /
    (mod_matrix[mmp, 2] - mod_matrix[, 2])
  mod_matrix[mmp, 3] <- NaN
  mod_matrix[mod_matrix[, 3] <= gam, 3] <- NaN

  # Computing new value of gamma
  gam <- min(mod_matrix[, 3], na.rm = TRUE) + 10^-8

  # Repeating calculations
  mods <- mod_matrix[, 1] - gam * mod_matrix[, 2]
  "The maximum partition at this value of gamma is detected. because gamma=0 here, this is the first max-modularity partition in the range. mmp='maximum modularity partition'"
  mmp <- which.max(mods)
  fmax <- find_max(mod_matrix, c(partitions$gamma_min, partitions$gamma_max))

  # Defining gammas and pre-allocating vectors
  gammas <- seq(
    partitions$gamma_min, partitions$gamma_max,
    (partitions$gamma_max - partitions$gamma_min) / 200
  )
  a <- p <- numeric(length(partitions$partitions))

  # Calculating a and p using sapply
  a <- sapply(partitions$partitions, function(p) {
    modularity(network, membership(p), resolution = 0, weights = E(network)$weight)
  })
  p <- a - sapply(partitions$partitions, function(p) {
    modularity(network, membership(p), resolution = 1, weights = E(network)$weight)
  })

  # Vectorized computation of modularity
  modularity <- outer(a, gammas, "-") - outer(p, gammas, "*")

  best_gammas <- fmax$edges
  best_gammas <- c(best_gammas[-length(best_gammas)], partitions$gamma_max)
  corresponding_partitions <- fmax$partitions
  best_modularities <- c()
  for (g in 1:length(best_gammas)) {
    partition_index <- corresponding_partitions[g]
    best_modularities[g] <- a[[partition_index]] - (best_gammas[g] * p[[partition_index]])
  }

  # Plot data frames
  all <- data.frame(x = gammas)
  for (i in 1:dim(modularity)[2]) {
    all <- cbind(all, i = modularity[, i])
  }
  colnames(all) <- c("x", paste("", 1:dim(modularity)[2], sep = ""))
  all <- melt(all, id = "x")
  colnames(all)[2] <- "partition_num"
  last <- corresponding_partitions[length(corresponding_partitions)]

  best_gammas <- c(partitions$gamma_min, best_gammas)
  best_modularities <- c(modularity[1, 1], best_modularities)
  last <- length(best_modularities)
  best <- data.frame(best_gammas, best_modularities)
  segments <- data.frame(
    x1 = best_gammas[1:last - 1],
    y1 = best_modularities[1:last - 1],
    x2 = best_gammas[2:last],
    y2 = best_modularities[2:last],
    partitions = corresponding_partitions
  )

  title <- plottitle
  if (is.null(plottitle)) {
    title <- " "
  }

  ggfig <- ggplot()
  ggfig <- ggfig +
    geom_line(
      data = all,
      mapping = aes(
        x = x,
        y = value,
        group = partition_num
      ),
      show.legend = F,
      color = all$partition_num,
      alpha = .3,
      na.rm = T
    ) +
    geom_segment(
      data = segments,
      mapping = aes(
        x = x1,
        y = y1,
        xend = x2,
        yend = y2
      ),
      color = "#63666A",
      linewidth = 1.5,
      na.rm = T
    ) +
    geom_text(
      data = segments,
      aes(
        x = (x1 + x2) / 2,
        y = (y1 + y2) / 2,
        label = partitions
      ),
      color = segments$partitions,
      vjust = -.5
    ) +
    geom_segment(
      data = best,
      mapping = aes(
        x = best_gammas,
        xend = best_gammas,
        y = best_modularities,
        yend = -Inf
      ),
      linetype = "dashed",
      color = "black",
      na.rm = T
    ) +
    geom_point(
      data = best,
      mapping = aes(
        x = best_gammas,
        y = best_modularities
      ),
      color = "black",
      na.rm = T
    ) +
    labs(
      x = expression(paste("Resolution Parameter (", gamma, ")")),
      y = "Modularity",
      title = title
    ) +
    scale_y_continuous(
      breaks = seq(0, max(segments$y1), length = 10),
      labels = round(seq(0, max(segments$y1), length = 10)),
      expand = c(0, 0),
      limits = c(0, max(segments$y1))
    ) +
    scale_x_continuous(
      breaks = c(segments$x1, 2),
      labels = c(round(segments$x1, 2), 2),
      limits = c(0, partitions$gamma_max),
      expand = c(0, 0)
    ) +
    theme_few() +
    theme(axis.text = element_text(size = 8))
  print(ggfig)

  partition_summary <- data.frame(matrix(ncol = 9, nrow = length(segments[, 1])))
  colnames(partition_summary) <- c(
    "segment_length", "starting_gamma", "ending_gamma",
    "gamma_range", "partition_num", "num_clusters",
    "next_gamma", "next_partition_num", "next_num_clusters"
  )

  for (x in 1:lengths(partition_summary)[1]) {
    partition_summary[x, "segment_length"] <- sqrt((segments[x, "x1"] - segments[x, "x2"])**2 + (segments[x, "y1"] - segments[x, "y2"])**2)
    partition_summary[x, "starting_gamma"] <- segments[x, "x1"]
    partition_summary[x, "ending_gamma"] <- segments[x, "x2"]
    partition_summary[x, "gamma_range"] <- abs(segments[x, "x1"] - segments[x, "x2"])
    partition_summary[x, "partition_num"] <- segments[x, "partitions"]
    partition_summary[x, "num_clusters"] <- partitions$partitions[segments$partitions][[x]]$nb_clusters
  }

  partition_summary <- partition_summary[order(-partition_summary$gamma_range), ]

  print(paste(
    count(partition_summary),
    "partitions in the CHAMP set (i.e., on the upper envelope of Q v. gamma)"
  ))
  # print(partition_summary)
  return(partition_summary)
}
