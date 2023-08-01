# Wrapper for other CHAMP_Map_scripts helper functions to identify the Convex
# Hull of Admissible Modularity Partitions (CHAMP) set of partitions and their
# corresponding domains of optimality.

# Code written in parts by Rachel Matthew, Ryan Rebne, Ava Scharfstein and PJM.

CHAMP <- function(network,
                  partitions,
                  plottitle = NULL) {
  # In the original CHAMP paper and python implementation, we use QHull to do
  # the heavy lifting. But since we're only dealing with a 1D parameter space
  # here (for single-layer networks; multilayer networks will be dealt with
  # elsewhere), we can instead brute-force the identification of the upper
  # envelope of the Q(gamma) lines. (See the paper for figures of this.)

  mod_matrix <- data.frame(
    row.names = 1:length(partitions$partitions),
    base = 1:length(partitions$partitions),
    decrement = 1:length(partitions$partitions)
  )

  for (k in 1:length(partitions$partitions)) {
    partition <- partitions$partitions[[k]]
    # VERY SLOW: ret <- partition_level_champ(network, partition)
    # mod_matrix[k, "base"] <- ret$a_partition
    # mod_matrix[k, "decrement"] <- ret$p_partition
    A <- modularity(network, membership(partitions$partitions[[k]]),
      resolution = 0, weights = E(network)$weight
    )
    Q <- modularity(network, membership(partitions$partitions[[k]]),
      resolution = 1, weights = E(network)$weight
    )
    mod_matrix[k, "base"] <- A
    mod_matrix[k, "decrement"] <- (A - Q)
  }

  # print(mod_matrix)

  # To start, gamma is set to 0. The modularity of each partition at this point is calculated.
  gam <- 0
  mods <- mod_matrix["base"] - gam * mod_matrix["decrement"]
  # print(paste("The modularities:", mods))

  # The maximum partition at this value of gamma is detected. because gamma=0 here, this is the first max-modularity partition in the range. mmp="maximum modularity partition"
  mmp <- which(mods == max(mods))[[1]]

  # Now algebra is used to compute when (at what value of gamma) each of the remaining partitions will have a greater modularity than the current max-modularity partition.
  # In this case, this calculates when each other partition will overtake the first mmp.
  mod_matrix["tilmax"] <- (mod_matrix[mmp, "base"] - mod_matrix["base"]) /
    (mod_matrix[mmp, "decrement"] - mod_matrix["decrement"])
  # The algorithm proceeds from left to right, so no alues less than the current gamma need be visited, nor should the same partition as the current mmp be chosen. To avoid this:
  mod_matrix[mmp, "tilmax"] <- NaN
  mod_matrix[which(mod_matrix["tilmax"] <= gam), "tilmax"] <- NaN

  # print(mod_matrix)

  # If this leaves no more available partitions, then the loop ends here. In this case, partitions are still available, so we continue. The minimum value remaining of the partitions' tilmax is the soonest gamma at which a new partition becomes the mmp, so we shift our gamma value there and record the end of the previous mmp
  gam <- min(mod_matrix["tilmax"], na.rm = T) + 10^-8
  # print(paste("partition", mmp, "is best from 0 to", gam))
  # note the added 10^-8
  # boost is necessary to ensure we have PASSED the end of the current mmp's range.

  # At this point the loop repeats, adjusting the mmp and repeating the process.
  mods <- mod_matrix["base"] - gam * mod_matrix["decrement"]
  # print(paste("The modularities:", mods))
  mmp <- which(mods == max(mods))[[1]]
  # print(paste(mmp, "is now the mmp."))

  # It repeats until no further partitions overtake the current mmp at a later gamma or until gamma exceeds a set range (the find_max function takes the gamma range as an argument)
  fmax <- find_max(mod_matrix, c(partitions$gamma_min, partitions$gamma_max))


  # Now for plotting purposes we re-do the whole thing brute-forced at equispaced gamma values
  gammas <- seq(
    partitions$gamma_min, partitions$gamma_max,
    (partitions$gamma_max - partitions$gamma_min) / 200
  )
  a <- list()
  p <- list()
  k <- 0
  # Note this whole loop just recomputes items computed above. Need to remove.
  for (partition in partitions$partitions) {
    k <- k + 1
    # VERY SLOW: ret <- partition_level_champ(network, partition)
    # a[k] <- ret$a_partition
    # p[k] <- ret$p_partition
    A <- modularity(network, membership(partitions$partitions[[k]]),
      resolution = 0, weights = E(network)$weight
    )
    Q <- modularity(network, membership(partitions$partitions[[k]]),
      resolution = 1, weights = E(network)$weight
    )
    a[k] <- A
    p[k] <- (A - Q)
  }

  modularity <- array(NA, dim = c(length(gammas), length(partitions$partitions)))
  for (k in 1:length(partitions$partitions)) {
    for (g in 1:length(gammas)) {
      modularity[g, k] <- a[[k]] - (gammas[g] * p[[k]])
    }
  }

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
