find_max <- function(mod_matrix, gammas) {
  boost <- 10^-8
  
  points_of_change <- c()
  corresponding_partitions <- c()
  
  gam <- min(gammas)
  while (gam < max(gammas)) {
    spread <- mod_matrix["base"] - gam*mod_matrix["decrement"]
    part <- which(spread==max(spread))[[1]]
    
    mod_matrix["tilmax"] <- (mod_matrix[part, "base"] - mod_matrix["base"])/
                            (mod_matrix[part, "decrement"] - mod_matrix["decrement"])
    mod_matrix[part, "tilmax"] <- NaN
    mod_matrix[which(mod_matrix["tilmax"]<=gam), "tilmax"] <- NaN
    if (all(is.na(mod_matrix["tilmax"]))) { break }
    
    gam <- min(mod_matrix["tilmax"], na.rm=T) + boost
    points_of_change <- append(points_of_change, gam)
    corresponding_partitions <- append(corresponding_partitions, part)
  }

  return (list("edges"=points_of_change, "partitions"=corresponding_partitions))
}