# Wrapper for other CHAMP_Map_scripts helper functions to get the iterative map
# for the resolution parameter on the CHAMP set of partitions and their
# corresponding domains of optimality.
#
# Expects as input to receive the partition_summary obtained by CHAMP.R.
#
# Code written in parts by Rachel Matthew, Ryan Rebne, Ava Scharfstein and PJM.

get_CHAMP_Map <- function( network, 
                           partitions,
                           partition_summary,
                           plottitle=NULL){

  for (x in 1:lengths(partition_summary)[1]) {
    if (partition_summary[x, "num_clusters"] > 1) {
      partition <- partitions$partitions[[partition_summary[x, "partition_num"]]]
      res_param <- derive_res_parameter(partition, network)
      
      partition_summary[x, "next_gamma"] <- res_param  # gamma stored
      
      # determining which cluster corresponds to said gamma...
      next_partition_num <- 0
      for (i in 1:lengths(partition_summary)[1]) {
        idx <- toString(i)
        if (res_param <= partition_summary[idx, "ending_gamma"]){
          next_partition_num <- idx
          break
        }
      } #... and storing it.
      partition_summary[x, "next_partition_num"] <- partition_summary[next_partition_num, "partition_num"]
      partition_summary[x, "next_num_clusters"] <- partition_summary[next_partition_num, "num_clusters"]
    }
  } 
  # partition_summary is now updated to carry this data for all partitions
  
  #PRINT FIXED POINTS:
  for (x in 1:lengths(partition_summary)[1]) {
    if (!is.na(partition_summary[x, "next_gamma"]) &&
        partition_summary[x, "next_gamma"] > partition_summary[x, "starting_gamma"] &&
        partition_summary[x, "next_gamma"] < partition_summary[x, "ending_gamma"]) {
      pnum <- partition_summary[x, "partition_num"]
      #plot(partitions$partitions[[pnum]], 
      #     network,
      #     main = str_c("Partition", pnum, "with", partition_summary[x, "num_clusters"],
      #                  "clusters", sep = " "))
      print(paste("Partition #",pnum,"(with",partition_summary[x,"num_clusters"],
                  "communities) is a fixed point of the iterative map"))
    }
  }
  
  #PLOTTING:
  plot_data.1 <- data.frame(
    x = sort(c(partition_summary$starting_gamma, partition_summary$ending_gamma)),
    y = sort(c(partition_summary$num_clusters, partition_summary$num_clusters)),
    group = sort(c(1:lengths(partition_summary)[2], 1:lengths(partition_summary)[2])),
    color = sort(c(1:lengths(partition_summary)[2], 1:lengths(partition_summary)[2]))
  )
  
  plot_data.2 <- data.frame(
    x1 = na.omit(partition_summary)$next_gamma,
    y1 = na.omit(partition_summary)$next_num_clusters
  )
  
  x2 <- c();  y2 <- c(); ends <- c()
  for (x in rownames(partition_summary)){
    p <- partition_summary[x,]
    if (!is.na(p$next_gamma)) {
      x2 <- append(x2, (p$starting_gamma + p$ending_gamma)/2)
      x2 <- append(x2, p$next_gamma)
      y2 <- append(y2, p$num_clusters)
      y2 <- append(y2, p$next_num_clusters)
      if ((p$starting_gamma + p$ending_gamma)/2 > p$next_gamma) {
        ends <- append(ends, "first")
      } else { ends <- append(ends, "last") }
    }
  }
  
  plot_data.3 <- data.frame( x2 = x2, y2 = y2, group = sort(c(1:length(x2), 1:length(x2))))
  
  palette <- sample(colors()[c(1:151, 362:657)], lengths(partition_summary)[1], replace=T)
  ggfig <- ggplot(data = plot_data.1) 
  ggfig <- ggfig +
    geom_line(aes(x = x, y = y, group = group, color=palette[color]), linewidth=2) +
    geom_point(aes(x = x, y = y, color=palette[color]), shape=4, size=2, stroke=2) +
    geom_point(data = plot_data.2, aes(x = x1, y = y1), size=2, stroke=1) +
    geom_line(data=plot_data.3, aes(x = x2, y = y2, group = group), 
              linewidth=1, color="darkgray",
              arrow = arrow(length = unit(0.5, "cm"), ends = ends)) +
    guides(color = "none") +
    labs(x = expression(gamma),
         y = "Number of communities",
         title = expression(paste("CHAMP Domains of Optimality and ",gamma," Estimates")))+
    theme_few() +
    theme(axis.text = element_text(size = 8))
  print(ggfig)
  
  #print(partition_summary)
  return(partition_summary)

}
