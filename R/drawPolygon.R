#' Draw the polygone given as input the coordinates
#'
#'@param d dataframe with coordinates \code{d}
#'
#'@return g_obj a ggplot object ready to plot with plot(g_obj)
#'
#'@keywords plot polygon
#'
#'@export
#'
#'@examples
#'g_plot <- draw.clonal.evolution.plot(d)
draw.clonal.evolution.plot <- function(d_plot){
  
  #draw every 3rd element
  nr_clones <- length(unique(d_plot$clone))
  cells_intermediate <- append(1, (d_plot$r[seq_along(d_plot$r) %% 3 == 0])+1)
  cells <- cells_intermediate[-length(cells_intermediate)]
  
  g_obj <- ggplot()
  g_obj = g_obj + geom_polygon(data=d_plot,  mapping=aes(x=x, y=y, group=clone, fill=clone, colour=clone ), size=1, alpha=0.5) 
  # draw circles
  g_obj = g_obj + geom_point(data=d_plot[cells,], aes(x=x, y=y ), color="black", size=8)
  g_obj = g_obj + geom_point(data=d_plot[cells,], aes(x=x, y=y, color=clone), size=7)
  g_obj = g_obj + geom_point(data=d_plot[cells,], aes(x=x, y=y ), color="black", size=3)
  
  # label circles
  g_obj = g_obj + geom_text(data=d_plot[cells,], aes(x=x, y=y, label=clone), hjust=0.5, vjust=-1, size=9)
  
  # gene labels for each circle
  circle_coord <- d_plot[cells,]
  for (i in 1:length(circle_coord$clone)){
    genes <- unlist(strsplit(circle_coord[i,]$mutation, ","))
    ypos <- 0 
    for (j in 1:length(genes)){
      print(genes[j])
      print(circle_coord[j,]$x+0.2)
      print(circle_coord[j,]$y-ypos-0.2)
      g_obj = g_obj + annotate("text", 
                               label=genes[j], 
                               x=circle_coord[i,]$x+0.25, 
                               y=circle_coord[i,]$y-ypos-0.25, 
                               size=4)
      ypos <- ypos+0.2
    }
  }
  
  #draw arrows
  g_obj = g_obj + annotate("segment", x=-0.1, y=-0.2, xend=4, yend=-0.2, arrow = arrow(length = unit(0.3, "cm")), size=0.6)
  g_obj = g_obj + annotate("segment", x=-0.1, y=0, xend=4, yend=0, size=0.5, colour="black")
  g_obj = g_obj + annotate("segment", x=-0.1, y=4, xend=4, yend=4, size=0.5, colour="black")
  g_obj = g_obj + annotate("segment", x=4, y=0, xend=4, yend=4, size=0.5, colour="black")
  g_obj = g_obj + annotate("segment", x=-0.1, y=0, xend=-0.1, yend=4, size=0.5, colour="black")
  g_obj = g_obj + xlab("time")
  g_obj = g_obj + ylab("relative clone size")
  
  # theme
  g_obj = g_obj + theme_bw()
  g_obj = g_obj + theme(axis.ticks = element_blank())
  g_obj = g_obj + theme(axis.text.x=element_blank())
  g_obj = g_obj + theme(axis.text.y=element_blank())
  g_obj = g_obj + theme(axis.title.x=element_text(size=20, vjust=3))
  g_obj = g_obj + theme(axis.title.y=element_text(size=20, vjust=15.5)) #15 relative clone size, for pdf
  g_obj = g_obj + theme(legend.position="none")
  g_obj = g_obj + theme(panel.grid.major=element_blank())
  g_obj = g_obj + theme(panel.grid.minor=element_blank())
  g_obj = g_obj + theme(panel.border=element_blank())
  g_obj = g_obj + theme(panel.background=element_blank())
  
  return(g_obj)
}

