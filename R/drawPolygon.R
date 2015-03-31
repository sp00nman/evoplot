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
draw.clonal.evolution.plot <- function(d){
  #draw every 3rd elemen
  nr_clones <- length(unique(d$clone))
  cells_intermediate <- append(1, (d$r[seq_along(d$r) %% 3 == 0])+1)
  cells <- cells_intermediate[-length(cells_intermediate)]
  
  #get labels
  #sub_tab <- d[cells,]
  #mut1 <- strsplit(as.character(sub_tab[sub_tab$clone=="B",]$mutation), ",")[[1]][1]
  #mut2 <- strsplit(as.character(sub_tab[sub_tab$clone=="B",]$mutation), ",")[[1]][2]
  #mut3 <- strsplit(as.character(sub_tab[sub_tab$clone=="B",]$mutation), ",")[[1]][3]
  #mut4 <- strsplit(as.character(sub_tab[sub_tab$clone=="B",]$mutation), ",")[[1]][4]
  #mut5 <- strsplit(as.character(sub_tab[sub_tab$clone=="C",]$mutation), ",")[[1]][1]
  
  g_obj <- ggplot()
  g_obj = g_obj + geom_polygon(data=d, aes(x=x, y=y, group=clone, fill=clone, colour=clone), size=1, alpha=0.5) 
  g_obj = g_obj + geom_point(data=d[cells,], aes(x=x, y=y ), color="black", size=8)
  g_obj = g_obj + geom_point(data=d[cells,], aes(x=x, y=y, color=clone), size=7)
  g_obj = g_obj + geom_point(data=d[cells,], aes(x=x, y=y ), color="black", size=3)
  #g_obj = g_obj + geom_text(data=d[cells,], aes(x=x, y=y, label=clone), hjust=0.5, vjust=-1, size=9)
  #first A, then B and C
  g_obj = g_obj + annotate("text", label=mut1, x=0.4, y=1.8, size=5) #A mut1
  g_obj = g_obj + annotate("text", label=mut2, x=0.4, y=1.65, size=5) #A mut2
  g_obj = g_obj + annotate("text", label=mut3, x=0.4, y=1.5, size=5) #A mut2
  g_obj = g_obj + annotate("text", label=mut1, x=((4/nr_clones))+0.5, y=1.8, size=5) #B mut1
  g_obj = g_obj + annotate("text", label=mut2, x=((4/nr_clones))+0.5, y=1.65, size=5) #B mut1
  g_obj = g_obj + annotate("text", label=mut3, x=((4/nr_clones))+0.5, y=1.5, size=5) #B mut1
  #g_obj = g_obj + annotate("text", label=mut4, x=((4/nr_clones))+0.5, y=1.35, size=5) #B mut1
  #g_obj = g_obj + annotate("text", label=mut5, x=0.4, y=3.8, size=5) #B mut1
  
  #g_obj = g_obj + geom_text(data=d[cells,][d$clone=="A",], aes(x=x, y=y, label=mutation), hjust=0.1, vjust=3.5, size=3)
  #draw arrows
  g_obj = g_obj + annotate("segment", x=-0.1, y=-0.2, xend=4, yend=-0.2, arrow = arrow(length = unit(0.3, "cm")), size=0.6)
  #g_obj = g_obj + annotate("segment", x=1, y=5, xend=5, yend=5, arrow = arrow(length = unit(0.3, "cm")), size=0.6)
  g_obj = g_obj + annotate("segment", x=-0.1, y=0, xend=4, yend=0, size=1, colour="#999999")
  g_obj = g_obj + annotate("segment", x=-0.1, y=4, xend=4, yend=4, size=1, colour="#999999")
  g_obj = g_obj + annotate("segment", x=4, y=0, xend=4, yend=4, size=1, colour="#999999")
  g_obj = g_obj + annotate("segment", x=-0.1, y=0, xend=-0.1, yend=4, size=1, colour="#999999")
  g_obj = g_obj + xlab("time")
  g_obj = g_obj + ylab("relative clone size")
  #g_obj = g_obj + mtext(side=3)
  g_obj = g_obj + theme_bw()
  g_obj = g_obj + theme(axis.ticks = element_blank(),
                        legend.position="none",
                        #legend.position=c(0.1,0.2),
                        #legend.text=element_text(size=16),
                        #legend.title=element_text(size=14),
                        axis.text.x=element_blank(),
                        axis.text.y=element_blank(), 
                        axis.title.x=element_text(size=20, vjust=3),
                        axis.title.y=element_text(size=20, vjust=15.5), #15 relative clone size, for pdf
                        panel.grid.major=element_blank(),
                        panel.grid.minor=element_blank(),
                        panel.border=element_blank(),
                        panel.background=element_blank())
  return(g_obj)
}
