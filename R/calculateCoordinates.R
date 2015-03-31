#' Calculate gamma coordinates to draw the polygons
#' 
#'              (beta)
#'              ^ 
#'            + + 
#'  (gamma) < + + 
#'            + + 
#'              v 
#'              (alpha)
#'
#'@param freq_clones A vector with the frequency of the clones \code{freq_clones}
#'@param xaxis_length Length of the x-axis \code{xaxis_length}
#'@param yaxis_length Length of the y-axis \code{yaxis_length}
#'@param xy_start Coordinates for the starting point of the plot \code{xy_start}
#'
#'@return list Returns a list with x and y coordinates
#'
#'@keywords coordinates
#'
#'@export
#'
#'@examples
#'gamma_coordinates_s <- calculate.gamma.coordinates(freq_clones[freq_clones$independent==2, ],
#'xaxis_length=4, 
#'yaxis_length=yaxis_length_per_clone[2],
#'xy_start=c(0,yaxis_length_per_clone[1])
calculate.gamma.coordinates <- function(freq_clones, xaxis_length, yaxis_length, xy_start=c(0,0)) {
  offset <- xy_start[2] #where to place the triangle
  y_coord=(yaxis_length/2)+offset
  nr_clones <- length(freq_clones$clone)
  print(c("yaxis_length: ", yaxis_length))
  print(c("offset: ", offset))
  print(c("y_coord: ", y_coord))
  print(c("number of clones: ", nr_clones))
  print(c("x-axis_length: ", xaxis_length))
  x <- NULL
  y <- rep(y_coord,nr_clones)
  print(c("y: ",y))
  j=1
  x_gamma_start <- xy_start[1] #draw first point [1] -->x
  dis_points <- (xaxis_length*(9/10)/nr_clones)
  print(c("dis_points: ", dis_points))
  x = append(x, x_gamma_start)
  
  nr_triangles <- length(freq_clones$clone)
  print(c("nr_triangles: ", nr_triangles))
  if (nr_triangles!=1){     
    for (i in 1:(nr_triangles-1)) {  #-1 'cause x[1] is filled with x_gamma_start
      print(c("j: ", j))
      print(c("x[j]: ", x[j]))
      x <- append(x, x[j]+dis_points)
      j=j+1
      print(c("x", x))
    }
  }
  return(list(x=x, y=y))
}

#' Calculate alpha & beta coordinates to draw the polygons
#' 
#'              (beta)
#'              ^ 
#'            + + 
#'  (gamma) < + + 
#'            + + 
#'              v 
#'              (alpha)
#'
#'@param freq_clones A vector with the frequency of the clones \code{freq_clones}
#'@param xaxis_length Length of the x-axis \code{xaxis_length}
#'@param yaxis_length Length of the y-axis \code{yaxis_length}
#'@param xy_start Coordinates for the starting point of the plot \code{xy_start}
#'
#'@return list Returns a list with x and y coordinates
#'
#'@keywords coordinates
#'
#'@export
#'
#'@examples
#'alpha_beta_coordinates_s <- calculate.alpha.beta.coordinates(freq_clones[freq_clones$independent==2, ],
#'                                                             xaxis_length=4,
#'                                                             yaxis_length=yaxis_length_per_clone[2],
#'                                                             xy_start=c(0,yaxis_length_per_clone[1]))
calculate.alpha.beta.coordinates <- function(freq_clones, xaxis_length, yaxis_length, xy_start=c(0,0)){
  offset <- xy_start[2] #where to place the triangle
  y_coord=(yaxis_length/2)+offset
  nr_clones <- length(freq_clones$clone)
  print(c("offset: ", offset))
  print(c("y_coord: ", y_coord))
  print(c("number of clones: ", nr_clones))
  print(c("freq_length: ", xaxis_length))
  
  x <- rep(xaxis_length,nr_clones*2)
  print(c("x:", x))
  y_alpha <- NULL
  y_alpha_a <- 0
  y_beta <- NULL
  y_beta_a <- 0
  j=1
  prev_rel_dis <- 0
  
  for (i in 1:length(freq_clones$clone)){
    print(c("j: ", j))
    rev_clones <- rev(as.character(freq_clones$clone)) #order C-->B-->A
    rel_dis <-(xaxis_length*freq_clones[freq_clones$clone==rev_clones[j], ]$frequency)/100
    prev_rel_dis <- append(prev_rel_dis, rel_dis)
    print(c("rel_dis: ", rel_dis))
    print(c("prev_rel_dis[j]: ", prev_rel_dis[j]))
    print(c("y_alpha_a before", y_alpha_a))
    print(c("y_beta_a before", y_beta_a))
    y_alpha_a <- y_coord-((rel_dis/2)+(prev_rel_dis[j]/2))
    y_alpha <- append(y_alpha, y_alpha_a)
    y_beta_a <- y_coord+((rel_dis/2)+(prev_rel_dis[j]/2))
    y_beta <- append(y_beta, y_beta_a)
    print(c("y_alpha:", y_alpha))
    print(c("y_beta:", y_beta))
    
    j=j+1
  } 
  return(list(x=x, y_alpha=y_alpha, y_beta=y_beta))
}