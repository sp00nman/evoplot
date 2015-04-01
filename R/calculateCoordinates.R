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
#'@keywords coordinates gamma alpha beta
#'
#'@export
#'
#'@examples
#'alpha_beta <- calculate.alpha.beta.coordinates(df_freq[df_freq$clone==1, ],
#'                                               xaxis_length=4,
#'                                               yaxis_length=yaxis_length_per_clone[1],
#'                                               xy_start=c(0,0))
calculate.gamma.coordinates <- function(freq_clones, xaxis_length, 
                                        yaxis_length, xy_start=c(0,0),
                                        debug=FALSE) {
  offset <- xy_start[2] #where to place the polygon
  y_coord <- (yaxis_length/2)+offset
  num_clones <- length(freq_clones$clone)
  
  if (debug) {print(c("yaxis_length: ", yaxis_length))}
  if (debug) {print(c("offset: ", offset))}
  if (debug) {print(c("y_coord: ", y_coord))}
  if (debug) {print(c("number of clones: ", num_clones))}
  if (debug) {print(c("x-axis_length: ", xaxis_length))}
  
  x <- c()
  y <- rep(y_coord,num_clones)
  
  if (debug) {print(c("y: ",y))}
  
  x_gamma_start <- xy_start[1] #draw first point [1] -->x
  dis_points <- (xaxis_length*(9/10)/num_clones)
  
  if (debug) {print(c("dis_points: ", dis_points))}
  
  x <- cbind(x, x_gamma_start)
  num_polygons <- length(freq_clones$clone)
  if (debug) {print(c("nr_triangles: ", num_polygons))}
  j=1
  if (num_polygons!=1){     
    for (i in 1:(num_polygons-1)) {  #-1 --> x[1] is filled with x_gamma_start
      if (debug) {print(c("j: ", j))}
      if (debug) {print(c("x[j]: ", x[j]))}
      
      x <- append(x, x[j]+dis_points)
      j=j+1
      
      if (debug) {print(c("x", x))}
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
#'@return list Returns a list with x and y coordinates for alpha & beta
#'
#'@keywords coordinates alpha beta
#'
#'@export
#'
#'@examples
#'alpha_beta <- calculate.alpha.beta.coordinates(df_freq[df_freq$clone==1, ],
#'                                               xaxis_length=4,
#'                                               yaxis_length=yaxis_length_per_clone[1],
#'                                               xy_start=c(0,0),
#'                                               debug=TRUE)
calculate.alpha.beta.coordinates <- function(freq_clones, xaxis_length, 
                                             yaxis_length, xy_start=c(0,0),
                                             debug=FALSE){
  offset <- xy_start[2] #where to place the polygon
  y_coord=(yaxis_length/2)+offset
  nr_clones <- length(freq_clones$clone)
  
  if (debug) {print(c("offset: ", offset))}
  if (debug) {print(c("y_coord: ", y_coord))}
  if (debug) {print(c("number of clones: ", nr_clones))}
  if (debug) {print(c("freq_length: ", xaxis_length))}
  
  x <- rep(xaxis_length,nr_clones*2)
  
  if (debug) {print(c("x:", x))}
  
  y_alpha <- c()
  y_alpha_a <- 0
  y_beta <- c()
  y_beta_a <- 0
  prev_rel_dis <- 0
  j=1
  for (i in 1:length(freq_clones$clone)){
    if (debug) {print(c("j: ", j))}
    
    rev_clones <- rev(as.character(freq_clones$clone)) #order C-->B-->A
    rel_dis <-(xaxis_length*freq_clones[freq_clones$clone==rev_clones[j], ]$frequency)/100
    prev_rel_dis <- cbind(prev_rel_dis, rel_dis)
    
    if (debug) {print(c("rel_dis: ", rel_dis))}
    if (debug) {print(c("prev_rel_dis[j]: ", prev_rel_dis[j]))}
    if (debug) {print(c("y_alpha_a before", y_alpha_a))}
    if (debug) {print(c("y_beta_a before", y_beta_a))}
    
    y_alpha_a <- y_coord-((rel_dis/2)+(prev_rel_dis[j]/2))
    y_alpha <- cbind(y_alpha, y_alpha_a)
    y_beta_a <- y_coord+((rel_dis/2)+(prev_rel_dis[j]/2))
    y_beta <- cbind(y_beta, y_beta_a)
    
    if (debug) {print(c("y_alpha:", y_alpha))}
    if (debug) {print(c("y_beta:", y_beta))}
    
    j=j+1
  } 
  return(list(x=x, y_alpha=y_alpha, y_beta=y_beta))
}
