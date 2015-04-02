#' Helper function that calls other functions to calculate coordinates
#'
#'
#'@param df dataframe with calculated frequencies \code{df}
#'@param yaxis_length Length of the yaxis [num] \code{yaxis_length}
#'@param debug TRUE gives verbose output [boolean] \code{debug}
#'
#'@return dataframe with x & y coordinates for all polygons 
#'
#'@keywords coordinates
#'
#'@export
#'
#'@examples
#'yaxis_length_per_clone <- calculate.yaxis.length.clones(df_freq, yaxis_length=4, debug=TRUE)
get_coordinates <- function(df, yaxis_length=4, debug=TRUE){
  yaxis_length_per_clone <- calculate.yaxis.length.clones(df, 
                                                          yaxis_length=4, 
                                                          debug=TRUE)
  
  gamma_per_clone <- list()
  alpha_beta_per_clone <- list()
  offset <- 0
  for (i in 1:(length(yaxis_length_per_clone))) {
    clone_name <- paste("clone_", i, sep="") 
    # calculate gamma point for each clone
    gamma <- calculate.gamma.coordinates(df[df$clone==i, ],
                                         xaxis_length=4,
                                         yaxis_length=yaxis_length_per_clone[i],
                                         xy_start=c(0, offset),
                                         debug=TRUE)
    gamma_per_clone[[clone_name]] <- gamma
    
    # calculate alpha beta point for each clone
    alpha_beta <- calculate.alpha.beta.coordinates(df[df$clone==i, ],
                                                   xaxis_length=4,
                                                   yaxis_length=yaxis_length_per_clone[i],
                                                   xy_start=c(0, offset),
                                                   debug=TRUE)
    alpha_beta_per_clone[[clone_name]] <- alpha_beta
    # increase offset
    offset <- offset + yaxis_length_per_clone[i]
  }
  
  # build dataframe for plotting
  
  gamma_x <- as.vector(unlist(sapply(gamma_per_clone, function(x){return(as.vector(x$x))}, simplify=TRUE)))
  alpha_x <- as.vector(unlist(sapply(alpha_beta_per_clone, function(x){return(as.vector(x$x))}, simplify=TRUE)))
  beta_x <- as.vector(unlist(sapply(alpha_beta_per_clone, function(x){return(as.vector(x$x))}, simplify=TRUE)))
  
  gamma_y <- as.vector(unlist(sapply(gamma_per_clone, function(x){return(as.vector(x$y))}, simplify=TRUE)))
  alpha_y <- as.vector(unlist(sapply(alpha_beta_per_clone, function(x){return(as.vector(x$y_alpha))}, simplify=TRUE)))
  beta_y <- as.vector(unlist(sapply(alpha_beta_per_clone, function(x){return(as.vector(x$y_beta))}, simplify=TRUE)))
  
  x <- c()
  y <- c()
  for (i in 1:(length(gamma_x))){
    print(c("i: ", i))
    subclones_coord_x <- as.vector(c(gamma_x[i], alpha_x[i], beta_x[i]))
    x <- as.vector(c(x, subclones_coord_x))
    subclones_coord_y <- as.vector(c(gamma_y[i], alpha_y[i], beta_y[i]))
    y <- as.vector(c(y, subclones_coord_y)) 
}
  
  clone <- as.vector(sapply(df$subclone, function(x){return(rep(x,3))}, simplify=TRUE))
  mutation <- as.vector(sapply(df$gene, function(x){return(rep(x,3))}, simplify=TRUE))
  r  <- seq(1:length(x))
  
  d_plot  <- data.frame(x=x, y=y, clone=clone, mutation=mutation,r=r, stringsAsFactors=FALSE)
  
  return(d_plot)
  
}
