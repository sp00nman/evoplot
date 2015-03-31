#' Calculate the length of the x-axis
#'
#'@param freq_clones Frequency of clones \code{freq_clones}
#'@param xaxis_length Length of the xaxis \code{xaxis_length}
#'
#'@return xaxis_length_per_clone 
#'
#'@keywords frequency clone
#'
#'@export
#'
#'@examples
#'yaxis_length_per_clone_1 <- calculate.xaxis.length.clones(freq_clones_1, xaxis_length=4)
calculate.xaxis.length.clones <-function(freq_clones, xaxis_length=4){
  nr_clones <-length(unique(freq_clones$independent))
  xaxis_length_per_clone <- NULL
  print(c("nr_clones: ",nr_clones))
  for (i in 1:nr_clones){
    sum_clone_frequency <- sum(freq_clones[freq_clones$independent==i,]$frequency)
    rel_xaxis_dis <- (sum_clone_frequency/100)*xaxis_length
    print(c("sum_clone_frequency: ", sum_clone_frequency))
    print(c("rel_xaxis_dis: ", rel_xaxis_dis))
    xaxis_length_per_clone <- append(xaxis_length_per_clone, rel_xaxis_dis)
  }
  print(c("xaxis_length_per_clone: ", xaxis_length_per_clone))
  return(xaxis_length_per_clone)
}