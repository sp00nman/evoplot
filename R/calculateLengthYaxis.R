#' Calculate the length of the y-axis
#'
#'@param freq_clones Frequency of clones \code{freq_clones}
#'@param yaxis_length Length of the yaxis \code{yaxis_length}
#'
#'@return yaxis_length_per_clone Returns a vector  
#'
#'@keywords frequency clone
#'
#'@export
#'
#'@examples
#'yaxis_length_per_clone <- calculate.yaxis.length.clones(df_freq, yaxis_length=4, debug=TRUE)
calculate.yaxis.length.clones <-function(freq_clones, yaxis_length=4, debug=FALSE){
  
  uniq_clones <-length(unique(freq_clones$clone))
  yaxis_length_per_clone <- c()
  if (debug){print(c("number of unique clones: ", uniq_clones))}
  
  for (i in 1:uniq_clones){
    sum_clone_frequency <- sum(freq_clones[freq_clones$clone==i,]$subclone_freq)
    rel_yaxis_dis <- (sum_clone_frequency/100)*yaxis_length
    yaxis_length_per_clone <- c(yaxis_length_per_clone, rel_yaxis_dis)
    if (debug){print(c("clone: ", i))}
    if (debug){print(c("sum_clone_frequency: ", sum_clone_frequency))}
    if (debug){print(c("rel_yaxis_dis: ", rel_yaxis_dis))}
  }
  
  return(yaxis_length_per_clone)
}