#' Calculates clone size frequencies if clone counts are given as input
#'
#'@param clones A dataframe with a column counts that contains the counts for 
#'each mutation in a clone \code{clones}
#'
#'@return clones Adds a column frequency to the dataframe with the corresponding 
#'frequencies for each mutation
#'
#'@keywords frequency clone
#'
#'@export
#'
#'@examples
#'clones <- calculate.frequencies(clones)

calculate.frequencies <- function(clones){
  frequency <- clones$counts*(100/(sum(clones$counts)))
  clones$frequency <- frequency
  return(clones)
}