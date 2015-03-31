#' Calculates clone size frequencies if clone counts are given as input
#'
#'@param clones A dataframe with a column counts that contains the counts for 
#'each mutation in a clone \code{clones}
#'
#'@return clones Returns vector with frequencies
#'
#'@keywords frequency clone
#'
#'@export
#'
#'@examples
#'clones <- calculate.frequencies(clones)

calculate.frequencies <- function(df){
  frequency <- df$colony_count*(100/(sum(df$colony_count)))
  return(frequency)
}