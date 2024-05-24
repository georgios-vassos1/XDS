#' Multiply a number by two
#'
#' @param x A single integer
#' @return The input multiplied by two
#' @examples
#' timesTwo(5)
#' @useDynLib DataStructures
#' @export
timesTwo <- function(x) {
  .Call('_DataStructures_timesTwo', PACKAGE = 'DataStructures', x)
}

