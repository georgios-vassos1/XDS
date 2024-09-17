#' @useDynLib XDS, .registration=TRUE
#' @importFrom Rcpp evalCpp
NULL

#' PriorityQueueWrapper class
#'
#' This class provides an interface to a priority queue in C++ using Rcpp. 
#' It allows you to push, pop, and check the state of the priority queue.
#'
#' @export
#' @name PriorityQueueWrapper
NULL

#' QueueWrapper class
#'
#' This class provides an interface to a generic queue in C++ using Rcpp.
#' It allows you to enqueue, dequeue, and check the state of the queue for any R-compatible type.
#'
#' @export
#' @name QueueWrapper
NULL

.onLoad <- function(libname, pkgname) {
  # Load both Rcpp modules when the package is loaded
  Rcpp::loadModule("PriorityQueueModule", TRUE)
  Rcpp::loadModule("QueueModule", TRUE)
}
