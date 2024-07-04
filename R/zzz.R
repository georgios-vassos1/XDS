#' @useDynLib XDS, .registration=TRUE
#' @importFrom Rcpp sourceCpp
NULL

#' @export
Rcpp::loadModule("QueueElementModule", TRUE)
