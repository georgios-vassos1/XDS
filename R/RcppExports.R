# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#' @useDynLib XDS
#' @export
createQueueElement <- function(priority, eventType, additionalInfo) {
    .Call(`_XDS_createQueueElement`, priority, eventType, additionalInfo)
}

#' @useDynLib XDS
#' @export
releaseQueueElement <- function(x) {
    invisible(.Call(`_XDS_releaseQueueElement`, x))
}

#' @useDynLib XDS
#' @export
getQueueElementInfo <- function(x) {
    .Call(`_XDS_getQueueElementInfo`, x)
}

