#' @title Merge LDA Topic Matrices
#'
#' @description
#' Collects LDA results and merges their topic matrices for a given set of vocabularies.
#'
#' @details
#'
#' @param x [\code{named list}]\cr
#' Output from \code{\link{LDARep}} or \code{\link{LDABatch}}.
#' @return [\code{named matrix}] with the count of vocabularies (row wise) in topics (column wise).
#'
#' @examples
#' #TODO
#'
#' @export mergeTopics

mergeTopics = function(x){
  UseMethod("mergeTopics")
}
