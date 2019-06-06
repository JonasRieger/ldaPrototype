#' @title Merge LDA Topic Matrices
#'
#' @description
#' Generic function, which collects LDA results and merges their topic matrices for a
#' given set of vocabularies. Uses the function \code{\link{mergeRepTopics}}
#' or \code{\link{mergeBatchTopics}}.
#'
#' @details
#'
#' @param x [\code{named list}]\cr
#' \code{\link{LDARep}} object or \code{\link{LDABatch}} object.
#' @param vocab [\code{character}]\cr
#' Vocabularies taken into consideration for merging topic matrices.
#' @param progress [\code{logical(1)}]\cr
#' Should a nice progress bar be shown? Turning it off, could lead to significantly
#' faster calculation. Default ist \code{TRUE}.
#' @return [\code{named matrix}] with the count of vocabularies (row wise) in topics (column wise).
#'
#' @examples
#' #TODO
#'
#' @export mergeTopics

mergeTopics = function(...) UseMethod("mergeTopics")

#' @export
mergeTopics.LDABatch = function(x, vocab, progress = TRUE){
  NextMethod("mergeBatchTopics")
}

#' @export
mergeTopics.LDARep = function(x, vocab, progress = TRUE){
  NextMethod("mergeRepTopics")
}
