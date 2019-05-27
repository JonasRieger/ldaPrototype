#' @title Merge LDA Topic Matrices
#'
#' @description
#' generic function to collect LDA results and merges their topic matrices for a
#' given set of vocabularies. Uses the function \code{\link{mergeRepTopics}}
#' or \code{\link{mergeBatchTopics}}.
#'
#' @details
#'
#' @param x [\code{named list}]\cr
#' Output from \code{\link{LDARep}} or \code{\link{LDABatch}}.
#' @param vocab [\code{character}]\cr
#' Vocabularies taken into consideration for merging topic matrices.
#' @return [\code{named matrix}] with the count of vocabularies (row wise) in topics (column wise).
#'
#' @examples
#' #TODO
#'
#' @export mergeTopics

mergeTopics = function(x, vocab) UseMethod("mergeTopics")

#' @export
mergeTopics.LDABatch = function(x, vocab){
  mergeBatchTopics(x = x, vocab = vocab)
}

#' @export
mergeTopics.LDARep = function(x, vocab){
  mergeRepTopics(x = x, vocab = vocab)
}
