#' @title Merge LDA Topic Matrices
#'
#' @description
#' Generic function, which collects LDA results and merges their topic matrices for a
#' given set of vocabularies. Uses the function \code{\link{mergeRepTopics}}
#' or \code{\link{mergeBatchTopics}}.
#'
#' @details
#'
#' @param ... Arguments \code{x}, \code{vocab} and \code{progress} for
#' \code{\link{mergeRepTopics}} or \code{\link{mergeBatchTopics}}.
#' @return [\code{named matrix}] with the count of vocabularies (row wise) in topics (column wise).
#'
#' @examples
#' #TODO
#'
#' @export mergeTopics

mergeTopics = function(...) UseMethod("mergeTopics")

#' @export
mergeTopics.LDABatch = function(x, vocab, progress = TRUE){
  mergeBatchTopics(x = x, vocab = vocab, progress = progress)
}

#' @export
mergeTopics.LDARep = function(x, vocab, progress = TRUE){
  mergeRepTopics(x = x, vocab = vocab, progress = progress)
}
