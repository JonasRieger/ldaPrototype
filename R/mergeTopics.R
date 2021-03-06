#' @title Merge LDA Topic Matrices
#'
#' @description
#' Generic function, which collects LDA results and merges their topic matrices
#' for a given set of vocabularies.
#'
#' @details
#' This function uses the function \code{\link{mergeRepTopics}} or
#' \code{\link{mergeBatchTopics}}. The topic matrices are transponed and cbinded,
#' so that the resulting matrix contains the counts of vocabularies/words (row wise)
#' in topics (column wise).
#'
#' @family merge functions
#' @family workflow functions
#'
#' @param x [\code{named list}]\cr
#' \code{\link{LDARep}} or \code{\link{LDABatch}} object.
#' @param vocab [\code{character}]\cr
#' Vocabularies taken into consideration for merging topic matrices.
#' @param progress [\code{logical(1)}]\cr
#' Should a nice progress bar be shown? Turning it off, could lead to significantly
#' faster calculation. Default ist \code{TRUE}.
#'
#' @return [\code{named matrix}] with the count of vocabularies (row wise) in topics (column wise).
#'
#' @examples
#' res = LDARep(docs = reuters_docs, vocab = reuters_vocab, n = 4, K = 10, num.iterations = 30)
#' topics = mergeTopics(res, vocab = reuters_vocab)
#' dim(topics)
#' length(reuters_vocab)
#'
#' \dontrun{
#' res = LDABatch(docs = reuters_docs, vocab = reuters_vocab, n = 4, K = 10, num.iterations = 30)
#' topics = mergeTopics(res, vocab = reuters_vocab)
#' dim(topics)
#' length(reuters_vocab)
#' }
#' @export mergeTopics

mergeTopics = function(x, vocab, progress = TRUE) UseMethod("mergeTopics")

#' @export
mergeTopics.LDABatch = function(x, vocab, progress = TRUE){
  mergeBatchTopics(x = x, vocab = vocab, progress = progress)
}

#' @export
mergeTopics.LDARep = function(x, vocab, progress = TRUE){
  mergeRepTopics(x = x, vocab = vocab, progress = progress)
}
