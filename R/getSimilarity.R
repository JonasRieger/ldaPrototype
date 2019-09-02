#' @title Getter for TopicSimilarity
#'
#' @description
#' Returns the corresponding element of a \code{\link[=jaccardTopics]{TopicSimilarity}} object.
#'
#' @family getter functions
#' @family TopicSimilarity funtions
#'
#' @param x [\code{named list}]\cr
#' \code{\link[=jaccardTopics]{TopicSimilarity}} object.

#' @export getSimilarity
getSimilarity = function(x) UseMethod("getSimilarity")

#' @export
getSimilarity.TopicSimilarity = function(x){
  x$sims
}

#' @rdname getSimilarity
#' @export getRelevantWords
getRelevantWords = function(x) UseMethod("getRelevantWords")

#' @export
getRelevantWords.TopicSimilarity = function(x){
  x$wordslimit
}

#' @rdname getSimilarity
#' @export getConsideredWords
getConsideredWords = function(x) UseMethod("getConsideredWords")

#' @export
getConsideredWords.TopicSimilarity = function(x){
  x$wordsconsidered
}

#' @rdname getSimilarity
#' @export
getParam.TopicSimilarity = function(x){
  x$param
}
