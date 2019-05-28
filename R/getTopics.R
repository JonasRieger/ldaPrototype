#' @title Getter and Setter for LDA
#'
#' @description
#'
#' @param x [\code{named list}]\cr
#' \code{\link{LDA}} object.

#' @export getTopics
getTopics = function(x) UseMethod("getTopics")

#' @export
getTopics.LDA = function(x){
  x$topics
}

#' @rdname getTopics
#' @export getAssignments
getAssignments = function(x) UseMethod("getAssignments")

#' @export
getAssignments.LDA = function(x){
  x$assignments
}

#' @rdname getTopics
#' @export getDocument_sums
getDocument_sums = function(x) UseMethod("getDocument_sums")

#' @export
getDocument_sums.LDA = function(x){
  x$document_sums
}

#' @rdname getTopics
#' @export getDocument_expects
getDocument_expects = function(x) UseMethod("getDocument_expects")

#' @export
getDocument_expects.LDA = function(x){
  x$document_expects
}

#' @rdname getTopics
#' @export getLog_likelihoods
getLog_likelihoods = function(x) UseMethod("getLog_likelihoods")

#' @export
getLog_likelihoods.LDA = function(x){
  x$log.likelihoods
}
