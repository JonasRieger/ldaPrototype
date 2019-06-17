#' @title Getter for LDA
#'
#' @description
#' Returns the corresponding element of a \code{\link{LDA}} object.
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
#' @export getLog.likelihoods
getLog.likelihoods = function(x) UseMethod("getLog.likelihoods")

#' @export
getLog.likelihoods.LDA = function(x){
  x$log.likelihoods
}


getAlpha = function(x){
  x$param$alpha
}

getEta = function(x){
  x$param$eta
}

getK = function(x){
  x$param$K
}

getNum.iterations = function(x){
  x$param$num.iterations
}

getParam = function(x){
  x$param
}

getEstimators = function(x){
  alpha = getAlpha(x)
  beta = getEta(x)
  K = getK(x)
  topics = getTopics(x)
  documents = getDocument_sums(x)

  topicSums = rowSums(topics)
  documentSums = colSums(documents)
  L = ncol(topics)

  phi = (topics + beta) / (topicSums + L*beta)
  theta = t((t(documents) + alpha) / (documentSums + K*alpha))
  return(list(phi = phi, theta = theta))
}
