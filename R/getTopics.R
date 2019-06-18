#' @title Getter for LDA
#'
#' @description
#' Returns the corresponding element of a \code{\link{LDA}} object.
#' \code{getEstimators} computes the estimators for \code{phi} and \code{theta}.
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

#' @rdname getTopics
#' @export getParam
getParam = function(x) UseMethod("getParam")

#' @export
getParam.LDA = function(x){
  x$param
}

#' @rdname getTopics
#' @export getK
getK = function(x) UseMethod("getK")

#' @export
getK.LDA = function(x){
  x$param$K
}

#' @rdname getTopics
#' @export getAlpha
getAlpha = function(x) UseMethod("getAlpha")

#' @export
getAlpha.LDA = function(x){
  x$param$alpha
}

#' @rdname getTopics
#' @export getEta
getEta = function(x) UseMethod("getEta")

#' @export
getEta.LDA = function(x){
  x$param$eta
}

#' @rdname getTopics
#' @export getNum.iterations
getNum.iterations = function(x) UseMethod("getNum.iterations")

#' @export
getNum.iterations.LDA = function(x){
  x$param$num.iterations
}

#' @rdname getTopics
#' @export getEstimators
getEstimators = function(x) UseMethod("getEstimators")

#' @export
getEstimators.LDA = function(x){
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
