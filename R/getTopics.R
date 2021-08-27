#' @title Getter for LDA
#'
#' @description
#' Returns the corresponding element of a \code{\link{LDA}} object.
#' \code{getEstimators} computes the estimators for \code{phi} and \code{theta}.
#'
#' @details
#' The estimators for \code{phi} and \code{theta} in
#' \deqn{w_n^{(m)} \mid T_n^{(m)}, \bm\phi_k  \sim \textsf{Discrete}(\bm\phi_k),}
#' \deqn{\bm\phi_k  \sim \textsf{Dirichlet}(\eta),}
#' \deqn{T_n^{(m)} \mid \bm\theta_m  \sim \textsf{Discrete}(\bm\theta_m),}
#' \deqn{\bm\theta_m  \sim \textsf{Dirichlet}(\alpha)}
#' are calculated referring to Griffiths and Steyvers (2004) by
#' \deqn{\hat{\phi}_{k, v} = \frac{n_k^{(v)} + \eta}{n_k + V \eta},}
#' \deqn{\hat{\theta}_{m, k} = \frac{n_k^{(m)} + \alpha}{N^{(m)} + K \alpha}}
#' with \eqn{V} is the vocabulary size, \eqn{K} is the number of modeled topics;
#' \eqn{n_k^{(v)}} is the count of assignments of the \eqn{v}-th word to
#' the \eqn{k}-th topic. Analogously, \eqn{n_k^{(m)}} is the count of assignments
#' of the \eqn{m}-th text to the \eqn{k}-th topic. \eqn{N^{(m)}} is the total
#' number of assigned tokens in text \eqn{m} and \eqn{n_k} the total number of
#' assigned tokens to topic \eqn{k}.
#'
#' @references Griffiths, Thomas L. and Mark Steyvers (2004). "Finding scientific topics".
#' In: \emph{Proceedings of the National Academy of Sciences} \bold{101} (suppl 1), pp.5228--5235,
#' \doi{10.1073/pnas.0307752101}.
#'
#' @family getter functions
#' @family LDA functions
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
