#' @title Pairwise Cosine Similarities
#'
#' @description
#' Calculates the similarity of all pairwise topic combinations using the
#' Cosine Similarity.
#'
#' @details
#' The Cosine Similarity ...
#'
#' \eqn{J_m} is calculated by
#' \deqn{J_m(\bm z_{i}, \bm z_{j} \mid \bm c) = \frac{\sum_{v = 1}^{V} 1_{\left\{n_{i}^{(v)} > c_i ~\wedge~ n_{j}^{(v)} > c_j\right\}}\left(n_{i}^{(v)}, n_{j}^{(v)}\right)}{\sum_{v = 1}^{V} 1_{\left\{n_{i}^{(v)} > c_i ~\vee~ n_{j}^{(v)} > c_j\right\}}\left(n_{i}^{(v)}, n_{j}^{(v)}\right)}}
#' with \eqn{V} is the vocabulary size, and \eqn{n_k^{(v)}} is the count of
#' assignments of the \eqn{v}-th word to the \eqn{k}-th topic. The threshold vector \eqn{\bm c}
#' is determined by the maximum threshold of the user given lower bounds \code{limit.rel}
#' and \code{limit.abs}. In addition, at least \code{atLeast} words per topic are
#' considered for calculation. According to this, if there are less than
#' \code{atLeast} words considered as relevant after applying \code{limit.rel}
#' and \code{limit.abs} the \code{atLeast} most common words per topic are taken
#' to determine topic similarities.
#'
#' @family TopicSimilarity functions
#'
#' @param topics [\code{named matrix}]\cr
#' The counts of vocabularies/words (row wise) in topics (column wise).
#' @param progress [\code{logical(1)}]\cr
#' Should a nice progress bar be shown? Turning it off, could lead to significantly
#' faster calculation. Default is \code{TRUE}.
#' If \code{pm.backend} is set, parallelization is done and no progress bar will be shown.
#' @param pm.backend [\code{character(1)}]\cr
#' One of "multicore", "socket" or "mpi".
#' If \code{pm.backend} is set, \code{\link[parallelMap]{parallelStart}} is
#' called before computation is started and \code{\link[parallelMap]{parallelStop}}
#' is called after.
#' @param ncpus [\code{integer(1)}]\cr
#' Number of (physical) CPUs to use. If \code{pm.backend} is passed,
#' default is determined by \code{\link[future]{availableCores}}.
#' @return [\code{named list}] with entries
#' \describe{
#'   \item{\code{sims}}{[\code{lower triangular named matrix}] with all pairwise
#'   similarities of the given topics.}
#'   \item{\code{wordslimit}}{[\code{integer}] = vocabulary size. See
#'   \code{\link{jaccardTopics}} for original purpose.}
#'   \item{\code{wordsconsidered}}{[\code{integer}] = vocabulary size. See
#'   \code{\link{jaccardTopics}} for original purpose.}
#'   \item{\code{param}}{[\code{named list}] with parameter
#'   \code{type} [\code{character(1)}] = "Cosine Similarity".}
#' }
#'
#' @examples
#' res = LDARep(docs = reuters_docs, vocab = reuters_vocab, n = 4, K = 10, num.iterations = 30)
#' topics = mergeTopics(res, vocab = reuters_vocab)
#' cosine = cosineTopics(topics)
#' cosine
#'
#' sim = getSimilarity(cosine)
#' dim(sim)
#'
#' @export cosineTopics

cosineTopics = function(topics, progress = TRUE, pm.backend, ncpus){
  if (missing(ncpus)) ncpus = NULL
  if (!missing(pm.backend) && !is.null(pm.backend)){
    cosineTopics.parallel(topics = topics, pm.backend = pm.backend, ncpus = ncpus)
  }else{
    cosineTopics.serial(topics = topics, progress = progress)
  }
}

cosineTopics.serial = function(topics, progress = TRUE){
  N = ncol(topics)

  rel = t(t(topics)/colSums(topics)) #faster than apply

  sims = matrix(nrow = N, ncol = N)
  colnames(sims) = rownames(sims) = colnames(topics)

  pb = .makeProgressBar(progress = progress,
    total = N-1, format = "Calculate Similarities [:bar] :percent eta: :eta")
  for(i in seq_len(N - 2)){
    sims[(i+1):N,i] = colSums(rel[,i] * rel[,(i+1):N]) / (sqrt(sum(rel[,i]^2))*sqrt(colSums(rel[,(i+1):N]^2)))
    pb$tick()
  }
  sims[N, N-1] = sum(rel[,N] * rel[,N-1]) / (sqrt(sum(rel[,N]^2))*sqrt(sum(rel[,N-1]^2)))
  pb$tick()

  wordsconsidered = rep(nrow(topics), N)
  res = list(sims = sims, wordslimit = wordsconsidered, wordsconsidered = wordsconsidered,
    param = list(type = "Cosine Similarity"))
  class(res) = "TopicSimilarity"
  res
}
