#' @title Pairwise Jensen-Shannon Similarities (Divergences)
#'
#' @description
#' Calculates the similarity of all pairwise topic combinations using the
#' Jensen-Shannon Divergence.
#'
#' @details
#' The Jensen-Shannon Similarity for two topics \bm z_{i} and \bm z_{j} ...
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
#' @param epsilon [\code{numeric(1)}]\cr
#' TODO!
#' Default is \code{1e-6}.
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
#'   \item{\code{param}}{[\code{named list}] with parameter specifications for
#'   \code{type} [\code{character(1)}] \code{= "Cosine Similarity"} and
#'   \code{epsilon} [\code{numeric(1)}]. See above for explanation.}
#' }
#'
#' @examples
#' res = LDARep(docs = reuters_docs, vocab = reuters_vocab, n = 4, K = 10, num.iterations = 30)
#' topics = mergeTopics(res, vocab = reuters_vocab)
#' js = jsTopics(topics)
#' js
#'
#' sim = getSimilarity(js)
#' dim(sim)
#'
#' js1 = jsTopics(topics, epsilon = 1)
#' sim1 = getSimilarity(js1)
#' summary((sim1-sim)[lower.tri(sim)])
#' plot(sim, sim1, xlab = "epsilon = 1e-6", ylab = "epsilon = 1")
#'
#' @export jsTopics

jsTopics = function(topics, epsilon = 1e-6, progress = TRUE, pm.backend, ncpus){
  if (missing(ncpus)) ncpus = NULL
  if (!missing(pm.backend) && !is.null(pm.backend)){
    jsTopics.parallel(topics = topics, epsilon = epsilon, pm.backend = pm.backend, ncpus = ncpus)
  }else{
    jsTopics.serial(topics = topics, epsilon = epsilon, progress = progress)
  }
}

jsTopics.serial = function(topics, epsilon, progress = TRUE){
  N = ncol(topics)

  rel = topics + epsilon
  rel = t(t(rel)/colSums(rel)) #faster than apply
  logrel = colSums(rel*log(rel))

  sims = matrix(nrow = N, ncol = N)
  colnames(sims) = rownames(sims) = colnames(topics)

  pb = .makeProgressBar(progress = progress,
    total = N-1, format = "Calculate Similarities [:bar] :percent eta: :eta")
  for(i in seq_len(N - 2)){
    sims[(i+1):N,i] = logrel[i] + logrel[(i+1):N] -
      colSums((rel[,i] + rel[,(i+1):N]) * log(rel[,i] + rel[,(i+1):N]))
    pb$tick()
  }
  sims[N, N-1] = logrel[N] + logrel[N-1] -
    sum((rel[,N] + rel[,N-1]) * log(rel[,N] + rel[,N-1]))
  pb$tick()
  sims = 1 - sims/2 - log(2)

  wordsconsidered = rep(nrow(topics), N)
  res = list(sims = sims, wordslimit = wordsconsidered, wordsconsidered = wordsconsidered,
    param = list(type = "Jensen-Shannon Divergence", epsilon = epsilon))
  class(res) = "TopicSimilarity"
  res
}
