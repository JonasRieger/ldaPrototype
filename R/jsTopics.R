#' @title Pairwise Jensen-Shannon Similarities (Divergences)
#'
#' @description
#' Calculates the similarity of all pairwise topic combinations using the
#' Jensen-Shannon Divergence.
#'
#' @details
#' The Jensen-Shannon Similarity for two topics \eqn{\bm z_{i}} and
#' \eqn{\bm z_{j}} is calculated by
#' \deqn{JS(\bm z_{i}, \bm z_{j}) = 1 - \left( KLD\left(\bm p_i, \frac{\bm p_i + \bm p_j}{2}\right) + KLD\left(\bm p_j, \frac{\bm p_i + \bm p_j}{2}\right) \right)/2}
#' \deqn{= 1 - KLD(\bm p_i, \bm p_i + \bm p_j)/2 - KLD(\bm p_j, \bm p_i + \bm p_j)/2 - \log(2)}
#' with \eqn{V} is the vocabulary size, \eqn{\bm p_k = \left(p_k^{(1)}, ..., p_k^{(V)}\right)},
#' and \eqn{p_k^{(v)}} is the proportion of assignments of the
#' \eqn{v}-th word to the \eqn{k}-th topic. KLD defines the Kullback-Leibler
#' Divergence calculated by
#' \deqn{KLD(\bm p_{k}, \bm p_{\Sigma}) = \sum_{v=1}^{V} p_k^{(v)} \log{\frac{p_k^{(v)}}{p_{\Sigma}^{(v)}}}.}
#'
#' There is an \code{epsilon} added to every \eqn{n_k^{(v)}}, the count
#' (not proportion) of assignments to ensure computability with respect to zeros.
#'
#' @family TopicSimilarity functions
#'
#' @param topics [\code{named matrix}]\cr
#' The counts of vocabularies/words (row wise) in topics (column wise).
#' @param epsilon [\code{numeric(1)}]\cr
#' Numerical value added to \code{topics} to ensure computability. See details.
#' Default is \code{1e-06}.
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

jsTopics.parallel = function(topics, epsilon, pm.backend, ncpus){
  N = ncol(topics)

  rel = topics + epsilon
  rel = t(t(rel)/colSums(rel)) #faster than apply
  logrel = colSums(rel*log(rel))

  if (missing(ncpus) || is.null(ncpus)) ncpus = future::availableCores()
  parallelMap::parallelStart(mode = pm.backend, cpus = ncpus)

  fun = function(s){
    lapply(s, function(i)
      logrel[i] + logrel[(i+1):N] -
        colSums((rel[,i] + rel[,(i+1):N]) * log(rel[,i] + rel[,(i+1):N])))
  }

  parallelMap::parallelExport("rel", "logrel", "N")
  sequences = lapply(seq_len(max(ncpus, 2)), function(x) seq(x, N-2, max(ncpus, 2)))
  val = parallelMap::parallelMap(fun = fun, sequences)
  parallelMap::parallelStop()

  rearrangedlist = list()
  for (i in seq_along(sequences)){
    rearrangedlist[sequences[[i]]] = val[[i]]
  }
  rm(val)

  sims = matrix(nrow = N, ncol = N)
  colnames(sims) = rownames(sims) = colnames(topics)
  sims[lower.tri(sims)] = c(unlist(rearrangedlist),
    logrel[N] + logrel[N-1] -
      sum((rel[,N] + rel[,N-1]) * log(rel[,N] + rel[,N-1])))

  wordsconsidered = rep(nrow(topics), N)
  res = list(sims = sims, wordslimit = wordsconsidered, wordsconsidered = wordsconsidered,
    param = list(type = "Jensen-Shannon Divergence"))
  class(res) = "TopicSimilarity"
  res
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
