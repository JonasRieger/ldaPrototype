#' @title Pairwise Cosine Similarities
#'
#' @description
#' Calculates the similarity of all pairwise topic combinations using the
#' Cosine Similarity.
#'
#' @details
#' The Cosine Similarity for two topics \eqn{\bm z_{i}} and \eqn{\bm z_{j}}
#' is calculated by
#' \deqn{ \cos(\theta | \bm z_{i}, \bm z_{j}) = \frac{ \sum_{v=1}^{V}{n_{i}^{(v)} \cdot n_{j}^{(v)}} }{ \sqrt{\sum_{v=1}^{V}{\left(n_{i}^{(v)}\right)^2}} \cdot \sqrt{\sum_{v=1}^{V}{\left(n_{j}^{(v)}\right)^2}} }}
#' with \eqn{\theta} determining the angle between the corresponding
#' count vectors \eqn{\bm z_{i}} and \eqn{\bm z_{j}},
#' \eqn{V} is the vocabulary size and \eqn{n_k^{(v)}} is the count of
#' assignments of the \eqn{v}-th word to the \eqn{k}-th topic.
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
#'   \code{type} [\code{character(1)}] \code{= "Cosine Similarity"}.}
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

cosineTopics.parallel = function(topics, pm.backend, ncpus){
  N = ncol(topics)

  rel = t(t(topics)/colSums(topics)) #faster than apply
  squaresums = sqrt(colSums(rel^2))

  if (missing(ncpus) || is.null(ncpus)) ncpus = future::availableCores()
  parallelMap::parallelStart(mode = pm.backend, cpus = ncpus)

  fun = function(s){
    lapply(s, function(i)
      colSums(rel[,i] * rel[,(i+1):N]) / squaresums[i] / squaresums[(i+1):N])
  }

  parallelMap::parallelExport("rel", "squaresums", "N")
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
    sum(rel[,N] * rel[,N-1]) / squaresums[N] / squaresums[N-1])

  wordsconsidered = rep(nrow(topics), N)
  res = list(sims = sims, wordslimit = wordsconsidered, wordsconsidered = wordsconsidered,
    param = list(type = "Cosine Similarity"))
  class(res) = "TopicSimilarity"
  res
}

cosineTopics.serial = function(topics, progress = TRUE){
  N = ncol(topics)

  rel = t(t(topics)/colSums(topics)) #faster than apply
  squaresums = sqrt(colSums(rel^2))

  sims = matrix(nrow = N, ncol = N)
  colnames(sims) = rownames(sims) = colnames(topics)

  pb = .makeProgressBar(progress = progress,
    total = N-1, format = "Calculate Similarities [:bar] :percent eta: :eta")
  for(i in seq_len(N - 2)){
    sims[(i+1):N,i] = colSums(rel[,i] * rel[,(i+1):N]) / squaresums[i] / squaresums[(i+1):N]
    pb$tick()
  }
  sims[N, N-1] = sum(rel[,N] * rel[,N-1]) / squaresums[N] / squaresums[N-1]
  pb$tick()

  wordsconsidered = rep(nrow(topics), N)
  res = list(sims = sims, wordslimit = wordsconsidered, wordsconsidered = wordsconsidered,
    param = list(type = "Cosine Similarity"))
  class(res) = "TopicSimilarity"
  res
}
