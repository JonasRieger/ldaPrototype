#' @title Pairwise RBO Similarities
#'
#' @description
#' Calculates the similarity of all pairwise topic combinations using the
#' rank-biased overlap (RBO) Similarity.
#'
#' @details
#' The RBO Similarity for two topics \eqn{\bm z_{i}} and \eqn{\bm z_{j}}
#' is calculated by
#' \deqn{RBO(\bm z_{i}, \bm z_{j} \mid k, p) = 2p^k\frac{\left|Z_{i}^{(k)} \cap Z_{j}^{(k)}\right|}{\left|Z_{i}^{(k)}\right| + \left|Z_{j}^{(k)}\right|} + \frac{1-p}{p} \sum_{d=1}^k 2 p^d\frac{\left|Z_{i}^{(d)} \cap Z_{j}^{(d)}\right|}{\left|Z_{i}^{(d)}\right| + \left|Z_{j}^{(d)}\right|}}
#' with \eqn{Z_{i}^{(d)}} is the vocabulary set of topic \eqn{\bm z_{i}} down to
#' rank \eqn{d}. Ties in ranks are resolved by taking the minimum.
#'
#' The value \code{wordsconsidered} describes the number of words per topic
#' ranked at rank \eqn{k} or above.
#'
#' @references Webber, William, Alistair Moffat and Justin Zobel (2010).
#' "A similarity measure for indefinite rankings".
#' In: \emph{ACM Transations on Information Systems} 28(4), p.20:1–-20:38,
#' DOI 10.1145/1852102.1852106,
#' URL \url{http://doi.acm.org/10.1145/1852102.1852106}
#'
#' @family TopicSimilarity functions
#'
#' @param topics [\code{named matrix}]\cr
#' The counts of vocabularies/words (row wise) in topics (column wise).
#' @param k [\code{integer(1)}]\cr
#' Maximum depth for evaluation. Words down to this rank are considered for the calculation of similarities.
#' @param p [0,1]\cr
#' Weighting parameter. Lower values emphasizes top ranked words while values
#' that go towards 1 correspond to equal weights for each evaluation depth.
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
#'   \code{type} [\code{character(1)}] \code{= "RBO Similarity"},
#'   \code{k} [\code{integer(1)}] and \code{p} [0,1]. See above for explanation.}
#' }
#'
#' @examples
#' res = LDARep(docs = reuters_docs, vocab = reuters_vocab, n = 4, K = 10, num.iterations = 30)
#' topics = mergeTopics(res, vocab = reuters_vocab)
#' rbo = rboTopics(topics, k = 12, p = 0.9)
#' rbo
#'
#' sim = getSimilarity(rbo)
#' dim(sim)
#'
#' @export rboTopics

rboTopics = function(topics, k, p, progress = TRUE, pm.backend, ncpus){
  assert_matrix(topics, mode = "integerish", any.missing = FALSE,
                col.names = "strict", min.cols = 2, min.rows = 2)
  assert_integerish(topics, lower = 0, any.missing = FALSE)
  assert_flag(progress)
  assert_number(p, lower = .Machine$double.eps, upper = 1 - .Machine$double.eps)
  assert_int(k, lower = 0, upper = nrow(topics))

  if (missing(ncpus)) ncpus = NULL
  if (!missing(pm.backend) && !is.null(pm.backend)){
    rboTopics.parallel(topics = topics, k = k, p = p, pm.backend = pm.backend, ncpus = ncpus)
  }else{
    rboTopics.serial(topics = topics, k = k, p = p, progress = progress)
  }
}

rboTopics.parallel = function(topics, k, p, pm.backend, ncpus){
  assert_choice(pm.backend, choices = c("multicore", "socket", "mpi"))
  if (missing(ncpus) || is.null(ncpus)) ncpus = future::availableCores()
  assert_int(ncpus, lower = 1)

  N = ncol(topics)

  if(ncpus > N-2){
    ncpus = N-2
    message("The selected number of cores exceeds the parallelizable complexity of the task, set ncpus to ", ncpus, ".")
  }
  if(ncpus == 1){
    message("There is only one core on the running system or one core selected, falling back to serial version.")
    jsTopics.serial(topics = topics, k = k, p = p)
  }

  ranks = apply(-topics, 2, frank, ties.method = "min") #faster than rank

  parallelMap::parallelStart(mode = pm.backend, cpus = ncpus)

  fun = function(s){
    lapply(s, function(i){
      tmp = do.call(rbind, lapply(1:k, function(d){
        tmp1 = ranks[,i] < d+1
        tmp2 = ranks[,(i+1):N] < d+1
        2 * colSums(tmp1&tmp2) / (sum(tmp1)+colSums(tmp2)) * p^d
        # colSums(tmp1&tmp2) / colSums(tmp1|tmp2) * p^d  ## would be Jaccard instead of overlap
      }))
      tmp[k,] + colSums(tmp) * (1-p) / p
    })
  }

  parallelMap::parallelExport("ranks", "N") # p and k exported by function
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
  tmp = sapply(1:k, function(d){
    tmp1 = ranks[,N-1] < d+1
    tmp2 = ranks[,N] < d+1
    2 * sum(tmp1&tmp2) / (sum(tmp1)+sum(tmp2)) * p^d
    # sum(tmp1&tmp2) / sum(tmp1|tmp2) * p^d ## would be Jaccard instead of overlap
  })
  sims[lower.tri(sims)] = c(unlist(rearrangedlist), tmp[k] + sum(tmp) * (1-p) / p)

  wordsconsidered = colSums(ranks < k+1)
  res = list(sims = sims, wordslimit = wordsconsidered, wordsconsidered = wordsconsidered,
             param = list(type = "RBO Similarity", k = k, p = p))
  class(res) = "TopicSimilarity"
  res
}

rboTopics.serial = function(topics, k, p, progress = TRUE){
  N = ncol(topics)

  ranks = apply(-topics, 2, frank, ties.method = "min") #faster than rank

  sims = matrix(nrow = N, ncol = N)
  colnames(sims) = rownames(sims) = colnames(topics)

  pb = .makeProgressBar(progress = progress,
                        total = N-1, format = "Calculate Similarities [:bar] :percent eta: :eta")
  for(i in seq_len(N - 2)){
    tmp = do.call(rbind, lapply(1:k, function(d){
      tmp1 = ranks[,i] < d+1
      tmp2 = ranks[,(i+1):N] < d+1
      2 * colSums(tmp1&tmp2) / (sum(tmp1)+colSums(tmp2)) * p^d
      # colSums(tmp1&tmp2) / colSums(tmp1|tmp2) * p^d ## would be Jaccard instead of overlap
    }))
    sims[(i+1):N,i] = tmp[k,] + colSums(tmp) * (1-p) / p
    pb$tick()
  }
  tmp = sapply(1:k, function(d){
    tmp1 = ranks[,N-1] < d+1
    tmp2 = ranks[,N] < d+1
    2 * sum(tmp1&tmp2) / (sum(tmp1)+sum(tmp2)) * p^d
    # sum(tmp1&tmp2) / sum(tmp1|tmp2) * p^d ## would be Jaccard instead of overlap
  })
  sims[N, N-1] = tmp[k] + sum(tmp) * (1-p) / p
  pb$tick()

  wordsconsidered = colSums(ranks < k+1)
  res = list(sims = sims, wordslimit = wordsconsidered, wordsconsidered = wordsconsidered,
             param = list(type = "RBO Similarity", k = k, p = p))
  class(res) = "TopicSimilarity"
  res
}
