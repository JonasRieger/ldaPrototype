#' @title Pairwise Jaccard Coefficients
#'
#' @description
#' Calculates the similarity of all pairwise topic combinations using a modified
#' Jaccard Coefficient.
#'
#' @details
#'
#' @param topics [\code{named matrix}]\cr
#' The counts of vocabularies (row wise) in topics (column wise).
#' @param limit.rel [0,1]\cr
#' A relative lower bound limit for which words are taken into account. All words
#' are taken as relevant for a topic that have a count higher than \code{limit.rel}
#' multiplied by the total count of the given topic.
#' @param limit.abs [\code{integer(1)}]\cr
#' An absolute lower bound limit for which words are taken into account. All words
#' are taken as relevant for a topic that have a count higher than \code{limit.abs}.
#' @param progress [\code{logical(1)}]\cr
#' Should a nice progress bar be shown? Turning it off, could lead to significantly
#' faster calculation. Default ist \code{TRUE}.
#' @return [\code{lower triangular named matrix}] with all pairwise jaccard similarities
#' of the given topics.
#'
#' @examples
#' # TODO
#'
#' @export jaccardTopics

jaccardTopics = function(topics, limit.rel = 1/500, limit.abs = 10, progress = TRUE){

  N = ncol(topics)

  index = as.integer(topics > limit.abs &
      topics > rep(colSums(topics)*limit.rel, each = nrow(topics)))
  #index = apply(topics, 2, function(x) x > (sum(x) * limit.rel))
  #index = index & (topics > limit.abs)
  sims = matrix(nrow = N, ncol = N)
  colnames(sims) = rownames(sims) = colnames(topics)

  pb = .makeProgressBar(progress = progress,
    total = N-1, format = "Calculate Similarities [:bar] :percent eta: :eta")
  for(i in seq_len(N - 2)){
    sims[(i+1):N,i] = colSums(index[,i] * index[,(i+1):N]) / colSums((index[,i] + index[,(i+1):N]) > 0)
    pb$tick()
  }
  sims[N, N-1] = sum(index[, N] & index[, N-1]) / sum(index[, N] | index[, N-1])
  pb$tick()

  sims[is.nan(sims)] = 0

  invisible(sims)
}
