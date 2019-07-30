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
#' A relative lower bound limit for which words are taken into account. Those words
#' are taken as relevant for a topic that have a count higher than \code{limit.rel}
#' multiplied by the total count of the given topic. Default is \code{1/500}.
#' @param limit.abs [\code{integer(1)}]\cr
#' An absolute lower bound limit for which words are taken into account. All words
#' are taken as relevant for a topic that have a count higher than \code{limit.abs}.
#' Default is \code{10}.
#' @param atLeast [\code{integer(1)}]\cr
#' An absolute count of how many words are at least considered as relevant for a topic.
#' Default is \code{0}.
#' @param progress [\code{logical(1)}]\cr
#' Should a nice progress bar be shown? Turning it off, could lead to significantly
#' faster calculation. Default is \code{TRUE}.
#' @return [\code{named list}] with entries
#' \describe{
#'   \item{\code{sims}}{[\code{lower triangular named matrix}] with all pairwise
#'   jaccard similarities of the given topics.}
#'   \item{\code{wordslimit}}{[\code{integer}] with counts of words determined as
#'   relevant based on \code{limit.rel} and \code{limit.abs}.}
#'   \item{\code{wordsconsidered}}{[\code{integer}] with counts of considered
#'   words for similarity calculation. Could differ from \code{wordslimit}, if
#'   \code{atLeast} is greater than zero.}
#'   \item{\code{param}}{[\code{named list}] with parameter specifications for
#'   \code{limit.rel} [0,1], \code{limit.abs} [\code{integer(1)}] and
#'   \code{atLeast} [\code{integer(1)}] See above for explanation.}
#' }
#'
#' @examples
#' # TODO
#'
#' @export jaccardTopics

jaccardTopics = function(topics, limit.rel, limit.abs, atLeast, progress = TRUE){

  if (missing(limit.rel)) limit.rel = .defaultLimit.rel()
  if (missing(limit.abs)) limit.abs = .defaultLimit.abs()
  if (missing(atLeast)) atLeast = .defaultAtLeast()

  N = ncol(topics)

  index = topics > limit.abs &
      topics > rep(colSums(topics)*limit.rel, each = nrow(topics))
  wordsconsidered = colSums(index)
  ind = wordsconsidered < atLeast
  if (any(ind)){
    index[,ind] = apply(as.matrix(topics[,ind]), 2,
      function(x) x >= -sort.int(-x, partial = atLeast)[atLeast])
  }

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

  res = list(sims = sims, wordslimit = wordsconsidered, wordsconsidered = colSums(index),
    param = list(limit.rel = limit.rel, limit.abs = limit.abs, atLeast = atLeast))
  class(res) = "TopicSimilarity"
  invisible(res)
}

#' @export
print.TopicSimilarity = function(x, ...){
  elements = paste0("\"", names(which(!sapply(x, is.null))), "\"")
  cat(
    "TopicSimilarity Object with element(s)\n",
    paste0(elements, collapse = ", "), "\n ",
    nrow(getSimilarity(x)), " Topics from ",
    length(unique(sapply(strsplit(colnames(getSimilarity(x)), "\\."), function(x) x[1]))),
    " independent runs\n ",
    paste0(paste0(names(getParam(x)), ": ", unlist(getParam(x))), collapse = ", "),
    "\n\n", sep = ""
  )
}
