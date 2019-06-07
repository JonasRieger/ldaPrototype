#' @title Topic Dendrogram
#'
#' @description
#' Builds a dendrogram for topics based on their pairwise similarities using a
#' cluster algorithm.
#'
#' @details
#'
#' @param sims [\code{symmetrical named matrix}]\cr
#' Pairwise jaccard similarities of underlying topics like
#' the output from \code{\link{jaccardTopics}}. The topic names should be
#' formatted as <\emph{Run X}>.<\emph{Topic Y}>, so that the name before the
#' first dot identifies the LDA run.
#' @param ind [\code{integer} or \code{character}]\cr
#' An integerish vector for specifying the topics taken into account. Alternatively
#' a character vector can be passed. Then, all topics are taken for which the name
#' contain at least one of the phrases in \code{ind} (see \code{\link[base]{grepl}}).
#' @param method [\code{character(1)}]\cr
#' The agglomeration method. See \code{\link[stats]{hclust}}.
#' @return [\code{\link[stats]{dendrogram}}] of all considered topics.
#'
#' @examples
#' # TODO
#'
#' @export dendTopics

dendTopics = function(sims, ind, method = "complete"){

  if (missing(ind)) ind = seq_len(ncol(sims))
  if (is.character(ind)) ind = rowSums(sapply(ind, grepl, x = colnames(sims))) > 0

  dend = stats::as.dendrogram(stats::hclust(stats::as.dist(1 - sims[ind, ind]), method = method))

  runs = gsub(pattern = "\\.(.?)*", x = colnames(sims)[ind], replacement = "")
  runs = table(runs)
  cols = rep(grDevices::grey(seq(0, 0.8, length.out = length(runs))), times = runs)
  dendextend::labels_colors(dend) = cols[stats::order.dendrogram(dend)]
  invisible(dend)
}
