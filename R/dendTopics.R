#' @title Topic Dendrogram
#'
#' @description
#' Builds a dendrogram for topics based on their pairwise similarities using the
#' cluster algorithm \code{\link[stats]{hclust}}.
#'
#' @details
#' The label´s colors are determined based on their Run belonging using
#' \code{\link[colorspace]{rainbow_hcl}} by default. Colors can be manipulated
#' using \code{\link[dendextend]{labels_colors}}. Analogously, the labels
#' themself can be  manipulated using \code{\link[dendextend:labels.hclust]{labels}}.
#' For both the function \code{\link[stats]{order.dendrogram}} is useful.
#'
#' The resulting \code{\link[stats]{dendrogram}} can be plotted. In addition,
#' it is possible to mark a pruning state in the plot, either by color or by
#' separator lines (or both) setting \code{pruning.par}. For the default values
#' of \code{pruning.par} call the corresponding function on any
#' \code{\link[=pruneSCLOP]{PruningSCLOP}} object.
#'
#' @family plot functions
#' @family TopicSimilarity functions
#' @family workflow functions
#'
#' @param sims [\code{\link[=jaccardTopics]{TopicSimilarity}} object
#' or \code{lower triangular named matrix}]\cr
#' \code{\link[=jaccardTopics]{TopicSimilarity}} object or
#' pairwise jaccard similarities of underlying topics as the \code{sims} element
#' from \code{\link[=jaccardTopics]{TopicSimilarity}} objects. The topic names should be
#' formatted as <\emph{Run X}>.<\emph{Topic Y}>, so that the name before the
#' first dot identifies the LDA run.
#' @param ind [\code{integer} or \code{character}]\cr
#' An integerish vector for specifying the topics taken into account. Alternatively
#' a character vector can be passed. Then, all topics are taken for which the name
#' contain at least one of the phrases in \code{ind} (see \code{\link[=grep]{grepl}}).
#' By default all topics are considered.
#' @param method [\code{character(1)}]\cr
#' The agglomeration method. See \code{\link[stats]{hclust}}.
#'
#' @return [\code{\link[stats]{dendrogram}}] \code{\link[=dendTopics]{TopicDendrogram}} object
#' (and \code{\link[stats]{dendrogram}} object) of all considered topics.
#'
#' @examples
#' res = LDARep(docs = reuters_docs, vocab = reuters_vocab, n = 4, K = 10, num.iterations = 30)
#' topics = mergeTopics(res, vocab = reuters_vocab)
#' jacc = jaccardTopics(topics, atLeast = 2)
#' sim = getSimilarity(jacc)
#'
#' dend = dendTopics(jacc)
#' dend2 = dendTopics(sim)
#'
#' \donttest{
#' plot(dend)
#' plot(dendTopics(jacc, ind = c("Rep2", "Rep3")))
#' }
#'
#' pruned = pruneSCLOP(dend)
#' \donttest{
#' plot(dend, pruning = pruned)
#' plot(dend, pruning = pruned, pruning.par = list(type = "color"))
#' plot(dend, pruning = pruned, pruning.par = list(type = "both", lty = 1, lwd = 2, col = "red"))
#'
#' dend2 = dendTopics(jacc, ind = c("Rep2", "Rep3"))
#' plot(dend2, pruning = pruneSCLOP(dend2), pruning.par = list(lwd = 2, col = "darkgrey"))
#' }
#'
#' @export dendTopics
dendTopics = function(sims, ind, method = "complete") UseMethod("dendTopics")

#' @export
dendTopics.TopicSimilarity = function(sims, ind, method = "complete"){
  sims = getSimilarity(sims)
  NextMethod("dendTopics")
}

#' @export
dendTopics.default = function(sims, ind, method = "complete"){
  if (missing(ind)) ind = seq_len(ncol(sims))
  if (is.character(ind)) ind = rowSums(sapply(ind, grepl, x = colnames(sims))) > 0

  dend = as.dendrogram(hclust(as.dist(1 - sims[ind, ind]), method = method))

  runs = gsub(pattern = "\\.(.?)*", x = colnames(sims)[ind], replacement = "")
  runs = table(runs)
  cols = rep(rainbow_hcl(n = length(runs)), times = runs)
  labels_colors(dend) = cols[order.dendrogram(dend)]
  class(dend) = c("TopicDendrogram", class(dend))
  invisible(dend)
}

#' @rdname dendTopics
#' @param x an R object.
#' @param pruning [\code{list of \link[stats]{dendrogram}s}]\cr
#' \code{\link[=pruneSCLOP]{PruningSCLOP}} object specifying the best possible
#' local pruning state.
#' @param pruning.par [\code{list}]\cr
#' List of parameters to mark the pruning. See section "Details" at \code{\link{dendTopics}}
#' for default parameters. Types for marking the pruning state are \code{"abline"},
#' \code{"color"} and \code{"both"}.
#' @param ... additional arguments.
#' @export
plot.TopicDendrogram = function(x, pruning, pruning.par, ...){
  dend = x
  class(dend) = class(dend)[-1]
  if (missing(pruning)){
    plot(dend, ...)
  }else{
    if (missing(pruning.par)) pruning.par = list()
    default = pruning.par(pruning)
    pruning.par = c(pruning.par, default[!names(default) %in% names(pruning.par)])
    if (pruning.par$type[1] %in% c("color", "both")){
      labels_colors(dend) = pruning.par$labels_colors
      labels(dend) = pruning.par$labels
    }
    plot(dend, ...)
    if (pruning.par$type[1] %in% c("abline", "both")){
      marks = head(cumsum(lengths(lapply(pruning, labels))), -1)
      pruning.par[c("type", "labels", "labels_colors")] = NULL
      pruning.par$v = marks+0.5
      do.call(abline, pruning.par)
    }
  }
}
