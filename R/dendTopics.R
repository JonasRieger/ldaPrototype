#' @title Topic Dendrogram
#'
#' @description
#' Builds a dendrogram for topics based on their pairwise similarities using a
#' cluster algorithm.
#'
#' @details
#' The label´s colors are determined based on their Run belonging using
#' \code{\link[colorspace]{rainbow_hcl}} by default. Colors can be manipulated
#' using \code{\link[dendextend]{color_labels}}. Analogously, the labels
#' themself can be  manipulated using \code{\link[dendextend]{labels}}. For both
#' the function \code{\link[dendextend]{order.dendrogram}} is useful.
#'
#' @param sims [\code{lower triangular named matrix}]\cr
#' Pairwise jaccard similarities of underlying topics as the \code{sims} element
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
#' @return [\code{\link[stats]{dendrogram}}] \code{\link[=dendTopics]{TopicDendrogram}}
#' (and \code{\link[stats]{dendrogram}}) object of all considered topics.
#'
#' @examples
#' # TODO
#'
#' @export dendTopics

dendTopics = function(sims, ind, method = "complete"){
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
#' @param pruning [\code{list of \link[stats]{dendrogram}s}]\cr
#' \code{\link[=pruneSCLOP]{PruningSCLOP}} object specifying the best possible
#' local pruning state.
#' @param pruning.par [\code{list}]\cr
#' List of parameters to mark the pruning. See \code{\link[=pruneSCLOP]{pruning.par}}
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
      labels_colors(dend) = pruning.par$label_colors
      labels(dend) = pruning.par$labels
    }
    plot(dend, ...)
    if (pruning.par$type[1] %in% c("abline", "both")){
      marks = cumsum(lengths(lapply(pruning, labels)))
      pruning.par[c("type", "labels", "label_colors")] = NULL
      pruning.par$v = marks+0.5
      do.call(abline, pruning.par)
    }
  }
}
