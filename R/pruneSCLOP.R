#' @title Local Pruning State of Topic Dendrograms
#'
#' @description
#' The function \code{\link{SCLOP}} calculates the S-CLOP for the best possible
#' local pruning state of a dendrogram from \code{\link{dendTopics}}.
#' The function \code{pruneSCLOP} supplies the corresponding pruning state itself.
#'
#' @details
#'
#' @param dend [\code{\link[stats]{dendrogram}}]\cr
#' \code{\link[=dendTopics]{TopicDendrogram}}
#' (and \code{\link[stats]{dendrogram}}) object of all considered topics as the
#' output from \code{\link{dendTopics}}.
#' @return[\code{list of \link[stats]{dendrogram}s}]
#' \code{\link[=pruneSCLOP]{PruningSCLOP}} object specifying the best possible
#' local pruning state.
#'
#' @examples
#' # TODO
#'
#' @export pruneSCLOP
pruneSCLOP = function(dend) UseMethod("pruneSCLOP")

#' @export
pruneSCLOP.TopicDendrogram = function(dend){
  tmpPruneSCLOPlist = list()
  nruns = length(unique(labels_colors(dend)))
  .pruneSCLOP = function(dend, nruns){
    if(is.leaf(dend)){
      tmpPruneSCLOPlist[[length(tmpPruneSCLOPlist)+1]] <<- dend
    }
    else{
      tab = table(labels_colors(dend))
      tmp = integer(nruns)
      tmp[1:length(tab)] = tab
      tab = tmp
      if(all.equal((mean(abs(tab-1)) * sum(tab)), .disparitySum(dend = dend, nruns = nruns)) == TRUE){
        tmpPruneSCLOPlist[[length(tmpPruneSCLOPlist)+1]] <<- dend
      }
      else{
        Recall(dend = dend[[1]], nruns = nruns)
        Recall(dend = dend[[2]], nruns = nruns)
      }
    }
  }
  .pruneSCLOP(dend = dend, nruns = nruns)
  class(tmpPruneSCLOPlist) = "PruningSCLOP"
  return(tmpPruneSCLOPlist)
}

#' @rdname pruneSCLOP
#' @inheritParams plot.TopicDendrogram
#' @export
plot.PruningSCLOP = function(x, dend, pruning.par, ...){
  if (missing(pruning.par)) pruning.par = list()
  plot.TopicDendrogram(dend = dend, pruning = x, pruning.par = pruning.par, ...)
}

#' @rdname pruneSCLOP
#' @export
pruning.par = function(pruning){
  list(
    type = "abline",
    lty = 3,
    label_colors = rep(sample(rainbow_hcl(n = length(pruning))),
      times = lengths(pruning)),
    labels = unlist(lapply(pruning, labels)))
}
