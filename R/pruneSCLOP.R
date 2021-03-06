#' @title Local Pruning State of Topic Dendrograms
#'
#' @description
#' The function \code{\link{SCLOP}} calculates the S-CLOP value for the best possible
#' local pruning state of a dendrogram from \code{\link{dendTopics}}.
#' The function \code{pruneSCLOP} supplies the corresponding pruning state itself.
#'
#' @details
#' For details of computing the S-CLOP values see \code{\link{SCLOP}}.
#'
#' For details and examples of plotting the pruning state see \code{\link{dendTopics}}.
#'
#' @family plot functions
#' @family SCLOP functions
#'
#' @param dend [\code{\link[stats]{dendrogram}}]\cr
#' \code{\link[=dendTopics]{TopicDendrogram}}
#' (and \code{\link[stats]{dendrogram}}) object of all considered topics as the
#' output from \code{\link{dendTopics}}.
#'
#' @return[\code{list of \link[stats]{dendrogram}s}]
#' \code{\link[=pruneSCLOP]{PruningSCLOP}} object specifying the best possible
#' local pruning state.
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
  plot.TopicDendrogram(x = dend, pruning = x, pruning.par = pruning.par, ...)
}

#' @rdname pruneSCLOP
#' @export
pruning.par = function(pruning){
  assert_class(pruning, "PruningSCLOP")
  assert_list(pruning, types = "dendrogram", any.missing = FALSE, min.len = 1)
  list(
    type = "abline",
    lty = 3,
    labels_colors = rep(sample(rainbow_hcl(n = length(pruning))),
      times = lengths(lapply(pruning, labels))),
    labels = unlist(lapply(pruning, labels)))
}

#' @export
print.PruningSCLOP = function(x, ...){
  cat(
    "PruningSCLOP Object consisting of ", length(x), " Topic Cluster\n ",
    "with ", round(mean(sapply(x, function(y) length(labels(y)))), 2), " (SD: ",
    round(sd(sapply(x, function(y) length(labels(y)))), 2),") mean Topics per Cluster\n ",
    "and ", length(unique(unlist(lapply(x, labels_colors)))), " different Colors", sep = "")
}
