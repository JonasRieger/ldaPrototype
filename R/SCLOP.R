#' @title Similarity/Stability of multiple sets of Objects using Clustering with Local Pruning
#'
#' @description
#' The function \code{SCLOP} calculates the S-CLOP for the best possible local pruning state
#' of a dendrogram from \code{\link{dendTopics}}. The function \code{pruneSCLOP}
#' supplies the corresponding pruning state itself.\cr
#' To get all pairwise S-CLOP scores of two LDA runs, the function \code{SCLOP.pairwise}
#' can be used. It returnes a matrix of the pairwise S-CLOP scores.\cr
#' All three functions use the function \code{disparitySum} to calculate the
#' least possible sum of disparities (on the best possible local pruning state)
#' on a given dendrogram.
#'
#' @details
#'
#' @param dend [\code{\link[stats]{dendrogram}}]\cr
#' Output from \code{\link{dendTopics}}.
#' @return
#' \describe{
#'   \item{\code{SCLOP}}{[0,1] value specifying the S-CLOP for the best possible
#'   local pruning state of the given dendrogram.}
#'   \item{\code{pruneSCLOP}}{[\code{list of \link[stats]{dendrogram}s}] specifying
#'   the best possible local pruning state.}
#'   \item{\code{disparitySum}}{[\code{numeric(1)}] value specifying the least
#'   possible sum of disparities on the given dendrogram.}
#'   \item{\code{SCLOP.pairwise}}{[\code{symmetrical named matrix}] with all
#'   pairwise S-CLOP scores of the given LDA runs.}
#' }
#'
#' @examples
#' # TODO
#'
#' @export SCLOP

SCLOP = function(dend){
  nruns = length(unique(dendextend::labels_colors(dend)))
  return(1 - (nruns/(nruns-1)) * disparitySum(dend) / stats::nobs(dend))
}

#' @rdname SCLOP
#' @export pruneSCLOP

pruneSCLOP = function(dend){
  tmpPruneSCLOPlist = list()
  nruns = length(unique(dendextend::labels_colors(dend)))
  .pruneSCLOP = function(dend, nruns){
    if(stats::is.leaf(dend)){
      tmpPruneSCLOPlist[[length(tmpPruneSCLOPlist)+1]] <<- dend
    }
    else{
      tab = table(dendextend::labels_colors(dend))
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
  return(tmpPruneSCLOPlist)
}

#' @rdname SCLOP
#' @export disparitySum

disparitySum = function(dend){
  .disparitySum(dend = dend, nruns = length(unique(dendextend::labels_colors(dend))))
}

.disparitySum = function(dend, nruns){
  if(stats::is.leaf(dend)) return((nruns-1)/nruns)
  tab = table(dendextend::labels_colors(dend))
  tmp = integer(nruns)
  tmp[1:length(tab)] = tab
  tab = tmp
  return(min(mean(abs(tab-1)) * sum(tab),
    Recall(dend[[1]], nruns) + Recall(dend[[2]], nruns)))
}

#' @rdname SCLOP
#' @param sims [\code{lower triangular named matrix}]\cr
#' Pairwise jaccard similarities of underlying topics as
#' the output from \code{\link{jaccardTopics}}. The topic names should be
#' formatted as <\emph{Run X}>.<\emph{Topic Y}>, so that the name before the
#' first dot identifies the LDA run.
#' @export SCLOP.pairwise

SCLOP.pairwise = function(sims){
  names = paste0(unique(sapply(strsplit(colnames(sims), "\\."), function(x) x[1])), "\\.")

  combs = utils::combn(names, 2)
  rownames(combs) = c("V1", "V2")
  vals = apply(combs, 2, function(x) SCLOP(dendTopics(sims = sims, ind = x)))
  dat = data.frame(t(combs))
  dat$SCLOP = vals

  mat = matrix(ncol = length(names), nrow = length(names))
  k = 1
  i = match(dat$V2, names)
  j = match(dat$V1, names)
  for(k in seq_len(nrow(dat))){
    mat[i[k], j[k]] = dat$SCLOP[k]
    mat[j[k], i[k]] = dat$SCLOP[k]
  }
  colnames(mat) = rownames(mat) = names
  return(mat)
}
