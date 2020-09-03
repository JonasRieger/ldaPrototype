#' @title Similarity/Stability of multiple sets of Objects using Clustering with Local Pruning
#'
#' @description
#' The function \code{SCLOP} calculates the S-CLOP value for the best possible
#' local pruning state of a dendrogram from \code{\link{dendTopics}}.
#' The function \code{\link{pruneSCLOP}} supplies the corresponding pruning state itself.\cr
#' To get all pairwise S-CLOP scores of two LDA runs, the function \code{SCLOP.pairwise}
#' can be used. It returns a matrix of the pairwise S-CLOP scores.\cr
#' All three functions use the function \code{disparitySum} to calculate the
#' least possible sum of disparities (on the best possible local pruning state)
#' on a given dendrogram.
#'
#' @details
#' For one specific cluster \eqn{g} and \eqn{R} LDA Runs the disparity is calculated by
#' \deqn{U(g) := \frac{1}{R} \sum_{r=1}^R \vert t_r^{(g)} - 1 \vert \cdot \sum_{r=1}^R t_r^{(g)},}
#' while \eqn{\bm t^{(g)} = (t_1^{(g)}, ..., t_R^{(g)})^T}
#' contains the number of topics that belong to the different LDA runs and that
#' occur in cluster \eqn{g}.
#'
#' The function \code{disparitySum} returns the least possible sum of disparities
#' \eqn{U_{\Sigma}(G^*)} for the best possible pruning state \eqn{G^*}
#' with \eqn{U_{\Sigma}(G) = \sum_{g \in G} U(g) \to \min}.
#' The highest possible value for \eqn{U_{\Sigma}(G^*)} is limited by
#' \deqn{U_{\Sigma,\textsf{max}} := \sum_{g \in \tilde{G}} U(g) = N \cdot \frac{R-1}{R},}
#' with \eqn{\tilde{G}} denotes the corresponding worst case pruning state. This worst
#' case scenario is useful for normalizing the SCLOP scores.
#'
#' The function \code{SCLOP} then calculates the value
#' \deqn{\textsf{S-CLOP}(G^*) := 1 - \frac{1}{U_{\Sigma,\textsf{max}}} \cdot \sum_{g \in G^*} U(g) ~\in [0,1],}
#' where \eqn{\sum\limits_{g \in G^*} U(g) = U_{\Sigma}(G^*)}.
#'
#' @family SCLOP functions
#' @family workflow functions
#'
#' @param dend [\code{\link[stats]{dendrogram}}]\cr
#' Output from \code{\link{dendTopics}}.
#'
#' @return
#' \describe{
#'   \item{\code{SCLOP}}{[0,1] value specifying the S-CLOP for the best possible
#'   local pruning state of the given dendrogram.}
#'   \item{\code{disparitySum}}{[\code{numeric(1)}] value specifying the least
#'   possible sum of disparities on the given dendrogram.}
#'   \item{\code{SCLOP.pairwise}}{[\code{symmetrical named matrix}] with all
#'   pairwise S-CLOP scores of the given LDA runs.}
#' }
#'
#' @examples
#' res = LDARep(docs = reuters_docs, vocab = reuters_vocab, n = 4, K = 10, num.iterations = 30)
#' topics = mergeTopics(res, vocab = reuters_vocab)
#' jacc = jaccardTopics(topics, atLeast = 2)
#' dend = dendTopics(jacc)
#'
#' SCLOP(dend)
#' disparitySum(dend)
#'
#' SCLOP.pairwise(jacc)
#' SCLOP.pairwise(getSimilarity(jacc))
#'
#' @export SCLOP

SCLOP = function(dend){
  nruns = length(unique(labels_colors(dend)))
  return(1 - (nruns/(nruns-1)) * disparitySum(dend) / nobs(dend))
}

#' @rdname SCLOP
#' @export disparitySum

disparitySum = function(dend){
  assert_class(dend, c("TopicDendrogram", "dendrogram"))
  .disparitySum(dend = dend, nruns = length(unique(labels_colors(dend))))
}

.disparitySum = function(dend, nruns){
  if(is.leaf(dend)) return((nruns-1)/nruns)
  tab = table(labels_colors(dend))
  tmp = integer(nruns)
  tmp[1:length(tab)] = tab
  tab = tmp
  return(min(mean(abs(tab-1)) * sum(tab),
    Recall(dend[[1]], nruns) + Recall(dend[[2]], nruns)))
}

#' @rdname SCLOP
#' @param sims [\code{\link[=jaccardTopics]{TopicSimilarity}} object
#' or \code{lower triangular named matrix}]\cr
#' \code{\link[=jaccardTopics]{TopicSimilarity}} object or
#' pairwise jaccard similarities of underlying topics as the \code{sims} element
#' from \code{\link[=jaccardTopics]{TopicSimilarity}} objects. The topic names should be
#' formatted as <\emph{Run X}>.<\emph{Topic Y}>, so that the name before the
#' first dot identifies the LDA run.
#' @param pm.backend [\code{character(1)}]\cr
#' One of "multicore", "socket" or "mpi".
#' If \code{pm.backend} is set, \code{\link[parallelMap]{parallelStart}} is
#' called before computation is started and \code{\link[parallelMap]{parallelStop}}
#' is called after.
#' @param ncpus [\code{integer(1)}]\cr
#' Number of (physical) CPUs to use. If \code{pm.backend} is passed,
#' default is determined by \code{\link[future]{availableCores}}.
#' @export SCLOP.pairwise
SCLOP.pairwise = function(sims, pm.backend, ncpus) UseMethod("SCLOP.pairwise")

#' @export
SCLOP.pairwise.TopicSimilarity = function(sims, pm.backend, ncpus){
  sims = getSimilarity(sims)
  NextMethod("SCLOP.pairwise")
}

#' @export
SCLOP.pairwise.default = function(sims, pm.backend, ncpus){
  assert_matrix(sims, mode = "numeric", all.missing = FALSE, nrows = ncol(sims), row.names = "strict", min.cols = 2)
  assert_numeric(sims[lower.tri(sims)], lower = 0, upper = 1, any.missing = FALSE)
  assert_true(all(colnames(sims) == row.names(sims)))
  assert_true(all(grepl("\\.", colnames(sims))))

  if (missing(ncpus)) ncpus = NULL
  if (!missing(pm.backend) && !is.null(pm.backend)){
    SCLOP.pairwise.parallel(sims = sims, pm.backend = pm.backend, ncpus = ncpus)
  }else{
    SCLOP.pairwise.serial(sims = sims)
  }
}

SCLOP.pairwise.serial = function(sims){
  names = unique(sapply(strsplit(colnames(sims), "\\."), function(x) x[1]))

  combs = combn(names, 2)
  vals = apply(combs, 2, function(x) SCLOP(dendTopics(sims = sims, ind = x)))

  mat = matrix(ncol = length(names), nrow = length(names))
  i = match(combs[2,], names)
  j = match(combs[1,], names)
  for(k in seq_len(length(vals))){
    mat[i[k], j[k]] = vals[k]
    mat[j[k], i[k]] = vals[k]
  }
  colnames(mat) = rownames(mat) = names
  return(mat)
}

SCLOP.pairwise.parallel = function(sims, pm.backend, ncpus){
  assert_choice(pm.backend, choices = c("multicore", "socket", "mpi"))
  if (missing(ncpus) || is.null(ncpus)) ncpus = future::availableCores()
  assert_int(ncpus, lower = 1)

  names = unique(sapply(strsplit(colnames(sims), "\\."), function(x) x[1]))

  combs = combn(names, 2)
  parallelMap::parallelStart(mode = pm.backend, cpus = ncpus)
  vals = parallelMap::parallelMap(fun = function(x) SCLOP(dendTopics(sims = sims, ind = x)),
                                 split(combs, rep(seq_len(ncol(combs)), each = 2)))
  parallelMap::parallelStop()
  vals = unlist(vals)

  mat = matrix(ncol = length(names), nrow = length(names))
  i = match(combs[2,], names)
  j = match(combs[1,], names)
  for(k in seq_len(length(vals))){
    mat[i[k], j[k]] = vals[k]
    mat[j[k], i[k]] = vals[k]
  }
  colnames(mat) = rownames(mat) = names
  return(mat)
}
