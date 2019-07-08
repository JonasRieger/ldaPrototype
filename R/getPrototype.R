#' @title Determine the Prototype LDA
#'
#' @description Returns the Prototype LDA of a set of LDAs. This set is given as
#' \code{\link{LDABatch}} object, \code{\link{LDARep}} object, or as list of LDAs.
#' If the matrix of S-CLOP scores \code{sclop} is passed, no calculation is done.
#'
#' @param x [\code{named list}]\cr
#' \code{\link{LDABatch}} or \code{\link{LDARep}} object.
#' @param lda [\code{named list}]\cr
#' List of \code{\link{LDA}} objects, named by the corresponding "job.id".
#' @param vocab [\code{character}]\cr
#' Vocabularies taken into consideration for merging topic matrices.
#' Not considered, if \code{sclop} is passed.
#' @param id [\code{character(1)}]\cr
#' A name for the computation. If not passed, it is set to "LDARep".
#' Not considered for \code{\link{LDABatch}} or \code{\link{LDARep}} objects.
#' @param limit.rel [0,1]\cr
#' See \code{\link{jaccardTopics}}. Default is \code{1/500}.
#' Not considered for calculation, if \code{sclop} is passed. But should be
#' passed determining the correct value for the resulting object.
#' @param limit.abs [\code{integer(1)}]\cr
#' See \code{\link{jaccardTopics}}. Default is \code{10}.
#' Not considered for calculation, if \code{sclop} is passed. But should be
#' passed determining the correct value for the resulting object.
#' @param atLeast [\code{integer(1)}]\cr
#' See \code{\link{jaccardTopics}}. Default is \code{0}.
#' Not considered for calculation, if \code{sclop} is passed. But should be
#' passed determining the correct value for the resulting object.
#' @param progress [\code{logical(1)}]\cr
#' Should a nice progress bar be shown for the steps of \code{\link{mergeTopics}}
#' and \code{\link{jaccardTopics}}? Turning it off, could lead to significantly
#' faster calculation. Default ist \code{TRUE}.
#' Not considered, if \code{sclop} is passed.
#' @param keepTopics [\code{logical(1)}]\cr
#' Should the merged topic matrix from \code{\link{mergeTopics}} be kept?
#' Not considered, if \code{sclop} is passed.
#' @param keepSims [\code{logical(1)}]\cr
#' Should the calculated topic similarities matrix from \code{\link{jaccardTopics}}
#' be kept? Not considered, if \code{sclop} is passed.
#' @param keepLDAs [\code{logical(1)}]\cr
#' Should the considered LDAs be kept?
#' @param sclop [\code{symmetrical named matrix}]\cr
#' (optional) All pairwise S-CLOP scores of the given LDA runs determined by
#' \code{\link{SCLOP.pairwise}}. Matching of names is not implemented yet, so order matters.
#' @param ... additional arguments
#' @return [\code{named list}] with entries
#'  \describe{
#'   \item{\code{lda}}{List of \code{\link{LDA}} objects of the determined Prototype LDA
#'   and - if \code{keepLDAs} is \code{TRUE} - all considered LDAs.}
#'   \item{\code{protoid}}{[\code{character(1)}] Name (ID) of the determined Prototype LDA.}
#'   \item{\code{id}}{[\code{character(1)}] See above.}
#'   \item{\code{limit.rel}}{[0,1] See above.}
#'   \item{\code{limit.abs}}{[\code{integer(1)}] See above.}
#'   \item{\code{topics}}{ [\code{named matrix}] with the count of vocabularies
#'   (row wise) in topics (column wise).}
#'   \item{\code{sims}}{[\code{lower triangular named matrix}] with all pairwise
#'   jaccard similarities of the given topics.}
#'   \item{\code{sclop}}{[\code{symmetrical named matrix}] with all pairwise
#'   S-CLOP scores of the given LDA runs.}
#' }

#' @export getPrototype
getPrototype = function(...) UseMethod("getPrototype")

#' @rdname getPrototype
#' @export
getPrototype.LDABatch = function(x, vocab, limit.rel, limit.abs, atLeast, progress = TRUE,
  keepTopics = FALSE, keepSims = FALSE, keepLDAs = FALSE, ...){

  if (missing(limit.rel)) limit.rel = .defaultLimit.rel()
  if (missing(limit.abs)) limit.abs = .defaultLimit.abs()
  if (missing(atLeast)) atLeast = .defaultAtLeast()
  lda = getLDA(x)
  id = getID(x)
  NextMethod("getPrototype", lda = lda, vocab = vocab, id = id,
    limit.rel = limit.rel, limit.abs = limit.abs, atLeast = atLeast,
    progress = progress,
    keepTopics = keepTopics, keepSims = keepSims, keepLDAs= keepLDAs)
}

#' @rdname getPrototype
#' @export
getPrototype.LDARep = function(x, vocab, limit.rel, limit.abs, progress = TRUE,
  keepTopics = FALSE, keepSims = FALSE, keepLDAs = FALSE, ...){

  if (missing(limit.rel)) limit.rel = .defaultLimit.rel()
  if (missing(limit.abs)) limit.abs = .defaultLimit.abs()
  if (missing(atLeast)) atLeast = .defaultAtLeast()
  lda = getLDA(x)
  id = getID(x)
  NextMethod("getPrototype", lda = lda, vocab = vocab, id = id,
    limit.rel = limit.rel, limit.abs = limit.abs, atLeast = atLeast,
    progress = progress,
    keepTopics = keepTopics, keepSims = keepSims, keepLDAs= keepLDAs)
}

#' @rdname getPrototype
#' @export
getPrototype.default = function(lda, vocab, id, limit.rel, limit.abs, progress = TRUE,
  keepTopics = FALSE, keepSims = FALSE, keepLDAs = FALSE, sclop, ...){

  if (missing(limit.rel)) limit.rel = .defaultLimit.rel()
  if (missing(limit.abs)) limit.abs = .defaultLimit.abs()
  if (missing(atLeast)) atLeast = .defaultAtLeast()
  if (missing(id)) id = "LDARep"
  if (missing(sclop)){
    topics = mergeRepTopics(lda = lda, vocab = vocab, id = id, progress = progress)
    sims = jaccardTopics(topics = topics, limit.rel = limit.rel, limit.abs = limit.abs,
      progress = progress)
    sclop = SCLOP.pairwise(sims = sims)
    if (!keepTopics) topics = NULL
    if (!keepSims) sims = NULL
  }else{
    topics = NULL
    sims = NULL
  }
  protoid = names(lda)[which.max(colSums(sclop, na.rm = TRUE))]
  if (!keepLDAs) lda = lda[which.max(colSums(sclop, na.rm = TRUE))]
  res = list(lda = lda, protoid = protoid, id = id,
    param = list(limit.rel = limit.rel, limit.abs = limit.abs, atLeast = atLeast),
    topics = topics, sims = sims, sclop = sclop)
  class(res) = "PrototypeLDA"
  invisible(res)
}

#' @export
print.PrototypeLDA = function(x, ...){

  n = length(getLDA(x, all = TRUE))
  elements = paste0("\"", names(which(!sapply(x, is.null))), "\"")
  cat("PrototypeLDA Object with elements\n", paste0(elements, collapse = ", "),
    "\n Prototype of computation \"", getID(x), "\"",
    ifelse(n > 1, paste0(" consisting of ", n, " independent runs"), ""),
    " is LDA \"", getPrototypeID(x), "\"\n ",
    paste0(paste0(names(getParam(getLDA(x))), ": ",
      round(unlist(getParam(getLDA(x))), 2)), collapse = ", "),
    "\n ", paste0(paste0(names(getParam(x)), ": ",
      unlist(getParam(x))), collapse = ", "),
    "\n\n", sep = "")
}
