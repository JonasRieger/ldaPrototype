#' @title Determine the Prototype LDA
#'
#' @description Returns the Prototype LDA of a set of LDAs, which is given as
#' \code{\link{LDABatch}} object, \code{\link{LDARep}} object, or as list of LDAs.
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
#' Not implemented for \code{\link{LDABatch}} or \code{\link{LDARep}} objects.
#' @param limit.rel [0,1]\cr
#' See \code{\link{jaccardTopics}}. Default is \code{1/500}.
#' Not considered for calculation, if \code{sclop} is passed. But should be
#' passed determining the correct value for the resulting object.
#' @param limit.abs [\code{integer(1)}]\cr
#' See \code{\link{jaccardTopics}}. Default is \code{10}.
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
#' @param sclop [\code{symmetrical named matrix}]\cr
#' All pairwise S-CLOP scores of the given LDA runs. Matching of names is not
#' implemented yet, so order matters.
#' @param ... additional arguments
#' @return [\code{named list}] with entries
#'  \describe{
#'   \item{\code{lda}}{\code{\link{LDA}} object.}
#'   \item{\code{ldaid}}{[\code{character(1)}] Name (ID) of the selected Prototype LDA.}
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
getPrototype.LDABatch = function(x, vocab, limit.rel, limit.abs, progress = TRUE,
  keepTopics = FALSE, keepSims = FALSE, ...){

  if (missing(limit.rel)) limit.rel = .defaultLimit.rel()
  if (missing(limit.abs)) limit.abs = .defaultLimit.abs()
  lda = getLDA(x)
  id = getID(x)
  NextMethod("getPrototype", lda = lda, vocab = vocab, id = id,
    limit.rel = limit.rel, limit.abs = limit.abs,
    keepTopics = keepTopics, keepSims = keepSims, progress = progress)
}

#' @rdname getPrototype
#' @export
getPrototype.LDARep = function(x, vocab, limit.rel, limit.abs, progress = TRUE,
  keepTopics = FALSE, keepSims = FALSE, ...){

  if (missing(limit.rel)) limit.rel = .defaultLimit.rel()
  if (missing(limit.abs)) limit.abs = .defaultLimit.abs()
  lda = getLDA(x)
  id = getID(x)
  NextMethod("getPrototype", lda = lda, vocab = vocab, id = id,
    limit.rel = limit.rel, limit.abs = limit.abs,
    keepTopics = keepTopics, keepSims = keepSims, progress = progress)
}

#' @rdname getPrototype
#' @export
getPrototype.default = function(lda, vocab, id, limit.rel, limit.abs, progress = TRUE,
  keepTopics = FALSE, keepSims = FALSE, sclop, ...){

  if (missing(limit.rel)) limit.rel = .defaultLimit.rel()
  if (missing(limit.abs)) limit.abs = .defaultLimit.abs()
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
  ind = which.max(colSums(sclop, na.rm = TRUE))
  proto = lda[[ind]]
  ldaid = names(lda)[ind]
  res = list(lda = proto, ldaid = ldaid, id = id,
    limit.rel = limit.rel, limit.abs = limit.abs,
    topics = topics, sims = sims, sclop = sclop)
  class(res) = "PrototypeLDA"
  invisible(res)
}

#' @export
print.PrototypeLDA = function(x, ...){

  add = ""
  if (!is.null(getMergedTopics(x))){
    add = " (including merged topics matrix)"
    if (!is.null(getSimilarity(x))){
      add = " (including merged topics matrix and similarity matrix)"
    }
  }else{
    if (!is.null(getSimilarity(x))) add = " (including similarity matrix)"
  }

  cat("PrototypeLDA Object", add, "\n LDA \"", getLDAID(x), "\" of \"", x$id, "\"\n ",
    paste0(paste0(names(getParam(getLDA(x))), ": ",
      round(unlist(getParam(getLDA(x))), 2)), collapse = ", "),
    "\n ", paste0(paste0(names(getParam(x)), ": ",
      round(unlist(getParam(x)), 2)), collapse = ", "),
    "\n\n", sep = "")
}
