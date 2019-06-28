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
#' @param id .
#' @param limit.rel .
#' @param limit.abs .
#' @param progress [\code{logical(1)}]\cr
#' Should a nice progress bar be shown? Turning it off, could lead to significantly
#' faster calculation. Default ist \code{TRUE}.
#' @param keepTopics .
#' @param keepSims .
#' @param sclop .
#' @param ... additional arguments
#' @return [\code{named list}] with entries ...

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
  }
  if (!keepTopics) topics = NULL
  if (!keepSims) sims = NULL
  ind = which.max(colSums(sclop, na.rm = TRUE))
  proto = lda[[ind]]
  lda.id = names(lda)[ind]
  res = list(lda = proto, lda.id = lda.id, id = id,
    limit.rel = limit.rel, limit.abs = limit.abs,
    topics = topics, sims = sims, sclop = sclop)
  class(res) = "PrototypeLDA"
  invisible(res)
}
