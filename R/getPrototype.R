#' @title ...
#'
#' @description ...
#'
#' @param x [\code{named list}]\cr
#' \code{\link{LDABatch}} or \code{\link{LDARep}} object.
#' @param lda [\code{named list}]\cr
#' List of \code{\link{LDA}} objects, named by the corresponding "job.id".
#' @param vocab .
#' @param limit.rel .
#' @param limit.abs .
#' @param progress .
#' @param keepTopics .
#' @param keepSims .
#' @param sclop .

#' @export getPrototype
getPrototype = function(...) UseMethod("getPrototype")

#' @export
getPrototype.LDABatch = function(x, vocab, limit.rel, limit.abs, progress = TRUE,
  keepTopics = FALSE, keepSims = FALSE, ...){

  if (missing(limit.rel)) limit.rel = .defaultLimit.rel()
  if (missing(limit.abs)) limit.abs = .defaultLimit.abs()
  lda = getLDA(x)
  id = getID(x)
  NextMethod("getPrototype", lda = lda, vocab = vocab, id = id,
    limit.rel = limt.rel, limit.abs = limit.abs,
    keepTopics = keepTopics, keepSims = keepSims, progress = progress)
}

#' @export
getPrototype.LDARep = function(x, vocab, limit.rel, limit.abs, progress = TRUE,
  keepTopics = FALSE, keepSims = FALSE, ...){

  if (missing(limit.rel)) limit.rel = .defaultLimit.rel()
  if (missing(limit.abs)) limit.abs = .defaultLimit.abs()
  lda = getLDA(x)
  id = getID(x)
  NextMethod("getPrototype", lda = lda, vocab = vocab, id = id,
    limit.rel = limt.rel, limit.abs = limit.abs,
    keepTopics = keepTopics, keepSims = keepSims, progress = progress)
}

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
    sclop = SCLOP.pairwise(sims = jacc)
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
