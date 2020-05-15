#' @title Determine the Prototype LDA
#'
#' @description Returns the Prototype LDA of a set of LDAs. This set is given as
#' \code{\link{LDABatch}} object, \code{\link{LDARep}} object, or as list of LDAs.
#' If the matrix of S-CLOP scores \code{sclop} is passed, no calculation is needed/done.
#'
#' @details While \code{\link{LDAPrototype}} marks the overall shortcut for performing
#' multiple LDA runs and choosing the Prototype of them, \code{getPrototype}
#' just hooks up at determining the Prototype. The generation of multiple LDAs
#' has to be done before use of this function. The function is flexible enough
#' to use it at at least two steps/parts of the analysis: After generating the
#' LDAs (no matter whether as LDABatch or LDARep object) or after determing
#' the pairwise SCLOP values.
#'
#' To save memory a lot of interim calculations are discarded by default.
#'
#' If you use parallel computation, no progress bar is shown.
#'
#' For details see the details sections of the workflow functions.
#'
#' @family shortcut functions
#' @family PrototypeLDA functions
#' @family workflow functions
#'
#' @param x [\code{named list}]\cr
#' \code{\link{LDABatch}} or \code{\link{LDARep}} object.
#' @param lda [\code{named list}]\cr
#' List of \code{\link{LDA}} objects, named by the corresponding "job.id".
#' @param vocab [\code{character}]\cr
#' Vocabularies taken into consideration for merging topic matrices.
#' Not considered, if \code{sclop} is passed. Default is the vocabulary of the first LDA.
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
#' @param pm.backend [\code{character(1)}]\cr
#' One of "multicore", "socket" or "mpi".
#' If \code{pm.backend} is set, \code{\link[parallelMap]{parallelStart}} is
#' called before computation is started and \code{\link[parallelMap]{parallelStop}}
#' is called after.
#' Not considered, if \code{sclop} is passed.
#' @param ncpus [\code{integer(1)}]\cr
#' Number of (physical) CPUs to use. If \code{pm.backend} is passed,
#' default is determined by \code{\link[future]{availableCores}}.
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
#'
#' @return [\code{named list}] with entries
#'  \describe{
#'   \item{\code{lda}}{List of \code{\link{LDA}} objects of the determined Prototype LDA
#'   and - if \code{keepLDAs} is \code{TRUE} - all considered LDAs.}
#'   \item{\code{protoid}}{[\code{character(1)}] Name (ID) of the determined Prototype LDA.}
#'   \item{\code{id}}{[\code{character(1)}] See above.}
#'   \item{\code{param}}{[\code{named list}] with parameter specifications for
#'   \code{limit.rel} [0,1], \code{limit.abs} [\code{integer(1)}] and
#'   \code{atLeast} [\code{integer(1)}]. See above for explanation.}
#'   \item{\code{topics}}{[\code{named matrix}] with the count of vocabularies
#'   (row wise) in topics (column wise).}
#'   \item{\code{sims}}{[\code{lower triangular named matrix}] with all pairwise
#'   jaccard similarities of the given topics.}
#'   \item{\code{wordslimit}}{[\code{integer}] with counts of words determined as
#'   relevant based on \code{limit.rel} and \code{limit.abs}.}
#'   \item{\code{wordsconsidered}}{[\code{integer}] with counts of considered
#'   words for similarity calculation. Could differ from \code{wordslimit}, if
#'   \code{atLeast} is greater than zero.}
#'   \item{\code{sclop}}{[\code{symmetrical named matrix}] with all pairwise
#'   S-CLOP scores of the given LDA runs.}
#' }
#'
#' @examples
#' res = LDARep(docs = reuters_docs, vocab = reuters_vocab,
#'    n = 4, K = 10, num.iterations = 30)
#' topics = mergeTopics(res, vocab = reuters_vocab)
#' jacc = jaccardTopics(topics, atLeast = 2)
#' dend = dendTopics(jacc)
#' sclop = SCLOP.pairwise(jacc)
#'
#' getPrototype(lda = getLDA(res), sclop = sclop)
#'
#' proto = getPrototype(res, vocab = reuters_vocab, keepSims = TRUE,
#'    limit.abs = 20, atLeast = 10)
#' proto
#' getPrototype(proto) # = getLDA(proto)
#' getConsideredWords(proto)
#' # > 10 if there is more than one word which is the 10-th often word (ties)
#' getRelevantWords(proto)
#' getSCLOP(proto)
#' @export getPrototype
getPrototype = function(...) UseMethod("getPrototype")

#' @export
getPrototype.PrototypeLDA = function(x, ...){
  getLDA(x, ...)
}

#' @rdname getPrototype
#' @export
getPrototype.LDABatch = function(x, vocab, limit.rel, limit.abs, atLeast,
  progress = TRUE, pm.backend, ncpus,
  keepTopics = FALSE, keepSims = FALSE, keepLDAs = FALSE, sclop, ...){

  if (missing(limit.rel)) limit.rel = .defaultLimit.rel()
  if (missing(limit.abs)) limit.abs = .defaultLimit.abs()
  if (missing(atLeast)) atLeast = .defaultAtLeast()
  if (missing(vocab)) vocab = .defaultVocab(x)
  if (missing(pm.backend)) pm.backend = NULL
  if (missing(ncpus)) ncpus = NULL
  if (missing(sclop)) sclop = NULL
  lda = getLDA(x)
  id = getID(x)

  NextMethod("getPrototype", lda = lda, vocab = vocab, id = id,
    limit.rel = limit.rel, limit.abs = limit.abs, atLeast = atLeast,
    progress = progress, pm.backend = pm.backend, ncpus = ncpus,
    keepTopics = keepTopics, keepSims = keepSims, keepLDAs = keepLDAs, sclop = sclop)
}

#' @rdname getPrototype
#' @export
getPrototype.LDARep = function(x, vocab, limit.rel, limit.abs, atLeast,
  progress = TRUE, pm.backend, ncpus,
  keepTopics = FALSE, keepSims = FALSE, keepLDAs = FALSE, sclop, ...){

  if (missing(limit.rel)) limit.rel = .defaultLimit.rel()
  if (missing(limit.abs)) limit.abs = .defaultLimit.abs()
  if (missing(atLeast)) atLeast = .defaultAtLeast()
  if (missing(vocab)) vocab = .defaultVocab(x)
  if (missing(pm.backend)) pm.backend = NULL
  if (missing(ncpus)) ncpus = NULL
  if (missing(sclop)) sclop = NULL
  lda = getLDA(x)
  id = getID(x)
  NextMethod("getPrototype", lda = lda, vocab = vocab, id = id,
    limit.rel = limit.rel, limit.abs = limit.abs, atLeast = atLeast,
    progress = progress, pm.backend = pm.backend, ncpus = ncpus,
    keepTopics = keepTopics, keepSims = keepSims, keepLDAs = keepLDAs, sclop = sclop)
}

#' @rdname getPrototype
#' @export
getPrototype.default = function(lda, vocab, id, limit.rel, limit.abs, atLeast,
  progress = TRUE, pm.backend, ncpus,
  keepTopics = FALSE, keepSims = FALSE, keepLDAs = FALSE, sclop, ...){

  if (missing(limit.rel)) limit.rel = .defaultLimit.rel()
  if (missing(limit.abs)) limit.abs = .defaultLimit.abs()
  if (missing(atLeast)) atLeast = .defaultAtLeast()
  if (missing(vocab)) vocab = .defaultVocab(lda)
  if (missing(pm.backend)) pm.backend = NULL
  if (missing(ncpus)) ncpus = NULL
  if (missing(id)) id = "LDARep"
  if (missing(sclop) || is.null(sclop)){
    topics = mergeRepTopics(lda = lda, vocab = vocab, id = id, progress = progress)
    sims = jaccardTopics(topics = topics, limit.rel = limit.rel, limit.abs = limit.abs,
      atLeast = atLeast, progress = progress, pm.backend = pm.backend, ncpus = ncpus)
    wordslimit = getRelevantWords(sims)
    wordsconsidered = getConsideredWords(sims)
    sclop = SCLOP.pairwise(sims)
    sims = getSimilarity(sims)
    if (!keepTopics) topics = NULL
    if (!keepSims){
      sims = NULL
      wordslimit = NULL
      wordsconsidered = NULL
    }
  }else{
    topics = NULL
    sims = NULL
    wordslimit = NULL
    wordsconsidered = NULL
  }
  protoid = as.integer(names(lda)[which.max(colSums(sclop, na.rm = TRUE))])
  if (!keepLDAs) lda = lda[which.max(colSums(sclop, na.rm = TRUE))]
  res = list(lda = lda, protoid = protoid, id = id,
    param = list(limit.rel = limit.rel, limit.abs = limit.abs, atLeast = atLeast),
    topics = topics, sims = sims, wordslimit = wordslimit,
    wordsconsidered = wordsconsidered, sclop = sclop)
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
