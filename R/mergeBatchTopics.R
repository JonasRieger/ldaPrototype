#' @title Merge LDA Topic Matrices
#'
#' @description
#' Collects LDA results from a given registry and merges their topic matrices for
#' a given set of vocabularies.
#'
#' @details
#' For details and examples see \code{\link{mergeTopics}}.
#'
#' @family merge functions
#' @family batch functions
#'
#' @param x [\code{named list}]\cr
#' \code{\link{LDABatch}} object. Alternatively \code{job}, \code{reg} and
#' \code{id} can be passed or their defaults are taken.
#' @param vocab [\code{character}]\cr
#' Vocabularies taken into consideration for merging topic matrices. Default is
#' the vocabulary of the first LDA.
#' @param job [\code{\link{data.frame}} or \code{integer}]\cr
#' A data.frame or data.table with a column named "job.id" or a vector of integerish job ids.
#' See \code{\link[batchtools]{reduceResultsList}}.
#' @param reg [\code{\link[batchtools:makeRegistry]{Registry}}]\cr
#' Registry. See \code{\link[batchtools]{reduceResultsList}}.
#' @param id [\code{character(1)}]\cr
#' A name for the registry. If not passed, the folder's name is extracted from \code{reg}.
#' @param progress [\code{logical(1)}]\cr
#' Should a nice progress bar be shown? Turning it off, could lead to significantly
#' faster calculation. Default ist \code{TRUE}.
#' @param ... additional arguments
#'
#' @return [\code{named matrix}] with the count of vocabularies (row wise) in topics (column wise).
#'
#' @export mergeBatchTopics

mergeBatchTopics = function(...) UseMethod("mergeBatchTopics")

#' @rdname mergeBatchTopics
#' @export
mergeBatchTopics.LDABatch = function(x, vocab, progress = TRUE, ...){

  if (!is.LDABatch(x)){
    stop("object is not a \"LDABatch\" object")
  }
  id = getID(x)
  job = getJob(x)
  reg = getRegistry(x)
  reg = batchtools::loadRegistry(reg$file.dir)
  if (missing(vocab)) vocab = .defaultVocab(x)

  NextMethod("mergeBatchTopics", vocab = vocab, reg = reg, job = job, id = id, progress = progress)
}

#' @rdname mergeBatchTopics
#' @export
mergeBatchTopics.default = function(vocab, reg, job, id, progress = TRUE, ...){

  if (missing(reg)) reg = batchtools::getDefaultRegistry()
  if (missing(job)) job = batchtools::findDone(reg = reg)
  if (missing(id))
    id = as.character(gsub(pattern = trimws(file.path(reg$work.dir, " ")),
      replacement = "", x = reg$file.dir))
  if (is.vector(job)) job = data.frame(job.id = job)
  topicList = batchtools::reduceResultsList(ids = job, fun = function(x) getTopics(LDA(x)), reg = reg)
  if (missing(vocab)) vocab = colnames(topicList[[1]])
  Ntopic = sapply(topicList, nrow)
  N = sum(Ntopic)

  pb = .makeProgressBar(progress = progress,
    total = length(topicList)+1, format = "Merge [:bar] :percent eta: :eta")
  topics = matrix(nrow = length(vocab), ncol = N)
  counter = 0
  mode(topics) = "integer"
  colnames(topics) = rep("", ncol(topics))
  rownames(topics) = vocab
  pb$tick()

  k = 1
  for(l in topicList){
    ind = colnames(l) %in% vocab
    topics[match(colnames(l)[ind], vocab), seq_len(Ntopic[k]) + counter] = t(l[,ind])
    colnames(topics)[seq_len(Ntopic[k]) + counter] = paste0(id, job$job.id[k], ".", seq_len(Ntopic[k]))
    counter = counter + Ntopic[k]
    k = k + 1
    pb$tick()
  }
  topics[is.na(topics)] = 0
  invisible(topics)
}
