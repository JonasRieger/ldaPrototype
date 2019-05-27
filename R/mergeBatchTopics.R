#' @title Merge LDA Topic Matrices
#'
#' @description
#' Collects LDA results from a given registry and merges their topic matrices for
#' a given set of vocabularies.
#'
#' @details
#'
#' @param x [\code{named list}]\cr
#' Output from \code{\link{LDABatch}}. Alternatively \code{job}, \code{reg} and
#' \code{id} can be passed or their defaults are taken.
#' @param vocab [\code{character}]\cr
#' Vocabularies taken into consideration for merging topic matrices.
#' @param job [\code{\link{data.frame}} or \code{integer}]\cr
#' A data.frame or data.table with a column named "job.id" or a vector of integerish job ids.
#' See \code{\link[batchtools]{reduceResultsList}}.
#' @param reg [\code{\link[batchtools]{Registry}}]\cr
#' Registry. See \code{\link[batchtools]{reduceResultsList}}.
#' @param id [\code{character(1)}]\cr
#' A name for the registry. If not passed, the folder's name is extracted from \code{reg}.
#' @return [\code{named matrix}] with the count of vocabularies (row wise) in topics (column wise).
#'
#' @examples
#' #TODO
#'
#' @export mergeBatchTopics
mergeBatchTopics = function(x, vocab, job, reg, id){

  if (!missing(x)){
    if (!is.LDABatch(x)){
      stop("object is not a LDABatch object")
    }
    id = getID(x)
    job = getJob(x)
    reg = getRegistry(x)
    batchtools::loadRegistry(reg$file.dir)
  }else{
    if (missing(reg)) reg = batchtools::getDefaultRegistry()
    if (missing(job)) job = batchtools::findDone(reg = reg)
    if (missing(id))
      id = as.character(gsub(pattern = trimws(file.path(reg$work.dir, " ")),
        replacement = "", x = reg$file.dir))
  }
  if (is.vector(job)) job = data.frame(job.id = job)
  Nlda = nrow(job)
  topicList = batchtools::reduceResultsList(ids = job, fun = function(x) x$topics, reg = reg)
  Ntopic = sapply(topicList, nrow)
  N = sum(Ntopic)

  topics = matrix(nrow = length(vocab), ncol = N)
  counter = 0
  mode(topics) = "integer"
  colnames(topics) = rep("", ncol(topics))
  rownames(topics) = vocab

  k = 1
  for(l in topicList){
    topics[match(colnames(l), vocab), seq_len(Ntopic[k]) + counter] = t(l)
    colnames(topics)[seq_len(Ntopic[k]) + counter] = paste0(id, job$job.id[k], ".", seq_len(Ntopic[k]))
    counter = counter + Ntopic[k]
    k = k + 1
  }
  topics[is.na(topics)] = 0
  invisible(topics)
}
