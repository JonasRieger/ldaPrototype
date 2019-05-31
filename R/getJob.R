#' @title Getter and Setter for LDARep and LDABatch
#'
#' @description
#' Returns the job ids and its parameter set (\code{getJob}) or the (registry's)
#' id (\code{getID}) for a \code{\link{LDABatch}} or \code{\link{LDARep}} object.
#' \code{getRegistry} returns the registry itself for a \code{\link{LDABatch}}
#' object. \code{getLDA} returns the list of \code{\link{LDA}} objects for a
#' \code{\link{LDABatch}} or \code{\link{LDARep}} object. In addition, you can
#' specify one or more LDAs by their id(s).\cr
#' \code{setFilDir} sets the registry's file directory for a
#' \code{\link{LDABatch}} object.
#'
#' @param x [\code{named list}]\cr
#' \code{\link{LDABatch}} or \code{\link{LDARep}} object.
#' @param job [\code{\link{data.frame}} or \code{integer}]\cr
#' A data.frame or data.table with a column named "job.id" or a vector of integerish job ids.
#' @param reduce [\code{logical(1)}]\cr
#' If the list of LDAs contains only one element, should the list be reduced and
#' the single (unnamed) element be returned?
#' @param file.dir [Vector to be coerced to a \code{\link[fs]{fs_path}} object.]\cr
#' New file directory to overwrite the registry's old one. This can be useful
#' if the registry is transferred from a batch system.

#' @export getJob
getJob = function(x) UseMethod("getJob")

#' @export
getJob.LDABatch = function(x){
  x$jobs
}

#' @export
getJob.LDARep = function(x){
  x$jobs
}

#' @rdname getJob
#' @export getID
getID = function(x) UseMethod("getID")

#' @export
getID.LDABatch = function(x){
  x$id
}

#' @export
getID.LDARep = function(x){
  x$id
}

#' @rdname getJob
#' @export getRegistry
getRegistry = function(x) UseMethod("getRegistry")

#' @export
getRegistry.LDABatch = function(x){
  x$reg
}

#' @rdname getJob
#' @export getLDA
getLDA = function(x, job, reduce) UseMethod("getLDA")

#' @export
getLDA.LDARep = function(x, job, reduce = TRUE){

  if (missing(job)) job = getJob(x)
  if (is.vector(job)) job = data.frame(job.id = as.integer(job))

  lda = x$lda[match(job$job.id, names(x$lda))]
  if (reduce && length(lda) == 1) lda = lda[[1]]
  lda
}

#' @export
getLDA.LDABatch = function(x, job, reduce = TRUE){

  reg = getRegistry(x)
  reg = batchtools::loadRegistry(file.dir = reg$file.dir)

  if (missing(job)) job = getJob(x)
  if (is.vector(job)) job = data.frame(job.id = as.integer(job))

  lda = batchtools::reduceResultsList(ids = job, fun = LDA, reg = reg)
  names(lda) = job$job.id
  if (reduce && length(lda) == 1) lda = lda[[1]]
  lda
}

#' @rdname getJob
#' @export setFileDir
setFileDir = function(x, file.dir) UseMethod("setFileDir")

#' @export
setFileDir.LDABatch = function(x, file.dir){
  x$reg$file.dir = fs::fs_path(file.dir)
  invisible(x)
}
