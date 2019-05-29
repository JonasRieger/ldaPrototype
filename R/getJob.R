#' @title Getter and Setter for LDARep and LDABatch
#'
#' @description
#' Returns the job ids and its parameter set (\code{getJob}) or the (registry's)
#' id (\code{getID}) for a \code{\link{LDABatch}} or \code{\link{LDARep}} object
#' \code{getRegistry} returns the registry itself for a \code{\link{LDABatch}}
#' object. \code{getLDA} returns the list of \code{\link{LDA}} objects for a
#' \code{\link{LDARep}} object\cr
#' Sets the registry's file directory (\code{setFilDir}) for a
#' \code{\link{LDABatch}} object.
#'
#' @param x [\code{named list}]\cr
#' \code{\link{LDABatch}} or \code{\link{LDARep}} object.

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
getLDA = function(x) UseMethod("getLDA")

#' @export
getLDA.LDARep = function(x){
  x$lda
}

#' @param file.dir [\code{character(1)}]\cr
#' New file directory to overwrite the registry's old one. This can be useful
#' if the registry is transferred from a batch system.
#' @rdname getJob
#' @export setFileDir
setFileDir = function(x, file.dir) UseMethod("setFileDir")

#' @export
setFileDir.LDABatch = function(x, file.dir){
  x$reg$file.dir = fs::fs_path(file.dir)
  invisible(x)
}
