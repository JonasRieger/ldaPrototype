#' @title Get Registry and Jobs
#'
#' @description
#' Returns the job ids and its parameter set (\code{getJob}) or the registry's
#' id (\code{getID}) or the registry itself (\code{getRegistry}) for a
#' \code{\link{LDABatch}} object.
#'
#' @param x [\code{named list}]\cr
#' Output from \code{\link{LDABatch}}.

#' @export getJob
getJob = function(x) UseMethod("getJob")

#' @export
getJob.LDABatch = function(x){
  x$jobs
}

#' @rdname getJob
#' @export getID
getID = function(x) UseMethod("getID")

#' @export
getID.LDABatch = function(x){
  x$id
}

#' @rdname getJob
#' @export getRegistry
getRegistry = function(x) UseMethod("getRegistry")

#' @export
getRegistry.LDABatch = function(x){
  x$reg
}

#' @param file.dir [\code{character(1)}]\cr
#' New file directory to overwrite the registry's old one. This can be useful
#' if the registry is transferred from a batch system.
#' @rdname getJob
#' @export setFileDir
setFileDir = function(x, file.dir) UseMethod("setFileDir")

#' @export
setFileDir.LDABatch = function(x, file.dir){
  x$reg$file.dir = file.dir
  invisible(x)
}
