#' @title Get Registry and Jobs
#'
#' @description
#' Returns the job ids and its parameter set (\code{getJob}) or the registry's
#' id (\code{getID}) or the registry itself (\code{getRegistry}).
#'
#' @name getJob
#' @rdname getJob

#' @export getJob
getJob = function(x) UseMethod("getJob")

#' @export
getJob.LDABatch = function(x){
  x$jobs
}

#' @export getID
getID = function(x) UseMethod("getID")

#' @export
getID.LDABatch = function(x){
  x$id
}

#' @export getRegistry
getRegistry = function(x) UseMethod("getRegistry")

#' @export
getRegistry.LDABatch = function(x){
  x$reg
}

#' @export setFileDir
setFileDir = function(x, file.dir) UseMethod("setFileDir")

#' @export
setFileDir.LDABatch = function(x, file.dir){
  x$reg$file.dir = file.dir
  return(x)
}
