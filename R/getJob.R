#' @title Get Job IDs
#'
#' @description
#' Returns the job ids and its parameter set.
#'
#' @name getJobs
#' @rdname getJobs

#' @export getJob

getJob = function(x) UseMethod("getJob", x)

#' @export

getJob.LDABatch = function(x){
  x$jobs
}
