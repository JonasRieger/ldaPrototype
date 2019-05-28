#' @title LDARep Constructor
#'
#' @description
#' Constructs a \code{\link{LDARep}} object for given elements ...
#'
#' @details
#'
#' @param 
#' @return [\code{named list}] with entries 
#'
#' @examples
#' #TODO
#'
#' @export as.LDARep

as.LDARep = function(reg, job, id){
  
  if (missing(reg)){
    reg = batchtools::getDefaultRegistry()
  }
  reg = batchtools::loadRegistry(reg$file.dir)
  if (missing(job)) job = batchtools::findJobs(reg = reg)
  if (is.vector(job)) job = data.frame(job.id = job)
  job = batchtools::flatten(batchtools::getJobPars(ids = job$job.id, reg = reg))
  if (missing(id))
    id = as.character(gsub(pattern = trimws(file.path(reg$work.dir, " ")),
      replacement = "", x = reg$file.dir))
  
  res = list(id = id, jobs = job, reg = reg)
  class(res) = "LDABatch"
  invisible(res)
}

#' @rdname as.LDARep
#' @export
is.LDARep = function(x, verbose = FALSE){
  
  if (!inherits(x, "LDARep")){
    if (verbose) message("object is not of class \"LDABatch\"")
    return(FALSE)
  }
  
  if (!is.list(x)){
    if (verbose) message("object is not a list")
    return(FALSE)
  }
  
  
  
  return(TRUE)
}
