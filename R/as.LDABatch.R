#' @title LDABatch Constructor
#'
#' @description
#' Constructs a \code{\link{LDABatch}} object for given elements \code{reg},
#' \code{job} and \code{id}.
#'
#' @details
#'
#' @param reg [\code{\link[batchtools]{Registry}}]\cr
#' Registry. See \code{\link[batchtools]{findDone}}.
#' @param job [\code{\link{data.frame}} or \code{integer}]\cr
#' A data.frame or data.table with a column named "job.id" or a vector of integerish job ids.
#' See \code{\link[batchtools]{reduceResultsList}}.
#' @param id [\code{character(1)}]\cr
#' A name for the registry. If not passed, the folder's name is extracted from \code{reg}.
#' @return [\code{named list}] with entries \code{id} for the registry's folder name,
#' \code{jobs} for the submitted jobs' ids and its parameter settings and
#' \code{reg} for the registry itself.
#'
#' @examples
#' #TODO
#'
#' @export as.LDABatch

as.LDABatch = function(reg, job, id){

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

#' @rdname as.LDABatch
#' @export
is.LDABatch = function(obj, verbose = FALSE){

  if (!inherits(obj, "LDABatch")){
    if (verbose) message("object is not of class \"LDABatch\"")
    return(FALSE)
  }

  if (!is.list(obj)){
    if (verbose) message("object is not a list")
    return(FALSE)
  }

  testNames = c("id", "jobs", "reg")

  if (length(setdiff(names(obj), testNames)) != 0  ||
      length(intersect(names(obj), testNames)) != 3){
    if (verbose) message("object does not contain exactly the list elements of an \"LDABatch\" object")
    return(FALSE)
  }

  if (inherits(try(batchtools::assertRegistry(reg = getRegistry(obj))), "try-error")){
    if (verbose) message("registry: assertion failed")
    return(FALSE)
  }
  if (verbose) message("registry: checked")

  if (verbose) message("jobs: ", appendLF = FALSE)
  job = getJob(obj)
  if (!data.table::is.data.table(job) || !("job.id" %in% colnames(job))){
    if (verbose) message("not a data.table with element job.id")
    return(FALSE)
  }
  if (verbose) message("checked")

  if (verbose) message("id: ", appendLF = FALSE)
  id = getID(obj)
  if (!is.character(id) || !(length(id) == 1)){
    if (verbose) message("not a character of length 1")
    return(FALSE)
  }
  if (verbose) message("checked")

  return(TRUE)
}
