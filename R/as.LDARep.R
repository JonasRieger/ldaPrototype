#' @title LDARep Constructor
#'
#' @description
#' Constructs a \code{\link{LDARep}} object for given elements \code{lda},
#' \code{job} and \code{id}.
#'
#' @details
#'
#' @param lda [\code{named list}]\cr
#' List of \code{\link{LDA}} objects, named by the corresponding "job.id".
#' @param job [\code{\link{data.frame}} or \code{named vector}]\cr
#' A data.frame or data.table with named columns "job.id", "K", "alpha", "eta"
#' and "num.iterations" or a named vector with entries "K", "alpha", "eta" and
#' "num.iterations".
#' @param id [\code{character(1)}]\cr
#' A name for the computation. If not passed, the prefix (everything before the
#' first dot) from \code{names(lda)} is extracted, if applicable. Else is set to "LDARep".
#' @return [\code{named list}] with entries \code{id} for computation's name,
#' \code{jobs} for the parameter settings and \code{lda} for the results itself.
#'
#' @examples
#' #TODO
#'
#' @export as.LDARep

as.LDARep = function(lda, job, id){

  if (missing(id)){
    id = unique(gsub(x = names(lda), pattern = "\\.(.*)?", replacement = ""))
    if (length(id) != 1) id = "LDARep"
  }
  if (is.vector(job)){
    if (all(names(.getDefaultParameters()) %in% names(job))){
      job = data.table::data.table(job.id = names(lda), t(job))
    }else{
      stop("Not all standard parameters are specified.")
    }
  }else{
    if (all(c(names(.getDefaultParameters()), "job.id") %in% colnames(job))){
      job = data.table::as.data.table(job)
      if (!all(intersect(job$job.id, names(lda)) %in% union(job$job.id, names(lda))) ||
          nrow(job) != length(lda)){
        stop("Names of LDAs and \"job.id\" do not fit together.")
      }
      if (anyDuplicated(job$job.id) || anyDuplicated(names(lda))){
        stop("Duplicated LDA names or \"job.id\".")
      }
    }else{
      stop("Not all standard parameters are specified.")
    }
  }

  res = list(id = id, lda = lda, jobs = job)
  class(res) = "LDARep"
  invisible(res)
}

#' @rdname as.LDARep
#' @export
is.LDARep = function(obj, verbose = FALSE){

  if (!inherits(obj, "LDARep")){
    if (verbose) message("object is not of class \"LDARep\"")
    return(FALSE)
  }

  if (!is.list(obj)){
    if (verbose) message("object is not a list")
    return(FALSE)
  }

  testNames = c("id", "jobs", "lda")

  if (length(setdiff(names(obj), testNames)) != 0  ||
      length(intersect(names(obj), testNames)) != 3){
    if (verbose) message("object does not contain exactly the list elements of a \"LDARep\" object")
    return(FALSE)
  }

  if (verbose) message("lda: ", appendLF = FALSE)
  lda = getLDA(obj)
  if(!is.list(lda)){
    if (verbose) message("not a list")
    return(FALSE)
  }
  if(!all(sapply(lda, is.LDA))){
    if (verbose) message("not all elements are \"LDA\" objects")
    return(FALSE)
  }
  if (verbose) message("checked")

  if (verbose) message("jobs: ", appendLF = FALSE)
  job = getJob(obj)
  if (!data.table::is.data.table(job) ||
      !all(c(names(.getDefaultParameters()), "job.id") %in% colnames(job))){
    if (verbose) message("not a data.table with standard parameters")
    return(FALSE)
    if (!all(intersect(job$job.id, names(lda)) %in% union(job$job.id, names(lda))) ||
        nrow(job) != length(lda)){
      if (verbose) message("names of LDAs and \"job.id\" do not fit together")
      return(FALSE)
    }
    if (anyDuplicated(job$job.id) || anyDuplicated(names(lda))){
      if (verbose) message("duplicated LDA names or \"job.id\"")
      return(FALSE)
    }
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
