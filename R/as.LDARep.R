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
    if (verbose) message("object is not of class \"LDABatch\"")
    return(FALSE)
  }

  if (!is.list(obj)){
    if (verbose) message("object is not a list")
    return(FALSE)
  }



  return(TRUE)
}
