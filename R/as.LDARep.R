#' @title LDARep Constructor
#'
#' @description
#' Constructs a \code{\link{LDARep}} object for given elements \code{lda},
#' \code{job} and \code{id}.
#'
#' @details
#' Given a list of \code{\link{LDA}} objects the function returns
#' a \code{\link{LDARep}} object, which can be handled using the getter functions
#' at \code{\link{getJob}}.
#'
#' @family constructor functions
#' @family replication functions
#'
#' @param x [\code{named list}]\cr
#' \code{\link{LDABatch}} or \code{\link{LDARep}} object.
#' @param lda [\code{named list}]\cr
#' List of \code{\link{LDA}} objects, named by the corresponding "job.id" (\code{integerish}).
#' If list is unnamed, names are set.
#' @param job [\code{\link{data.frame}} or \code{named vector}]\cr
#' A data.frame or data.table with named columns (at least)
#' "job.id" (\code{integerish}), "K", "alpha", "eta" and "num.iterations"
#' or a named vector with entries (at least) "K", "alpha", "eta" and "num.iterations".
#' If not passed, it is interpreted from \code{param} of each LDA.
#' @param id [\code{character(1)}]\cr
#' A name for the computation. If not passed, it is set to "LDARep".
#' @param obj [\code{R} object]\cr
#' Object to test.
#' @param verbose [\code{logical(1)}]\cr
#' Should test information be given in the console?
#' @param ... additional arguments
#'
#' @return [\code{named list}] with entries \code{id} for computation's name,
#' \code{jobs} for the parameter settings and \code{lda} for the results themselves.
#'
#' @examples
#' res = LDARep(docs = reuters_docs, vocab = reuters_vocab, n = 4, K = 7, num.iterations = 20)
#' lda = getLDA(res)
#'
#' res2 = as.LDARep(lda, id = "newName")
#' res2
#' getJob(res2)
#' getJob(res)
#'
#' \donttest{
#' batch = LDABatch(docs = reuters_docs, vocab = reuters_vocab, n = 4, id = "TEMP", K = 30)
#' res3 = as.LDARep(batch)
#' res3
#' getJob(res3)
#' }
#'
#' @export as.LDARep
as.LDARep = function(...) UseMethod("as.LDARep")

#' @rdname as.LDARep
#' @export
as.LDARep.default = function(lda, job, id, ...){

  if (missing(id)) id = "LDARep"
  if (is.null(names(lda))){
    if (missing(job) || is.vector(job)){
      names(lda) = seq_along(lda)
    }
    else{
      names(lda) = job$job.id
    }
  }
  if (missing(job)){
    job = rbindlist(lapply(lda, function(x)
      as.data.table(data.frame((getParam(x))))), fill = TRUE)
    job$job.id = as.integer(names(lda))
    setcolorder(job, "job.id")
  }else{
    if (is.vector(job)){
      if (all(names(.getDefaultParameters()) %in% names(job))){
        job = data.table(job.id = as.integer(names(lda)), t(job))
      }else{
        stop("Not all standard parameters are specified.")
      }
    }else{
      if (all(c(names(.getDefaultParameters()), "job.id") %in% colnames(job))){
        job = as.data.table(job)
        if (!all(union(job$job.id, names(lda)) %in% intersect(job$job.id, names(lda))) ||
            nrow(job) != length(lda)){
          stop("Names of LDAs and \"job.id\" do not fit together.")
        }
        job$job.id = as.integer(job$job.id)
      }else{
        stop("Not all standard parameters are specified.")
      }
    }
  }
  if (anyDuplicated(job$job.id) || anyDuplicated(names(lda))){
    stop("Duplicated LDA names or \"job.id\".")
  }

  res = list(id = id, lda = lda, jobs = job)
  class(res) = "LDARep"
  res
}

#' @rdname as.LDARep
#' @export
as.LDARep.LDARep = function(x, ...){

  lda = getLDA(x)
  job = getJob(x)
  id = getID(x)

  res = list(id = id, lda = lda, jobs = job)
  class(res) = "LDARep"
  res
}

#' @export
as.LDARep.LDABatch = as.LDARep.LDARep


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
  lda = getLDA(obj, reduce = FALSE)
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
  if (!is.data.table(job) ||
      !all(c(names(.getDefaultParameters()), "job.id", "seed") %in% colnames(job))){
    if (verbose) message("not a data.table with standard parameters")
    return(FALSE)
  }
  if (anyDuplicated(job$job.id) || anyDuplicated(names(lda))){
    if (verbose) message("duplicated LDA names or \"job.id\"")
    return(FALSE)
  }
  if (!is.integer(job$job.id)){
    if (verbose) message("\"job.id\" is not integerish")
    return(FALSE)
  }
  #if (!all(union(job$job.id, names(lda)) %in% intersect(job$job.id, names(lda))) ||
  #    nrow(job) != length(lda)){
  #  if (verbose) message("names of LDAs and \"job.id\" do not fit together")
  #  return(FALSE)
  #}
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
