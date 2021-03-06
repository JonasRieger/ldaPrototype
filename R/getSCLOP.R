#' @title Getter for PrototypeLDA
#'
#' @description
#' Returns the corresponding element of a \code{\link[=getPrototype]{PrototypeLDA}} object.
#'
#' @family getter functions
#' @family PrototypeLDA functions
#'
#' @param x [\code{named list}]\cr
#' \code{\link[=getPrototype]{PrototypeLDA}} object.
#' @param job [\code{\link{data.frame}} or \code{integer}]\cr
#' A data.frame or data.table with a column named "job.id" or a vector of
#' integerish job ids. Default is the (integerish) ID of the Prototype LDA.
#' @param reduce [\code{logical(1)}]\cr
#' If the list of LDAs contains only one element, should the list be reduced and
#' the single (unnamed) element be returned? Default is \code{TRUE}.
#' Not considered, if \code{all} is \code{TRUE}.
#' @param all [\code{logical(1)}]\cr
#' Shortcut for \code{job}: Should all stored LDAs be returned?

#' @export getSCLOP
getSCLOP = function(x) UseMethod("getSCLOP")

#' @export
getSCLOP.PrototypeLDA = function(x){
  x$sclop
}

#' @rdname getSCLOP
#' @export
getSimilarity.PrototypeLDA = function(x){
  x$sims
}

#' @rdname getSCLOP
#' @export
getRelevantWords.PrototypeLDA = function(x){
  x$wordslimit
}

#' @rdname getSCLOP
#' @export
getConsideredWords.PrototypeLDA = function(x){
  x$wordsconsidered
}

#' @rdname getSCLOP
#' @export getMergedTopics
getMergedTopics = function(x) UseMethod("getMergedTopics")

#' @export
getMergedTopics.PrototypeLDA = function(x){
  x$topics
}

#' @rdname getSCLOP
#' @export getPrototypeID
getPrototypeID = function(x) UseMethod("getPrototypeID")

#' @export
getPrototypeID.PrototypeLDA = function(x){
  as.integer(x$protoid)
}

#' @rdname getSCLOP
#' @export
getLDA.PrototypeLDA = function(x, job, reduce = TRUE, all = FALSE){
  assert_flag(reduce)
  assert_flag(all)

  if (all) return(x$lda)
  if (missing(job)) job = getPrototypeID(x)
  if (is.vector(job)) job = data.frame(job.id = as.integer(job))

  assert_integerish(job$job.id, lower = 1, any.missing = FALSE, min.len = 1)

  lda = x$lda[na.omit(match(job$job.id, names(x$lda)))]
  if (reduce && length(lda) == 1) lda = lda[[1]]
  lda
}

#' @rdname getSCLOP
#' @export
getID.PrototypeLDA = function(x){
  x$id
}

#' @rdname getSCLOP
#' @export
getParam.PrototypeLDA = function(x){
  x$param
}

#' @rdname getSCLOP
#' @export
getJob.PrototypeLDA = function(x){
  x$jobs
}
