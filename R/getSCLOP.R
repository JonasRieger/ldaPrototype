#' @title Getter for PrototypeLDA
#'
#' @description
#' Returns the corresponding element of a \code{\link[=getPrototype]{PrototypeLDA}} object.
#'
#' @param x [\code{named list}]\cr
#' \code{\link[=getPrototype]{PrototypeLDA}} object.
#' @param job not implemented (relict from \code{\link{getLDA}})
#' @param reduce not implemented (relict from \code{\link{getLDA}})

#' @export getSCLOP
getSCLOP = function(x) UseMethod("getSCLOP")

#' @export
getSCLOP.PrototypeLDA = function(x){
  x$sclop
}

#' @rdname getSCLOP
#' @export getSimilarity
getSimilarity = function(x) UseMethod("getSimilarity")

#' @export
getSimilarity.PrototypeLDA = function(x){
  x$sims
}

#' @rdname getSCLOP
#' @export getMergedTopics
getMergedTopics = function(x) UseMethod("getMergedTopics")

#' @export
getMergedTopics.PrototypeLDA = function(x){
  x$topics
}

#' @rdname getSCLOP
#' @export getLDAID
getLDAID = function(x) UseMethod("getLDAID")

#' @export
getLDAID.PrototypeLDA = function(x){
  x$ldaid
}

#' @rdname getSCLOP
#' @export
getLDA.PrototypeLDA = function(x, job, reduce){
  x$lda
}

#' @rdname getSCLOP
#' @export
getID.PrototypeLDA = function(x){
  x$id
}

#' @rdname getSCLOP
#' @export
getParam.PrototypeLDA = function(x){
  list(limit.rel = x$limit.rel, limit.abs = x$limit.abs)
}
