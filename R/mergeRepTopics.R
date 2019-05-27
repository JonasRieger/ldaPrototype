#' @title Merge LDA Topic Matrices
#'
#' @description
#' Collects LDA results from a list of replicated runs and merges their topic
#' matrices for a given set of vocabularies.
#'
#' @details
#'
#' @param x [\code{named list}]\cr
#' Output from \code{\link{LDARep}}.
#' @param vocab [\code{character}]\cr
#' Vocabularies taken into consideration for merging topic matrices.
#' @return [\code{named matrix}] with the count of vocabularies (row wise) in topics (column wise).
#'
#' @examples
#' #TODO
#'
#' @export mergeRepTopics
mergeRepTopics = function(x, vocab, ...){

  if (!missing(x)){
    if (!is.LDARep(x)){
      stop("TEXT")
    }

  }else{

  }
}
