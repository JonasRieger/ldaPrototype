#' @title LDA Object
#'
#' @description Constructor for LDA objects used in this package.
#'
#' @details
#'
#' @param x [\code{named list}]\cr
#' Output from \code{\link[lda]{lda.collapsed.gibbs.sampler}}. Alternatively each
#' element can be passed for individual results. Individually set parameters
#' overwrite elements from \code{x}.
#' @param assignments .
#' @param topics .
#' @param document_sums .
#' @param document_expects .
#' @param log.likelihoods .
#' @return [\code{named list}] LDA object.
#'
#' @examples
#' #TODO
#'
#' @export LDA

LDA = function(x, assignments = NULL, topics = NULL, document_sums = NULL,
  document_expects = NULL, log.likelihoods = NULL){

  if (!missing(x)){
    if (is.null(assignments)) assignments = x$assignments
    if (is.null(topics)) topics = x$topics
    if (is.null(document_sums)) document_sums = x$document_sums
    if (is.null(document_expects)) document_expects = x$document_expects
    if (is.null(log.likelihoods)) log.likelihoods = x$log.likelihoods
  }
  res = list(
    assignments = assignments,
    topics = topics,
    document_sums = document_sums,
    document_expects = document_expects,
    log.likelihoods = log.likelihoods)
  class(res) = "LDA"
  invisible(res)
}

#' @export
is.LDA = function(x, verbose = FALSE){

  if (!is.list(x)){
    if (verbose) message("object is not a list")
    return(FALSE)
  }

  emptyLDA = LDA()
  if (length(setdiff(names(x), names(emptyLDA))) != 0  ||
      length(intersect(names(x), names(emptyLDA))) != 5){
    if (verbose) message("object does not contain exactly the list elements of an LDA object")
    return(FALSE)
  }

  if (verbose) message("assignments: ", appendLF = FALSE)
  if (!is.null(x$assignments)){
    if (!is.list(x$assignments)){
      if (verbose) message("not a list")
      return(FALSE)
    }
    NDocs = lengths(x$assignments)
    NTopic = max(unlist(a$assignments)) + 1
    if (!all(sapply(x$assignments, is.integer))){
      if (verbose) message("list elements are not all integerish")
      return(FALSE)
    }
  }
  if (verbose) message("checked")

  if (verbose) message("topics: ", appendLF = FALSE)
  if (!is.null(x$topics)){
    if (!is.matrix(x$topics)){
      if (verbose) message("not a matrix")
      return(FALSE)
    }
    if (exists("NTopic") && NTopic != nrow(x$topics)){
      if (verbose) message("number of topics is not consistent")
      return(FALSE)
    }else{
      NTopic = nrow(x$topics)
    }
    if (!is.integer(x$topics)){
      if (verbose) message("matrix is not integerish")
      return(FALSE)
    }
  }
  if (verbose) message("checked")

  if (verbose) message("document_sums: ", appendLF = FALSE)
  if (!is.null(x$document_sums)){
    if (!is.matrix(x$document_sums)){
      if (verbose) message("not a matrix")
      return(FALSE)
    }
    if (exists("NTopic") && NTopic != nrow(x$document_sums)){
      if (verbose) message("number of topics is not consistent")
      return(FALSE)
    }else{
      NTopic = nrow(x$document_sums)
    }
    if (exists("NDocs")){
      if (length(NDocs) != ncol(x$document_sums)){
        if (verbose) message("number of documents is not consistent")
        return(FALSE)
      }
      if (any(NDocs != colSums(x$document_sums))){
        if (verbose) message("lengths of documents is not consistent")
        return(FALSE)
      }
    }else{
      NDocs = colSums(x$document_sums)
    }
    if (!all(sapply(x$document_sums, is.integer))){
      if (verbose) message("list elements are not all integerish")
      return(FALSE)
    }
  }
  if (verbose) message("checked")

  if (verbose) message("document_expects: ", appendLF = FALSE)
  if (!is.null(x$document_expects)){
    if (!is.matrix(x$document_expects)){
      if (verbose) message("not a matrix")
      return(FALSE)
    }
    if (exists("NTopic") && NTopic != nrow(x$document_expects)){
      if (verbose) message("number of topics is not consistent")
      return(FALSE)
    }else{
      NTopic = nrow(x$document_expects)
    }
    if (exists("NDocs")){
      if (length(NDocs) != ncol(x$document_expects)){
        if (verbose) message("number of documents is not consistent")
        return(FALSE)
      }
      if (any(colSums(x$document_expects) %% NDocs > 0)){
        if (verbose) message("lengths of documents is not consistent")
        return(FALSE)
      }
    }
    if (!all(sapply(x$document_expects, is.integer))){
      if (verbose) message("list elements are not all integerish")
      return(FALSE)
    }
  }
  if (verbose) message("checked")

  if (verbose) message("log.likelihoods: ", appendLF = FALSE)
  if (!is.null(x$log.likelihoods)){
    if (!is.matrix(x$log.likelihoods)){
      if (verbose) message("not a matrix")
      return(FALSE)
    }
    if (nrow(x$log.likelihoods) != 2){
      if (verbose) message("not two rows")
      return(FALSE)
    }
    if (!is.numeric(x$log.likelihoods)){
      if (verbose) message("not numeric")
      return(FALSE)
    }
  }
  if (verbose) message("checked")


  return(TRUE)
}

#' @export
print.LDA = function(x){
  cat("print")
}
