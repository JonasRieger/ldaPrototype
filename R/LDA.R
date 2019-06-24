#' @title LDA Object
#'
#' @description Constructor for LDA objects used in this package.
#'
#' @details
#'
#' @param x [\code{named list}]\cr
#' Output from \code{\link[lda]{lda.collapsed.gibbs.sampler}}. Alternatively each
#' element can be passed for individual results. Individually set elements
#' overwrite elements from \code{x}.
#' @param param [\code{named list}]\cr
#' Parameters of the function call \code{\link[lda]{lda.collapsed.gibbs.sampler}}.
#' Names always have to contain "K", "alpha", "eta" and "num.iterations".
#' @param assignments Individual element for LDA object.
#' @param topics Individual element for LDA object.
#' @param document_sums Individual element for LDA object.
#' @param document_expects Individual element for LDA object.
#' @param log.likelihoods Individual element for LDA object.
#' @param obj [\code{R} object]\cr
#' Object to test.
#' @param verbose [\code{integer(1)}]\cr
#' Should test information be given in the console?
#' @return [\code{named list}] LDA object.
#'
#' @examples
#' #TODO
#'
#' @export LDA

LDA = function(x, param = NULL, assignments = NULL, topics = NULL, document_sums = NULL,
  document_expects = NULL, log.likelihoods = NULL){

  if (!missing(x)){
    if (hasName(x, "param")) param = x$param
    if (is.null(assignments)) assignments = x$assignments
    if (is.null(topics)) topics = x$topics
    if (is.null(document_sums)) document_sums = x$document_sums
    if (is.null(document_expects)) document_expects = x$document_expects
    if (is.null(log.likelihoods)) log.likelihoods = x$log.likelihoods
  }
  res = list(
    param = param,
    assignments = assignments,
    topics = topics,
    document_sums = document_sums,
    document_expects = document_expects,
    log.likelihoods = log.likelihoods)
  class(res) = "LDA"
  invisible(res)
}

#' @rdname LDA
#' @export
as.LDA = function(x, param = NULL, assignments = NULL, topics = NULL, document_sums = NULL,
  document_expects = NULL, log.likelihoods = NULL){
  if (!missing(x)){
    LDA(
      x = x,
      param = param,
      assignments = assignments,
      topics = topics,
      document_sums = document_sums,
      document_expects = document_expects,
      log.likelihoods = log.likelihoods)
  }else{
    LDA(
      param = param,
      assignments = assignments,
      topics = topics,
      document_sums = document_sums,
      document_expects = document_expects,
      log.likelihoods = log.likelihoods)
  }
}

#' @rdname LDA
#' @export
is.LDA = function(obj, verbose = FALSE){

  if (!inherits(obj, "LDA")){
    if (verbose) message("object is not of class \"LDA\"")
    return(FALSE)
  }

  if (!is.list(obj)){
    if (verbose) message("object is not a list")
    return(FALSE)
  }

  emptyLDA = LDA(param = .getDefaultParameters())
  if (length(setdiff(names(obj), names(emptyLDA))) != 0  ||
      length(intersect(names(obj), names(emptyLDA))) != 6){
    if (verbose) message("object does not contain exactly the list elements of an \"LDA\" object")
    return(FALSE)
  }

  if (verbose) message("param: ", appendLF = FALSE)
  param = getParam(obj)
  if (!is.list(param)){
    if (verbose) message("not a list")
    return(FALSE)
  }
  if (any(!(names(getParam(emptyLDA)) %in% names(param)))){
    if (verbose) message("not all standard parameters are specified")
    return(FALSE)
  }
  if (verbose) message("checked")

  NTopic = getK(obj)

  if (verbose) message("assignments: ", appendLF = FALSE)
  assignments = getAssignments(obj)
  if (!is.null(assignments)){
    if (!is.list(assignments)){
      if (verbose) message("not a list")
      return(FALSE)
    }
    NDocs = lengths(assignments)
    if (NTopic != max(unlist(assignments)) + 1){
      warning("Check Assignments. Maximum of Assignments do not correspond to Number of Topics.")
    }
    if (!all(sapply(assignments, is.integer))){
      if (verbose) message("list elements are not all integerish")
      return(FALSE)
    }
  }
  if (verbose) message("checked")

  if (verbose) message("topics: ", appendLF = FALSE)
  topics = getTopics(obj)
  if (!is.null(topics)){
    if (!is.matrix(topics)){
      if (verbose) message("not a matrix")
      return(FALSE)
    }
    if (NTopic != nrow(topics)){
      if (verbose) message("number of topics is not consistent")
      return(FALSE)
    }
    if (!is.integer(topics)){
      if (verbose) message("matrix is not integerish")
      return(FALSE)
    }
  }
  if (verbose) message("checked")

  if (verbose) message("document_sums: ", appendLF = FALSE)
  document_sums = getDocument_sums(obj)
  if (!is.null(document_sums)){
    if (!is.matrix(document_sums)){
      if (verbose) message("not a matrix")
      return(FALSE)
    }
    if (NTopic != nrow(document_sums)){
      if (verbose) message("number of topics is not consistent")
      return(FALSE)
    }
    if (exists("NDocs")){
      if (length(NDocs) != ncol(document_sums)){
        if (verbose) message("number of documents is not consistent")
        return(FALSE)
      }
      if (any(NDocs != colSums(document_sums))){
        if (verbose) message("lengths of documents is not consistent")
        return(FALSE)
      }
    }else{
      NDocs = colSums(document_sums)
    }
    if (!all(sapply(document_sums, is.integer))){
      if (verbose) message("list elements are not all integerish")
      return(FALSE)
    }
  }
  if (verbose) message("checked")

  if (verbose) message("document_expects: ", appendLF = FALSE)
  document_expects = getDocument_expects(obj)
  if (!is.null(document_expects)){
    if (!is.matrix(document_expects)){
      if (verbose) message("not a matrix")
      return(FALSE)
    }
    if (NTopic != nrow(document_expects)){
      if (verbose) message("number of topics is not consistent")
      return(FALSE)
    }
    if (exists("NDocs")){
      if (length(NDocs) != ncol(document_expects)){
        if (verbose) message("number of documents is not consistent")
        return(FALSE)
      }
      if (any(colSums(document_expects) %% NDocs > 0)){
        if (verbose) message("lengths of documents is not consistent")
        return(FALSE)
      }
    }
    if (!all(sapply(document_expects, is.integer))){
      if (verbose) message("list elements are not all integerish")
      return(FALSE)
    }
  }
  if (verbose) message("checked")

  if (verbose) message("log.likelihoods: ", appendLF = FALSE)
  log.likelihoods = getLog.likelihoods(obj)
  if (!is.null(log.likelihoods)){
    if (!is.matrix(log.likelihoods)){
      if (verbose) message("not a matrix")
      return(FALSE)
    }
    if (nrow(log.likelihoods) != 2){
      if (verbose) message("not two rows")
      return(FALSE)
    }
    if (ncol(log.likelihoods) != getNum.iterations(obj)){
      if (verbose) message("number of columns does not equal num.iterations")
      return(FALSE)
    }
    if (!is.numeric(log.likelihoods)){
      if (verbose) message("not numeric")
      return(FALSE)
    }
  }
  if (verbose) message("checked")

  return(TRUE)
}

#' @export
print.LDA = function(x, ...){
  val = .getValues.LDA(x)
  elements = paste0("\"", names(which(!sapply(x, is.null))), "\"")
  cat(
    "LDA Object with element(s)\n",
    paste0(elements, collapse = ", "), "\n ",
    val[1], " Texts with mean length of ", round(val[2], 2), " Tokens\n ",
    val[3], " different Words\n ",
    paste0(paste0(names(getParam(x)), ": ", round(unlist(getParam(x)), 2)), collapse = ", "),
    "\n\n", sep = ""
  )
}

.getValues.LDA = function(x){
  NDoc = NA
  meanDocLength = NA
  NWord = NA
  if (!is.null(x$assignments)){
    NDocs = lengths(x$assignments)
    NDoc = length(NDocs)
    meanDocLength = mean(NDocs)
  }else{
    if (!is.null(x$document_sums)){
      NDocs = colSums(x$document_sums)
      NDoc = length(NDocs)
      meanDocLength = mean(NDocs)
    }else{
      if (!is.null(x$document_expects)){
        NDoc = ncol(x$document_expects)
      }
    }
  }
  if (!is.null(x$topics)){
    NWord = ncol(x$topics)
  }
  return(c(NDoc = NDoc, meanDocLength = meanDocLength, NWord = NWord))
}
