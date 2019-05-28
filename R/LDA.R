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
#' @param assignments Individual element for LDA object.
#' @param topics Individual element for LDA object.
#' @param document_sums Individual element for LDA object.
#' @param document_expects Individual element for LDA object.
#' @param log.likelihoods Individual element for LDA object.
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

#' @rdname LDA
#' @export
as.LDA = function(x, assignments = NULL, topics = NULL, document_sums = NULL,
  document_expects = NULL, log.likelihoods = NULL){
  if (!missing(x)){
    LDA(
      x = x,
      assignments = assignments,
      topics = topics,
      document_sums = document_sums,
      document_expects = document_expects,
      log.likelihoods = log.likelihoods)
  }else{
    LDA(
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

  emptyLDA = LDA()
  if (length(setdiff(names(obj), names(emptyLDA))) != 0  ||
      length(intersect(names(obj), names(emptyLDA))) != 5){
    if (verbose) message("object does not contain exactly the list elements of an \"LDA\" object")
    return(FALSE)
  }

  if (verbose) message("assignments: ", appendLF = FALSE)
  if (!is.null(obj$assignments)){
    if (!is.list(obj$assignments)){
      if (verbose) message("not a list")
      return(FALSE)
    }
    NDocs = lengths(obj$assignments)
    NTopic = max(unlist(obj$assignments)) + 1
    if (!all(sapply(obj$assignments, is.integer))){
      if (verbose) message("list elements are not all integerish")
      return(FALSE)
    }
  }
  if (verbose) message("checked")

  if (verbose) message("topics: ", appendLF = FALSE)
  if (!is.null(obj$topics)){
    if (!is.matrix(obj$topics)){
      if (verbose) message("not a matrix")
      return(FALSE)
    }
    if (exists("NTopic") && NTopic != nrow(obj$topics)){
      if (verbose) message("number of topics is not consistent")
      return(FALSE)
    }else{
      NTopic = nrow(obj$topics)
    }
    if (!is.integer(obj$topics)){
      if (verbose) message("matrix is not integerish")
      return(FALSE)
    }
  }
  if (verbose) message("checked")

  if (verbose) message("document_sums: ", appendLF = FALSE)
  if (!is.null(obj$document_sums)){
    if (!is.matrix(obj$document_sums)){
      if (verbose) message("not a matrix")
      return(FALSE)
    }
    if (exists("NTopic") && NTopic != nrow(obj$document_sums)){
      if (verbose) message("number of topics is not consistent")
      return(FALSE)
    }else{
      NTopic = nrow(obj$document_sums)
    }
    if (exists("NDocs")){
      if (length(NDocs) != ncol(obj$document_sums)){
        if (verbose) message("number of documents is not consistent")
        return(FALSE)
      }
      if (any(NDocs != colSums(obj$document_sums))){
        if (verbose) message("lengths of documents is not consistent")
        return(FALSE)
      }
    }else{
      NDocs = colSums(obj$document_sums)
    }
    if (!all(sapply(obj$document_sums, is.integer))){
      if (verbose) message("list elements are not all integerish")
      return(FALSE)
    }
  }
  if (verbose) message("checked")

  if (verbose) message("document_expects: ", appendLF = FALSE)
  if (!is.null(obj$document_expects)){
    if (!is.matrix(obj$document_expects)){
      if (verbose) message("not a matrix")
      return(FALSE)
    }
    if (exists("NTopic") && NTopic != nrow(obj$document_expects)){
      if (verbose) message("number of topics is not consistent")
      return(FALSE)
    }else{
      NTopic = nrow(obj$document_expects)
    }
    if (exists("NDocs")){
      if (length(NDocs) != ncol(obj$document_expects)){
        if (verbose) message("number of documents is not consistent")
        return(FALSE)
      }
      if (any(colSums(obj$document_expects) %% NDocs > 0)){
        if (verbose) message("lengths of documents is not consistent")
        return(FALSE)
      }
    }
    if (!all(sapply(obj$document_expects, is.integer))){
      if (verbose) message("list elements are not all integerish")
      return(FALSE)
    }
  }
  if (verbose) message("checked")

  if (verbose) message("log.likelihoods: ", appendLF = FALSE)
  if (!is.null(obj$log.likelihoods)){
    if (!is.matrix(obj$log.likelihoods)){
      if (verbose) message("not a matrix")
      return(FALSE)
    }
    if (nrow(obj$log.likelihoods) != 2){
      if (verbose) message("not two rows")
      return(FALSE)
    }
    if (!is.numeric(obj$log.likelihoods)){
      if (verbose) message("not numeric")
      return(FALSE)
    }
  }
  if (verbose) message("checked")

  return(TRUE)
}

#' @export
print.LDA = function(x){
  val = .getValues.LDA(x)
  like = ifelse(val[5], paste0("\n Computed Log-Likelihoods of ", val[6], " Iterations"), "")
  elements = paste0("\"", names(which(!sapply(x, is.null))), "\"")
  cat(
    "LDA Object with element(s)\n",
    paste0(elements, collapse = ", "), "\n ",
    val[1], " Texts with mean length of ", round(val[2], 2), " Tokens\n ",
    val[4], " different Words\n ",
    val[3], " latent Topics",
    like, "\n\n",
    sep = ""
  )
}

.getValues.LDA = function(x){
  NDoc = NA
  meanDocLength = NA
  NTopic = NA
  NWord = NA
  like = !is.null(x$log.likelihoods)
  iter = ifelse(like, ncol(x$log.likelihoods), NA)
  if (!is.null(x$assignments)){
    NDocs = lengths(x$assignments)
    NDoc = length(NDocs)
    meanDocLength = mean(NDocs)
    NTopic = max(unlist(x$assignments)) + 1
  }else{
    if (!is.null(x$document_sums)){
      NDocs = colSums(x$document_sums)
      NDoc = length(NDocs)
      meanDocLength = mean(NDocs)
      NTopic = nrow(x$document_sums)
    }else{
      if (!is.null(x$document_expects)){
        NDoc = ncol(x$document_expects)
        NTopic = nrow(x$document_expects)
      }
    }
  }
  if (!is.null(x$topics)){
    NWord = ncol(x$topics)
    NTopic = nrow(x$topics)
  }
  return(c(NDoc = NDoc, meanDocLength = meanDocLength, NTopic = NTopic,
    NWord = NWord, like = like, iter = iter))
}
