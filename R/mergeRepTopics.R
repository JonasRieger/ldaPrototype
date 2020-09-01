#' @title Merge LDA Topic Matrices
#'
#' @description
#' Collects LDA results from a list of replicated runs and merges their topic
#' matrices for a given set of vocabularies.
#'
#' @details
#' For details and examples see \code{\link{mergeTopics}}.
#'
#' @family merge functions
#' @family replication functions
#'
#' @param x [\code{named list}]\cr
#' \code{\link{LDARep}} object. Alternatively \code{lda} and \code{id} can be passed.
#' @param vocab [\code{character}]\cr
#' Vocabularies taken into consideration for merging topic matrices. Default is
#' the vocabulary of the first LDA.
#' @param lda [\code{named list}]\cr
#' List of \code{\link{LDA}} objects, named by the corresponding "job.id".
#' @param id [\code{character(1)}]\cr
#' Name for the computation. Default is "LDARep".
#' @param progress [\code{logical(1)}]\cr
#' Should a nice progress bar be shown? Turning it off, could lead to significantly
#' faster calculation. Default ist \code{TRUE}.
#' @param ... additional arguments
#'
#' @return [\code{named matrix}] with the count of vocabularies (row wise) in topics (column wise).
#'
#' @export mergeRepTopics
mergeRepTopics = function(...) UseMethod("mergeRepTopics")

#' @rdname mergeRepTopics
#' @export
mergeRepTopics.LDARep = function(x, vocab, progress = TRUE, ...){

  if (!is.LDARep(x)){
    stop("object is not a \"LDARep\" object")
  }
  lda = getLDA(x)
  id = getID(x)
  if (missing(vocab)) vocab = .defaultVocab(x)

  NextMethod("mergeRepTopics", lda = lda, vocab = vocab, id = id, progress = progress)
}

#' @rdname mergeRepTopics
#' @export
mergeRepTopics.default = function(lda, vocab, id, progress = TRUE, ...){

  if (missing(vocab)) vocab = .defaultVocab(lda)
  if (missing(id)) id = "LDARep"

  assert_flag(progress)
  assert_string(id, min.chars = 1)
  assert_list(lda, types = "LDA", any.missing = FALSE, min.len = 1, names = "named")
  assert_character(vocab, any.missing = FALSE, unique = TRUE, min.len = 2)

  topicList = lapply(lda, getTopics)
  Ntopic = sapply(topicList, nrow)
  N = sum(Ntopic)

  pb = .makeProgressBar(progress = progress,
    total = length(topicList)+1, format = "Merge [:bar] :percent eta: :eta")
  topics = matrix(nrow = length(vocab), ncol = N)
  counter = 0
  mode(topics) = "integer"
  colnames(topics) = rep("", ncol(topics))
  rownames(topics) = vocab
  name = names(lda)
  pb$tick()

  k = 1
  for(l in topicList){
    ind = colnames(l) %in% vocab
    topics[match(colnames(l)[ind], vocab), seq_len(Ntopic[k]) + counter] = t(l[,ind])
    colnames(topics)[seq_len(Ntopic[k]) + counter] = paste0(id, name[k], ".", seq_len(Ntopic[k]))
    counter = counter + Ntopic[k]
    k = k + 1
    pb$tick()
  }
  topics[is.na(topics)] = 0
  invisible(topics)
}
