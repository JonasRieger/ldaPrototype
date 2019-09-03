#' @title A Snippet of the Reuters Dataset
#'
#' @description bla
#'
#' @name reuters
#' @aliases reuters_docs reuters_vocab docs vocab
#'
#' @docType data
#' @format
#' \code{reuters_docs} is a list of documents of length 91 prepared by \code{\link[tosca]{LDAprep}}.
#'
#' \code{reuters_vocab} is a \code{character} vector of length 2139
#'
#' @usage data(reuters_docs)
#'
#' @source
#' \url{http://ronaldo.cs.tcd.ie/esslli07/data/reuters21578-xml/}
#'
#' @references
#' Lewis, David (1997). \emph{Reuters-21578 Text Categorization Collection Distribution 1.0}.
#' \url{http://kdd.ics.uci.edu/databases/reuters21578/reuters21578.html}
#'
#' Luz, Saturnino. \emph{XML-encoded version of Reuters-21578}.
#' \url{http://ronaldo.cs.tcd.ie/esslli07/data/reuters21578-xml/}
"reuters_docs"

#' @rdname reuters
#' @usage data(reuters_vocab)
"reuters_vocab"
