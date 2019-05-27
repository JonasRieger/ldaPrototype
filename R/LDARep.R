#' @title LDA Replications
#'
#' @description
#' Performs multiple runs of Latent Dirichlet Allocation.
#'
#' @details
#'
#' @param docs [\code{list}]\cr
#' Documents as received from \code{\link[tosca]{LDAprep}}.
#' @param vocab [\code{character}]\cr
#' Vocabularies passed to \code{\link[lda]{lda.collapsed.gibbs.sampler}}.
#' @param n [\code{integer(1)}]\cr
#' Number of Replications.
#' @param seeds [\code{integer(n)}]\cr
#' Random Seeds for each Replication.
#' @return [\code{named list}]
#'
#' @examples
#' #TODO
#'
#' @export LDARep

LDARep = function(docs, vocab, n = 100, seeds){


  .Random.seed <<- oldseed
  class(res) = "LDARep"
  invisible(res)
}

#' @export
print.LDARep = function(x){
  cat("print")
}

#' @rdname is.LDA
#' @export
is.LDARep = function(x){
  inherits(x, "LDARep")
}
