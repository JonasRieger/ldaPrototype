#' @title ldaPrototype: Prototype of multiple Latent Dirichlet Allocation Runs
#'
#' @description Determine a Prototype from a number of runs of Latent Dirichlet
#' Allocation (LDA) measuring its similarities with S-CLOP: A procedure to select
#' the LDA run with highest mean pairwise similarity, which is measured by S-CLOP
#' (Similarity of multiple sets by Clustering with Local Pruning), to all other
#' runs. LDA runs are specified by its assignments leading to estimators for
#' distribution parameters. Repeated runs lead to different results, which we
#' encounter by choosing the most representative LDA run as prototype.\cr
#' For bug reports and feature requests please use the issue tracker:
#' \url{https://github.com/JonasRieger/ldaPrototype/issues}.
#'
#' @section Data:
#' \code{\link{reuters}} Example Dataset (91 articles from Reuters) for testing.
#'
#' @section Constructor:
#' \code{\link{LDA}} LDA objects used in this package.\cr
#' \code{\link{as.LDARep}} LDARep objects.\cr
#' \code{\link{as.LDABatch}} LDABatch objects.
#'
#' @section Getter:
#' \code{\link{getTopics}} Getter for \code{\link{LDA}} objects.\cr
#' \code{\link{getJob}} Getter for \code{\link{LDARep}} and \code{\link{LDABatch}} objects.\cr
#' \code{\link{getSimilarity}} Getter for \code{\link[=jaccardTopics]{TopicSimilarity}} objects.\cr
#' \code{\link{getSCLOP}} Getter for \code{\link[=getPrototype]{PrototypeLDA}} objects.\cr
#' \code{\link{getPrototype}} Determine the Prototype LDA.
#'
#' @section Performing multiple LDAs:
#' \code{\link{LDARep}} Performing multiple LDAs locally (using parallelization).\cr
#' \code{\link{LDABatch}} Performing multiple LDAs on Batch Systems.
#'
#' @section Calculation Steps (Workflow) to determine the Prototype LDA:
#' \code{\link{mergeTopics}} Merge topic matrices from multiple LDAs.\cr
#' \code{\link{jaccardTopics}} Calculate topic similarities.\cr
#' \code{\link{dendTopics}} Create a dendrogram from topic similarities.\cr
#' \code{\link{SCLOP}} Determine various S-CLOP values.\cr
#' \code{\link{pruneSCLOP}} Prune \code{\link[=dendTopics]{TopicDendrogram}} objects.
#'
#' @section Shortcuts:
#' \code{\link{getPrototype}} Shortcut which includes all calculation steps.\cr
#' \code{\link{LDAPrototype}} Shortcut which performs multiple LDAs and
#' determines their Prototype.
#'
#' @import data.table
#' @import stats
#' @import checkmate
#' @importFrom utils combn hasName head
#' @importFrom progress progress_bar
#' @importFrom fs fs_path
#' @importFrom lda lda.collapsed.gibbs.sampler
#' @importFrom dendextend labels_colors labels_colors<- labels<-
#' @importFrom colorspace rainbow_hcl
#' @importFrom graphics plot abline
"_PACKAGE"

.getDefaultParameters = function(K){
  if (missing(K)){
    return(list(K = 100, alpha = 0.01, eta = 0.01, num.iterations = 200))
  }else{
    return(list(K = K, alpha = 1/K, eta = 1/K, num.iterations = 200))
  }
}

.defaultLimit.rel = function() 1/500
.defaultLimit.abs = function() 10
.defaultAtLeast = function() 0

# x is a LDARep or LDABatch (or a list of LDAs or single LDA object)
# returns the considered vocabulary of the first LDA object
# (useful for the case of simply having replications of the same parameters)
.defaultVocab = function(x){
  if (inherits(x, c("LDARep", "LDABatch"))){
    x = getLDA(x, job = getJob(x)$job.id[1], reduce = TRUE)
  }
  if (inherits(x, "LDA")){
    return(colnames(getTopics(x)))
  }else{
    return(colnames(getTopics(x[[1]])))
  }
}
