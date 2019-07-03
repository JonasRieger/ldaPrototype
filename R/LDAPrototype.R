#' @title Determine the Prototype LDA
#'
#' @description Performs multiple runs of LDA and returns the Prototype LDA of
#' this set of LDAs.
#'
#' @inheritParams LDARep
#' @param vocab [\code{character}]\cr
#' Vocabularies taken into consideration for merging topic matrices.
#' @param limit.rel [0,1]\cr
#' See \code{\link{jaccardTopics}}. Default is \code{1/500}.
#' @param limit.abs [\code{integer(1)}]\cr
#' See \code{\link{jaccardTopics}}. Default is \code{10}.
#' @param progress [\code{logical(1)}]\cr
#' Should a nice progress bar be shown for the steps of \code{\link{mergeTopics}}
#' and \code{\link{jaccardTopics}}? Turning it off, could lead to significantly
#' faster calculation. Default ist \code{TRUE}.
#' @param keepTopics [\code{logical(1)}]\cr
#' Should the merged topic matrix from \code{\link{mergeTopics}} be kept?
#' @param keepSims [\code{logical(1)}]\cr
#' Should the calculated topic similarities matrix from \code{\link{jaccardTopics}}
#' @param keepLDAs [\code{logical(1)}]\cr
#' Should the considered LDAs be kept?
#' @inherit getPrototype return
#'
#' @export LDAPrototype

LDAPrototype = function(docs, vocab, n = 100, seeds, id = "LDARep", pm.backend,
  ncpus, limit.rel, limit.abs, progress = TRUE, keepTopics = FALSE, keepSims = FALSE,
  keepLDAs = FALSE, ...){

  NULL
}
