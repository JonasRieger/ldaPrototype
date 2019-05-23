#' @title LDA Replications on a Batch System
#'
#' @description
#' Performs multiple runs of Latent Dirichlet Allocation on a batch system using
#' the package \code{\link[batchtools]{batchtools}}.
#'
#' @details
#'
#' @param id [\code{character(1)}]\cr
#' Name for the registry's folder.
#' @param docs [\code{list}]\cr
#' Documents as received from \code{\link[tosca]{LDAprep}}.
#' @param vocab [\code{character}]\cr
#' Vocabularies passed to \code{\link[lda]{lda.collapsed.gibbs.sampler}}.
#' @param n [\code{integer(1)}]\cr
#' Number of Replications.
#' @param seeds [\code{integer(n)}]\cr
#' Random Seeds for each Replication.
#' @param load [\code{logical(1)}]\cr
#' If a folder with name \code{id} exists: should the existing registry be loaded?
#' @param chunk.size [\code{integer(1)}]\cr
#' Requested number of chunks. See \code{\link[batchtools]{chunk}}.
#' @param resources [\code{named list}]\cr
#' Computational resources for the jobs to submit. See \code{\link[batchtools]{submitJobs}}.
#' @param ... additional arguments passed to \code{\link[lda]{lda.collapsed.gibbs.sampler}}.
#' Arguments may be passed as scalar (which will be coerced to a vector of length \code{n})
#' or vector of length \code{n}.
#' @return [\code{named list}] with entries \code{id} for the registry's folder name,
#' \code{ids} for the submitted jobs' ids and \code{reg} for the registry itself.
#'
#' @examples
#' #TODO
#'
#' @export LDABatch

LDABatch = function(id = "LDArep", docs, vocab, n = 100, seeds, load = FALSE, chunk.size = 1, resources, ...){

  stopifnot(is.character(id), length(id) == 1,
    is.list(docs), all(sapply(docs, is.matrix)), all(sapply(docs, nrow) == 2),
    all(sapply(docs, function(x) all(x[2,] == 1))),
    is.character(vocab),
    is.numeric(n), length(n) == 1, as.integer(n) == n,
    is.logical(load), length(load) == 1,
    is.numeric(chunk.size), as.integer(chunk.size) == chunk.size)

  moreArgs = list(...)
  if(anyDuplicated(names(moreArgs))){
    tmp = duplicated(names(moreArgs), fromLast = TRUE)
    warning("Parameter(s) ", paste0(names(moreArgs)[tmp], collapse = ", "), " are duplicated. Take last ones.")
    moreArgs = moreArgs[!tmp]
  }
  if ("K" %in% names(moreArgs)){
    default = list(K = 0, alpha = 1/moreArgs$K, eta = 1/moreArgs$K, num.iterations = 200)
  }else{
    default = list(K = 100, alpha = 0.01, eta = 0.01, num.iterations = 200)
  }
  moreArgs = c(moreArgs, default[!(c("K", "alpha", "eta", "num.iterations") %in% names(moreArgs))])
  moreArgs[lengths(moreArgs) == 1] = lapply(moreArgs[lengths(moreArgs) == 1], rep, times = n)
  if (any(lengths(moreArgs) != n)){
    stop("Additional arguments for lda::lda.collapsed.gibbs.sampler \"",
      paste(names(moreArgs)[lengths(moreArgs) != n], collapse = ","),
      "\" are not of length 1 or ", n)
  }
  moreArgs = do.call(cbind, moreArgs)

  fd = file.path(id)
  if (dir.exists(fd)){
    if (load){
      message("Load Experiment Registry: ", fd)
      reg = batchtools::loadRegistry(fd, writeable = TRUE)
    }else{
      stop("Direction ", fd, " exists. Set load = TRUE or a new id.")
    }
  }else{
    message("Create Experiment Registry: ", fd)
    reg = batchtools::makeExperimentRegistry(file.dir = fd, packages = "lda")
  }

  batchtools::addProblem(paste0(id, "Problem"), data = list(docs = docs, vocab = vocab))

  batchtools::addAlgorithm(paste0(id, "Algorithm"),
    fun = function(job, data, instance, seed, ...){
      set.seed(seed)
      return(lda::lda.collapsed.gibbs.sampler(documents = data$docs, vocab = data$vocab, ...))
    })

  if (missing(seeds) || length(seeds) != n){
    message("No seeds given or length of given seeds differs from number of replications: sample seeds")
    if (!exists(".Random.seed", envir = globalenv())){
      runif(1)
    }
    oldseed = .Random.seed
    seeds = sample(9999999, n)
  }
  if (anyDuplicated(seeds)){
    message(sum(duplicated(seeds)), " duplicated seeds.")
  }

  algo.designs = list(data.table::data.table(seed = seeds, moreArgs))
  names(algo.designs) = paste0(id, "Algorithm")

  ids = batchtools::addExperiments(
    prob.designs = NULL,
    algo.designs = algo.designs
  )

  if(chunk.size > 1)
    ids$chunk = batchtools::chunk(ids$job.id, chunk.size = chunk.size)

  if (missing(resources)){
    message("Argument resources is missing: using default resources.")
    batchtools::submitJobs(ids)
  }else{
    batchtools::submitJobs(ids, resources = resources)
  }

  .Random.seed <<- oldseed
  res = list(id = id, jobs = cbind(ids, algo.designs[[1]]), reg = reg)
  class(res) = "LDABatch"
  invisible(res)
}

#' @export
print.LDABatch = function(x){
  chunked = ifelse("chunk" %in% colnames(x$jobs), "Chunked ", "")
  parameters = unique(x$jobs[, !colnames(x$jobs) %in% c("job.id", "chunk", "seed"), with = FALSE])
  if (nrow(parameters) == 1){
    parameters = paste0("parameters ",
      paste0(paste0(colnames(parameters), ": ", as.character(round(parameters, 4))), collapse = ", "))
  }else{
    parameters = paste0(nrow(parameters), " different parameter sets.")
  }
  cat(
    chunked, "LDABatch Object \"", x$id, "\"\n ",
    nrow(x$jobs), " LDA Runs", "\n ",
    "with ", parameters, "\n\n",
    sep = ""
  )
}
