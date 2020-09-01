#' @title LDA Replications on a Batch System
#'
#' @description
#' Performs multiple runs of Latent Dirichlet Allocation on a batch system using
#' the \code{\link[batchtools]{batchtools-package}}.
#'
#' @details The function generates multiple LDA runs with the possability of
#' using a batch system. The integration is done by the
#' \code{\link[batchtools]{batchtools-package}}. After all jobs of the
#' corresponding registry are terminated, the whole registry can be ported to
#' your local computer for further analysis.
#'
#' The function returns a \code{LDABatch} object. You can receive results and
#' all other elements of this object with getter functions (see \code{\link{getJob}}).
#'
#' @family batch functions
#' @family LDA functions
#'
#' @param docs [\code{list}]\cr
#' Documents as received from \code{\link[tosca]{LDAprep}}.
#' @param vocab [\code{character}]\cr
#' Vocabularies passed to \code{\link[lda]{lda.collapsed.gibbs.sampler}}.
#' For additional (and necessary) arguments passed, see ellipsis (three-dot argument).
#' @param n [\code{integer(1)}]\cr
#' Number of Replications.
#' @param seeds [\code{integer(n)}]\cr
#' Random Seeds for each Replication.
#' @param id [\code{character(1)}]\cr
#' Name for the registry's folder.
#' @param load [\code{logical(1)}]\cr
#' If a folder with name \code{id} exists: should the existing registry be loaded?
#' @param chunk.size [\code{integer(1)}]\cr
#' Requested chunk size for each single chunk. See \code{\link[batchtools]{chunk}}.
#' @param resources [\code{named list}]\cr
#' Computational resources for the jobs to submit. See \code{\link[batchtools]{submitJobs}}.
#' @param ... additional arguments passed to \code{\link[lda]{lda.collapsed.gibbs.sampler}}.
#' Arguments will be coerced to a vector of length \code{n}.
#' Default parameters are \code{alpha = eta = 1/K} and \code{num.iterations = 200}.
#' There is no default for \code{K}.
#'
#' @return [\code{named list}] with entries \code{id} for the registry's folder name,
#' \code{jobs} for the submitted jobs' ids and its parameter settings and
#' \code{reg} for the registry itself.
#'
#' @examples
#' \dontrun{
#' batch = LDABatch(docs = reuters_docs, vocab = reuters_vocab, n = 4, K = 15)
#' batch
#' getRegistry(batch)
#' getJob(batch)
#' getLDA(batch, 2)
#'
#' batch2 = LDABatch(docs = reuters_docs, vocab = reuters_vocab, K = 15, chunk.size = 20)
#' batch2
#' head(getJob(batch2))
#' }
#'
#' @export LDABatch

LDABatch = function(docs, vocab, n = 100, seeds, id = "LDABatch", load = FALSE, chunk.size = 1, resources, ...){

  assert_string(id, min.chars = 1)
  assert_list(docs, min.len = 1, names = "unique", types = "matrix", any.missing = FALSE)
  stopifnot(all(sapply(docs, nrow) == 2),
            all(sapply(docs, function(x) all(x[2,] == 1))))
  assert_character(vocab, any.missing = FALSE, unique = TRUE, min.len = 2)
  assert_int(n, lower = 1)
  assert_flag(load)
  assert_int(chunk.size, lower = 1)
  assert_integerish(K, lower = 2, any.missing = FALSE, min.len = 1, max.len = n)

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
      LDA(lda.collapsed.gibbs.sampler(documents = data$docs, vocab = data$vocab, ...),
        param = list(...))
    })

  moreArgs = data.table(do.call(cbind, .paramList(n = n, ...)))

  if (missing(seeds) || length(seeds) != n){
    message("No seeds given or length of given seeds differs from number of replications: sample seeds. Sampled seeds can be obtained via getJob().")
    if (!exists(".Random.seed", envir = globalenv())){
      runif(1)
    }
    oldseed = .Random.seed
    seeds = sample(9999999, n)
    .Random.seed <<- oldseed
  }
  if (anyDuplicated(seeds)){
    message(sum(duplicated(seeds)), " duplicated seeds.")
  }
  moreArgs = data.table(seed = seeds, moreArgs)

  algo.designs = list(moreArgs)
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

  res = list(id = id, jobs = cbind(ids, moreArgs), reg = reg)
  class(res) = "LDABatch"
  res
}

#' @export
print.LDABatch = function(x, ...){
  jobs = getJob(x)
  chunked = ifelse("chunk" %in% colnames(jobs), "Chunked ", "")
  parameters = unique(jobs[, !colnames(jobs) %in%
      c("job.id", "chunk", "seed", "problem", "algorithm"), with = FALSE])
  if (nrow(parameters) == 1){
    parameters = paste0("parameters ",
      paste0(paste0(colnames(parameters), ": ", as.character(round(parameters, 4))), collapse = ", "))
  }else{
    parameters = paste0(nrow(parameters), " different parameter sets.")
  }
  cat(
    chunked, "LDABatch Object \"", getID(x), "\"\n ",
    nrow(jobs), " LDA Runs", "\n ",
    "with ", parameters, "\n\n",
    sep = ""
  )
}
