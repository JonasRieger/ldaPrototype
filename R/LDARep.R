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
#' @param id [\code{character(1)}]\cr
#' Name for the computation and prefix for the replication's ids.
#' @param pm.backend [\code{character(1)}]\cr
#' One of \"multicore\", \"socket\" or \"mpi\".
#' If \code{pm.backend} is set, \code{\link[parallelMap]{parallelStart}} is
#' called before computation is started and \code{\link[parallelMap]{parallelStop}}
#' is called after.
#' @param ncpus [\code{integer(1)}]\cr
#' Number of (physical) CPUs to use.
#' @return [\code{named list}]
#'
#' @examples
#' #TODO
#'
#' @export LDARep

LDARep = function(docs, vocab, n = 100, seeds, id = "LDARep", pm.backend, ncpus, ...){

  args = .paramList(n = n, ...)
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
  args$seed = seeds
  args$fun = function(seed, ...){
    set.seed(seed)
    LDA(lda::lda.collapsed.gibbs.sampler(documents = docs, vocab = vocab, ...))
  }

  if (!missing(pm.backend)){
    if (missing(ncpus)) ncpus = parallel::detectCores()
    parallelMap::parallelStart(mode = pm.backend, ncpus = ncpus)
  }

  ldas = do.call(parallelMap::parallelMap, args = args)
  if (!missing(pm.backend)) parallelMap::parallelStop()
  args = data.table::data.table(do.call(cbind, args[names(args) != "fun"]))
  args$job.id = paste(id, seq_len(n), ".")

  .Random.seed <<- oldseed

  res = list(id = id, lda = ldas, jobs = args)
  class(res) = "LDARep"
  invisible(res)
}

#' @export
print.LDARep = function(x){
  jobs = getJob(x)
  parameters = unique(jobs[, !colnames(jobs) %in% c("job.id", "seed"), with = FALSE])
  if (nrow(parameters) == 1){
    parameters = paste0("parameters ",
      paste0(paste0(colnames(parameters), ": ", as.character(round(parameters, 4))), collapse = ", "))
  }else{
    parameters = paste0(nrow(parameters), " different parameter sets.")
  }
  cat(
    "LDARep Object \"", getID(x), "\"\n ",
    nrow(jobs), " LDA Runs", "\n ",
    "with ", parameters, "\n\n",
    sep = ""
  )
}

.getDefaultParameters = function(K){
  if (missing(K)){
    return(list(K = 100, alpha = 0.01, eta = 0.01, num.iterations = 200))
  }else{
    return(list(K = K, alpha = 1/K, eta = 1/K, num.iterations = 200))
  }
}

.paramList = function(n, ...){
  moreArgs = list(...)
  if(anyDuplicated(names(moreArgs))){
    tmp = duplicated(names(moreArgs), fromLast = TRUE)
    warning("Parameter(s) ", paste0(names(moreArgs)[tmp], collapse = ", "), " are duplicated. Take last one(s).")
    moreArgs = moreArgs[!tmp]
  }
  if ("K" %in% names(moreArgs)){
    default = .getDefaultParameters(K = moreArgs$K)
  }else{
    default = .getDefaultParameters()
  }
  moreArgs = c(moreArgs, default[!names(default) %in% names(moreArgs)])
  moreArgs[lengths(moreArgs) != n] = lapply(moreArgs[lengths(moreArgs) != n], rep_len, length.out = n)
  return(moreArgs)
}
