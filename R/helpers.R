.makeProgressBar = function(progress, ...) {
  if (progress && getOption("width") >= 20L){
    progress::progress_bar$new(...)
  }else{
    list(tick = function(len = 1, tokens = list()) NULL, update = function(ratio, tokens) NULL)
  }
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

.defaultLimit.rel = function() 1/500
.defaultLimit.abs = function() 10
