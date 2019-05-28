#' @title LDARep Constructor
#'
#' @description
#' Constructs a \code{\link{LDARep}} object for given elements ...
#'
#' @details
#'
#' @param 
#' @return [\code{named list}] with entries 
#'
#' @examples
#' #TODO
#'
#' @export as.LDARep

as.LDARep = function(){
  
  res = list(id = id, jobs = job, reg = reg)
  class(res) = "LDARep"
  invisible(res)
}

#' @rdname as.LDARep
#' @export
is.LDARep = function(obj, verbose = FALSE){
  
  if (!inherits(obj, "LDARep")){
    if (verbose) message("object is not of class \"LDABatch\"")
    return(FALSE)
  }
  
  if (!is.list(obj)){
    if (verbose) message("object is not a list")
    return(FALSE)
  }
  
  
  
  return(TRUE)
}
