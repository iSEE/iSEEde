#' Generics for Differential Expression Results
#' 
#' An overview of the generics for accessing common pieces of information in differential expression results.
#' 
#' @section Definitions:
#' \item `pvalue(x)` returns a numeric vector of raw p-values.
#' 
#' \item `log2foldchange(x)` returns a numeric vector of log2-fold-change values.
#' 
#' \item `log2average(x)` returns a numeric vector of average log2-expression values.
#' 
#' @docType methods
#' @aliases pvalue log2foldchange log2average
#' @name de-generics
#' @author Kevin Rue-Albrecht
NULL

setGeneric(
  "pvalue",
  function(x) standardGeneric("pvalue")
)

setGeneric(
  "log2foldchange",
  function(x) standardGeneric("log2foldchange")
)

setGeneric(
  "log2average",
  function(x) standardGeneric("log2average")
)
