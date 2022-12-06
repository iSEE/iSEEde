#' Generics for Differential Expression Results
#' 
#' An overview of the generics for accessing common pieces of information in differential expression results.
#' 
#' @section Definitions:
#' \itemize{
#' \item `pvalue(x)` returns a numeric vector of raw p-values.
#' \item `log2foldchange(x)` returns a numeric vector of log2-fold-change values.
#' \item `log2average(x)` returns a numeric vector of average log2-expression values.
#' }
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

#' Generics for Embbedding Results into a SummarizedExperiment Object
#' 
#' An overview of the generics for embedding results into a \linkS4class{SummarizedExperiment} object, in a format compatible with \pkg{iSEEde}.
#' 
#' @section Definitions:
#' \itemize{
#' \item `embedResults(x, se, name, ...)` embeds the results `x` in the \linkS4class{SummarizedExperiment} `se`.
#' }
#' 
#' @docType methods
#' @aliases embedResults
#' @name utils-SummarizedExperiment
#' @author Kevin Rue-Albrecht
NULL

setGeneric(
  "embedResults",
  function(x, se, name, ...) standardGeneric("embedResults")
)
