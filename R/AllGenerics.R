#' Generics for Differential Expression Results
#' 
#' An overview of the generics for accessing common pieces of information in differential expression results.
#' 
#' @section Definitions:
#' \itemize{
#' \item `pValue(x)` returns a numeric vector of raw p-values.
#' \item `log2FoldChange(x)` returns a numeric vector of log2-fold-change values.
#' \item `averageLog2(x)` returns a numeric vector of average log2-expression values.
#' }
#' 
#' @docType methods
#' @aliases pValue log2FoldChange averageLog2
#' @name de-generics
#' @author Kevin Rue-Albrecht
#' 
#' @examples 
#' showMethods(pValue)
#' showMethods(log2FoldChange)
#' showMethods(averageLog2)
NULL

setGeneric(
  "pValue",
  function(x) standardGeneric("pValue")
)

setGeneric(
  "log2FoldChange",
  function(x) standardGeneric("log2FoldChange")
)

setGeneric(
  "averageLog2",
  function(x) standardGeneric("averageLog2")
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
#' 
#' @examples 
#' embedResultsMethods
#' 
#' showMethods(embedResults)
NULL

#' @rdname utils-SummarizedExperiment
#' @aliases embedResults,ANY-method
setGeneric(
  "embedResults",
  function(x, se, name, ...) standardGeneric("embedResults")
)
