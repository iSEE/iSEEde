#' The iSEEDESeq2Results class
#' 
#' The `iSEEDESeq2Results` class is used to provide an common interface to differential expression results produced by the \pkg{limma} package.
#' It provides methods to access common differential expression statistics (e.g., log2 fold-change, p-value, log2 average abundance).
#' 
#' This class inherits all its slots directly from its parent class \linkS4class{DataFrame}.
#' 
#' @section Constructor:
#' \code{iSEEDESeq2Results(data, row.names = rownames(data))} creates an instance of a `iSEEDESeq2Results` class, with:
#' 
#' \describe{
#' \item{`data`}{A `data.frame` produced by `DESeq2::results()` or `DESeq2::lfcShrink()`.}
#' \item{`row.names`}{The character vector of rownames for the \linkS4class{SummarizedExperiment} object in which the object is to be embedded. Must be a superset of `rownames(data)`.}
#' }
#' 
#' @section Supported methods:
#' \itemize{
#' \item `pvalue(x)` returns the vector of raw p-values.
#' \item `log2foldchange(x)` returns the vector of log2-fold-change values.
#' \item `log2average(x)` returns the vector of average log2-expression values.
#' }
#' 
#' @author Kevin Rue-Albrecht
#' 
#' @docType methods
#' @name iSEEDESeq2Results-class
#' @aliases
#' iSEEDESeq2Results
#' showAsCell,iSEEDESeq2Results-method
#' pvalue,iSEEDESeq2Results-method
#' log2foldchange,iSEEDESeq2Results-method
#' log2average,iSEEDESeq2Results-method
#' 
#' @examples
#' library(DESeq2)
#' 
#' ##
#' # From DESeq2::DESeq() ----
#' ##
#' 
#' cnts <- matrix(rnbinom(n=1000, mu=100, size=1/0.5), ncol=10)
#' rownames(cnts) <- paste("Gene",1:100)
#' cond <- factor(rep(1:2, each=5))
#' 
#' # object construction
#' dds <- DESeqDataSetFromMatrix(cnts, DataFrame(cond), ~ cond)
#' 
#' # standard analysis
#' dds <- DESeq(dds)
#' res <- results(dds)
#' head(res)
#' 
#' ##
#' # iSEEDESeq2Results ----
#' ##
#' 
#' # Package the results in a iSEEDESeq2Results object
#' iseede_table <- iSEEDESeq2Results(res, row.names=rownames(dds))
#' 
#' # Store the iSEEDESeq2Results object in the SummarizedExperiment rowData
#' rowData(dds)[["iSEEde"]] <- DataFrame(DESeq2=I(iseede_table))
#' 
#' dds
#' 
#' ##
#' # Methods ----
#' ##
#' 
#' head(pvalue(iseede_table))
#' head(log2foldchange(iseede_table))
#' head(average(iseede_table))
NULL

setClass("iSEEDESeq2Results", contains = "DFrame")

#' @export
#' @importFrom methods new
#' @importFrom S4Vectors DataFrame
iSEEDESeq2Results <- function(data, row.names = rownames(data)) {
  df <- DataFrame(row.names=row.names)
  df[rownames(data), colnames(data)] <- data
  new("iSEEDESeq2Results", df)
}

#' @importMethodsFrom S4Vectors showAsCell
setMethod("showAsCell", "iSEEDESeq2Results", function(object) {
  ans <- rep.int("<iSEEDESeq2Results>", nrow(object))
})

#' @export
setMethod("pvalue", "iSEEDESeq2Results", function(x) {
  out <- x[["pvalue"]]
  names(out) <- rownames(x)
  out
})

#' @export
setMethod("log2foldchange", "iSEEDESeq2Results", function(x) {
  out <- x[["log2FoldChange"]]
  names(out) <- rownames(x)
  out
})

#' @export
setMethod("log2average", "iSEEDESeq2Results", function(x) {
  out <- log2(x[["baseMean"]])
  names(out) <- rownames(x)
  out
})
