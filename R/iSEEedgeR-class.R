#' The iSEEedgeRResults class
#' 
#' The `iSEEedgeRResults` class is used to provide an common interface to differential expression results produced by the \pkg{edgeR} package.
#' It provides methods to access common differential expression statistics (e.g., log fold-change, p-value, average abundance).
#' 
#' This class inherits all its slots directly from its parent class \linkS4class{DataFrame}.
#' 
#' @section Constructor:
#' \code{iSEEedgeRResults(data, row.names = rownames(data))} creates an instance of a `iSEEedgeRResults` class, with:
#' 
#' \describe{
#' \item{`data`}{A `data.frame` produced by `edgeR::topTags()`.}
#' \item{`row.names`}{The character vector of rownames for the \linkS4class{SummarizedExperiment} object in which the object is to be embedded. Must be a superset of `rownames(data)`.}
#' }
#' 
#' @section Supported methods:
#' \itemize{
#' \item `pvalue(x)` returns the vector of raw p-values.
#' \item `logfc(x)` returns the vector of log2-fold-change values.
#' \item `average(x)` returns the vector of average log2-expression values.
#' }
#' 
#' @author Kevin Rue-Albrecht
#' 
#' @docType methods
#' @name iSEEedgeRResults-class
#' @aliases
#' iSEEedgeRResults
#' showAsCell,iSEEedgeRResults-method
#' pvalue,iSEEedgeRResults-method
#' logfc,iSEEedgeRResults-method
#' average,iSEEedgeRResults-method
#' 
#' @examples
#' library(edgeR)
#' library(SummarizedExperiment)
#' 
#' ##
#' # From edgeR::topTags() ----
#' ##
#' 
#' # generate raw counts from NB, create list object
#' y <- matrix(rnbinom(80,size=1,mu=10),nrow=20)
#' d <- DGEList(counts=y,group=rep(1:2,each=2),lib.size=rep(c(1000:1001),2))
#' rownames(d$counts) <- paste("gene",1:nrow(d$counts),sep=".")
#' 
#' # estimate common dispersion and find differences in expression
#' # here we demonstrate the 'exact' methods, but the use of topTags is
#' # the same for a GLM analysis
#' d <- estimateCommonDisp(d)
#' de <- exactTest(d)
#' 
#' # look at top 10
#' res <- topTags(de)
#' 
#' ##
#' # iSEEedgeRResults ----
#' ##
#' 
#' # Simulate the original SummarizedExperiment object
#' se <- SummarizedExperiment(assays = list(counts=d$counts))
#' 
#' # Package the results in a iSEEedgeRResults object
#' iseede_table <- iSEEedgeRResults(res, row.names=rownames(se))
#' 
#' # Store the iSEEedgeRResults object in the SummarizedExperiment rowData
#' rowData(se)[["iSEEde"]] <- DataFrame(edgeR=I(iseede_table))
#' 
#' se
#' 
#' ##
#' # Methods ----
#' ##
#' 
#' head(pvalue(iseede_table))
#' head(logfc(iseede_table))
#' head(average(iseede_table))
NULL

setClass("iSEEedgeRResults", contains = "DFrame")

#' @export
#' @importFrom methods new
#' @importFrom S4Vectors DataFrame
iSEEedgeRResults <- function(data, row.names = rownames(data)) {
  df <- DataFrame(row.names=row.names)
  # Direct coercion to DataFrame fails because it picks up other named slots
  df[rownames(data), colnames(data)] <- as.data.frame(data)
  new("iSEEedgeRResults", df)
}

#' @importMethodsFrom S4Vectors showAsCell
setMethod("showAsCell", "iSEEedgeRResults", function(object) {
  ans <- rep.int("<iSEEedgeRResults>", nrow(object))
})

#' @export
setMethod("pvalue", "iSEEedgeRResults", function(x) {
  out <- x[["PValue"]]
  names(out) <- rownames(x)
  out
})

#' @export
setMethod("logfc", "iSEEedgeRResults", function(x) {
  out <- x[["logFC"]]
  names(out) <- rownames(x)
  out
})

#' @export
setMethod("average", "iSEEedgeRResults", function(x) {
  out <- x[["logCPM"]]
  names(out) <- rownames(x)
  out
})
