#' The iSEELimmaResults class
#' 
#' The `iSEELimmaResults` class is used to provide an common interface to differential expression results produced by the \pkg{limma} package.
#' It provides methods to access common differential expression statistics (e.g., log fold-change, p-value, average abundance).
#' 
#' This class inherits all its slots directly from its parent class \linkS4class{DataFrame}.
#' 
#' @section Constructor:
#' \code{iSEELimmaResults(data, row.names = rownames(data))} creates an instance of a `iSEELimmaResults` class, with:
#' 
#' \describe{
#' \item{`data`}{A `data.frame` produced by `limma::topTable()`.}
#' \item{`row.names`}{The character vector of rownames for the [`SummarizedExperiment-class`] object in which the object is to be embedded. Must be a superset of `rownames(data)`.}
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
#' @name iSEELimmaResults-class
#' @aliases
#' iSEELimmaResults
#' showAsCell,iSEELimmaResults-method
#' pvalue,iSEELimmaResults-method
#' logfc,iSEELimmaResults-method
#' average,iSEELimmaResults-method
#' 
#' @examples
#' library(limma)
#' library(SummarizedExperiment)
#' 
#' ##
#' # From limma::lmFit() ----
#' ##
#' 
#' sd <- 0.3*sqrt(4/rchisq(100,df=4))
#' y <- matrix(rnorm(100*6,sd=sd),100,6)
#' rownames(y) <- paste("Gene",1:100)
#' y[1:2,4:6] <- y[1:2,4:6] + 2
#' design <- cbind(Grp1=1, Grp2vs1=c(0,0,0,1,1,1))
#' 
# Ordinary fit
#' fit <- lmFit(y,design)
#' fit <- eBayes(fit)
#' tt <- topTable(fit, coef=2)
#' head(tt)
#' 
#' ##
#' # iSEELimmaResults ----
#' ##
#' 
#' # Simulate the original SummarizedExperiment object
#' se <- SummarizedExperiment(assays = y)
#' 
#' # Package the results in a iSEELimmaResults object
#' iseede_table <- iSEELimmaResults(tt, row.names=rownames(y))
#' 
#' # Store the iSEELimmaResults object in the SummarizedExperiment rowData
#' rowData(se)[["iSEEde"]] <- DataFrame(limma=I(iseede_table))
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

setClass("iSEELimmaResults", contains = "DFrame")

#' @export
#' @importFrom methods new
#' @importFrom S$Vectors DataFrame
iSEELimmaResults <- function(data, row.names = rownames(data)) {
  df <- DataFrame(row.names=row.names)
  df[rownames(data), colnames(data)] <- data
  new("iSEELimmaResults", df)
}

#' @importMethodsFrom S4Vectors showAsCell
setMethod("showAsCell", "iSEELimmaResults", function(object) {
  ans <- rep.int("<iSEELimmaResults>", nrow(object))
})

#' @export
setMethod("pvalue", "iSEELimmaResults", function(x) {
  out <- x[["P.Value"]]
  names(out) <- rownames(x)
  out
})

#' @export
setMethod("logfc", "iSEELimmaResults", function(x) {
  out <- x[["logFC"]]
  names(out) <- rownames(x)
  out
})

#' @export
setMethod("average", "iSEELimmaResults", function(x) {
  out <- x[["AveExpr"]]
  names(out) <- rownames(x)
  out
})
