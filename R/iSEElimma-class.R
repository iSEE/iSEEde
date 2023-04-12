#' The iSEELimmaResults class
#'
#' The `iSEELimmaResults` class is used to provide an common interface to differential expression results produced by the \pkg{limma} package.
#' It provides methods to access common differential expression statistics (e.g., log fold-change, p-value, log2 average abundance).
#'
#' This class inherits all its slots directly from its parent class \linkS4class{DataFrame}.
#'
#' @section Constructor:
#' \code{iSEELimmaResults(data, row.names = rownames(data))} creates an instance of a `iSEELimmaResults` class, with:
#'
#' \describe{
#' \item{`data`}{A `data.frame` produced by `limma::topTable()`.}
#' \item{`row.names`}{The character vector of rownames for the \linkS4class{SummarizedExperiment} object in which the object is to be embedded. Must be a superset of `rownames(data)`.}
#' }
#'
#' @section Supported methods:
#' \itemize{
#' \item `embedContrastResults(x, se, name, class = "limma", ...)` embeds `x` in `se` under the identifier `name`. See [`embedContrastResults()`] for more details.
#' \item `pValue(x)` returns the vector of raw p-values.
#' \item `log2FoldChange(x)` returns the vector of log2-fold-change values.
#' \item `averageLog2(x)` returns the vector of average log2-expression values.
#' }
#'
#' @author Kevin Rue-Albrecht
#'
#' @docType methods
#' @name iSEELimmaResults-class
#' @aliases
#' iSEELimmaResults
#' showAsCell,iSEELimmaResults-method
#' pValue,iSEELimmaResults-method
#' log2FoldChange,iSEELimmaResults-method
#' averageLog2,iSEELimmaResults-method
#' embedContrastResults,iSEELimmaResults-method
#'
#' @examples
#' library(limma)
#' library(SummarizedExperiment)
#'
#' ##
#' # From limma::lmFit() ----
#' ##
#'
#' sd <- 0.3 * sqrt(4 / rchisq(100, df = 4))
#' y <- matrix(rnorm(100 * 6, sd = sd), 100, 6)
#' rownames(y) <- paste("Gene", 1:100)
#' y[1:2, 4:6] <- y[1:2, 4:6] + 2
#' design <- cbind(Grp1 = 1, Grp2vs1 = c(0, 0, 0, 1, 1, 1))
#'
# Ordinary fit
#' fit <- lmFit(y, design)
#' fit <- eBayes(fit)
#' tt <- topTable(fit, coef = 2)
#' head(tt)
#'
#' ##
#' # iSEELimmaResults ----
#' ##
#'
#' # Simulate the original SummarizedExperiment object
#' se <- SummarizedExperiment(assays = list(counts = y))
#'
#' # Embed the Limma-Voom results in the SummarizedExperiment object
#' se <- embedContrastResults(tt, se, name = "Limma-Voom", class = "limma")
#' 
#' ##
#' # Access ----
#' ##
#' 
#' contrastResultsNames(se)
#' contrastResults(se)
#' contrastResults(se, "Limma-Voom")
#'
#' head(pValue(contrastResults(se, "Limma-Voom")))
#' head(log2FoldChange(contrastResults(se, "Limma-Voom")))
#' head(averageLog2(contrastResults(se, "Limma-Voom")))
NULL

setClass("iSEELimmaResults", contains = "DFrame")

#' @export
#' @importFrom methods new
#' @importFrom S4Vectors DataFrame
iSEELimmaResults <- function(data, row.names = rownames(data)) {
    df <- DataFrame(row.names = row.names)
    df[rownames(data), colnames(data)] <- data
    new("iSEELimmaResults", df)
}

#' @importFrom S4Vectors setValidity2
setValidity2("iSEELimmaResults", function(.Object) {
    msg <- character(0)

    column_names <- c("logFC", "AveExpr", "P.Value")
    for (name in column_names) {
        if (!name %in% colnames(.Object)) {
            msg <- c(msg, sprintf("'%s' must exist in colnames(.Object)", name))
        }
    }

    if (length(msg) > 0) {
        return(msg)
    }
    TRUE
})

#' @importMethodsFrom S4Vectors showAsCell
setMethod("showAsCell", "iSEELimmaResults", function(object) {
    ans <- rep.int("<iSEELimmaResults>", nrow(object))
    ans
})

#' @export
setMethod("pValue", "iSEELimmaResults", function(x) {
    out <- x[["P.Value"]]
    names(out) <- rownames(x)
    out
})

#' @export
setMethod("log2FoldChange", "iSEELimmaResults", function(x) {
    out <- x[["logFC"]]
    names(out) <- rownames(x)
    out
})

#' @export
setMethod("averageLog2", "iSEELimmaResults", function(x) {
    out <- x[["AveExpr"]]
    names(out) <- rownames(x)
    out
})

#' @export
setMethod("embedContrastResults", "iSEELimmaResults", function(x, se, name, class, ...) {
  .embed_de_result(x, se, name)
})
