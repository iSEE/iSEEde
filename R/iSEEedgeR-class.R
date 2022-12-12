#' The iSEEedgeRResults class
#'
#' The `iSEEedgeRResults` class is used to provide an common interface to differential expression results produced by the \pkg{edgeR} package.
#' It provides methods to access common differential expression statistics (e.g., log fold-change, p-value, log2 average abundance).
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
#' \item `embedResults(x, se, name, ...)` embeds `x` in the column `name` of `rowData(se)[["iSEEde"]]`.
#' \item `pValue(x)` returns the vector of raw p-values.
#' \item `log2FoldChange(x)` returns the vector of log2-fold-change values.
#' \item `averageLog2(x)` returns the vector of average log2-expression values.
#' }
#'
#' @author Kevin Rue-Albrecht
#'
#' @docType methods
#' @name iSEEedgeRResults-class
#' @aliases
#' iSEEedgeRResults
#' showAsCell,iSEEedgeRResults-method
#' pValue,iSEEedgeRResults-method
#' log2FoldChange,iSEEedgeRResults-method
#' averageLog2,iSEEedgeRResults-method
#' embedResults,TopTags-method
#' embedResults,iSEEedgeRResults-method
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
#' y <- matrix(rnbinom(80, size = 1, mu = 10), nrow = 20)
#' d <- DGEList(counts = y, group = rep(1:2, each = 2), lib.size = rep(c(1000:1001), 2))
#' rownames(d$counts) <- paste("gene", 1:nrow(d$counts), sep = ".")
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
#' se <- SummarizedExperiment(assays = list(counts = d$counts))
#'
#' # Package the results in a iSEEedgeRResults object
#' iseede_table <- iSEEedgeRResults(res, row.names = rownames(se))
#'
#' # Store the iSEEedgeRResults object in the SummarizedExperiment rowData
#' rowData(se)[["iSEEde"]] <- DataFrame(edgeR = I(iseede_table))
#'
#' se
#'
#' ##
#' # Methods ----
#' ##
#'
#' head(pValue(iseede_table))
#' head(log2FoldChange(iseede_table))
#' head(averageLog2(iseede_table))
NULL

setClass("iSEEedgeRResults", contains = "DFrame")

#' @export
#' @importFrom methods new is
#' @importFrom S4Vectors DataFrame
iSEEedgeRResults <- function(data, row.names = rownames(data)) {
    stopifnot(is(data, "TopTags"))
    df <- DataFrame(row.names = row.names)
    # Direct coercion to DataFrame fails because it picks up other named slots
    df[rownames(data), colnames(data)] <- as.data.frame(data)
    new("iSEEedgeRResults", df)
}

#' @importFrom S4Vectors setValidity2
setValidity2("iSEEedgeRResults", function(.Object) {
    msg <- character(0)

    column_names <- c("logFC", "logCPM", "PValue")
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
setMethod("showAsCell", "iSEEedgeRResults", function(object) {
    ans <- rep.int("<iSEEedgeRResults>", nrow(object))
    ans
})

#' @export
setMethod("pValue", "iSEEedgeRResults", function(x) {
    out <- x[["PValue"]]
    names(out) <- rownames(x)
    out
})

#' @export
setMethod("log2FoldChange", "iSEEedgeRResults", function(x) {
    out <- x[["logFC"]]
    names(out) <- rownames(x)
    out
})

#' @export
setMethod("averageLog2", "iSEEedgeRResults", function(x) {
    out <- x[["logCPM"]]
    names(out) <- rownames(x)
    out
})
