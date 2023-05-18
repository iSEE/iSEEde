#' The iSEEedgeRResults class
#'
#' The `iSEEedgeRResults` class is used to provide a common interface to differential expression results produced by the \pkg{edgeR} package.
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
#' \item `embedContrastResults(x, se, name, ...)` embeds `x` in `se` under the identifier `name`. See [`embedContrastResults()`] for more details.
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
#' embedContrastResults,TopTags-method
#' embedContrastResults,iSEEedgeRResults-method
#'
#' @examples
#' library(edgeR)
#' library(SummarizedExperiment)
#'
#' ##
#' # From edgeR::glmLRT() ----
#' ##
#'
#' nlibs <- 3
#' ngenes <- 100
#' dispersion.true <- 0.1
#' 
#' # Make first gene respond to covariate x
#' x <- 0:2
#' design <- model.matrix(~x)
#' beta.true <- cbind(Beta1=2,Beta2=c(2,rep(0,ngenes-1)))
#' mu.true <- 2^(beta.true %*% t(design))
#' 
#' # Generate count data
#' y <- rnbinom(ngenes*nlibs,mu=mu.true,size=1/dispersion.true)
#' y <- matrix(y,ngenes,nlibs)
#' colnames(y) <- c("x0","x1","x2")
#' rownames(y) <- paste("gene",1:ngenes,sep=".")
#' d <- DGEList(y)
#' 
#' # Normalize
#' d <- calcNormFactors(d)
#' 
#' # Fit the NB GLMs
#' fit <- glmFit(d, design, dispersion=dispersion.true)
#' 
#' # Likelihood ratio tests for trend
#' results <- glmLRT(fit, coef=2)
#' tt <- topTags(results)
#'
#' ##
#' # iSEEedgeRResults ----
#' ##
#'
#' # Simulate the original SummarizedExperiment object
#' se <- SummarizedExperiment(assays = list(counts = d$counts))
#'
#' # Embed the edgeR results in the SummarizedExperiment object
#' se <- embedContrastResults(tt, se, name = "edgeR")
#' 
#' ##
#' # Access ----
#' ##
#' 
#' contrastResultsNames(se)
#' contrastResults(se)
#' contrastResults(se, "edgeR")
#'
#' head(pValue(contrastResults(se, "edgeR")))
#' head(log2FoldChange(contrastResults(se, "edgeR")))
#' head(averageLog2(contrastResults(se, "edgeR")))
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

#' @export
#' @importClassesFrom edgeR TopTags
setMethod("embedContrastResults", "TopTags", function(x, se, name, ...) {
  ## Remove other rowData columns that might have been picked up by edgeR:::SE2DGEList()
  x_clean <- x[, c("logFC", "logCPM", "LR", "PValue", "FDR")]
  res <- iSEEedgeRResults(x_clean, row.names = rownames(se))
  embedContrastResults(res, se, name)
})

#' @export
setMethod("embedContrastResults", "iSEEedgeRResults", function(x, se, name, ...) {
  .embed_de_result(x, se, name)
})
