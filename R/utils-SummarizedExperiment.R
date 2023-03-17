#' @export
#'
#' @rdname utils-SummarizedExperiment
embedContrastResultsMethods <- c(
    "limma" = "iSEELimmaResults"
)

#' @export
setMethod("embedContrastResults", "ANY", function(x, se, name, ...) {
    msg <- sprintf(
        "no 'embedContrastResults' method defined for object
      of class %s, consider defining your own.",
        sQuote(class(x))
    )
    stop(paste(strwrap(msg), collapse = "\n"))
})

#' @importFrom SummarizedExperiment rowData rowData<-
#' @importFrom S4Vectors DataFrame
.embed_de_result <- function(x, se, name) {
    iseede_data <- rowData(se)[["iSEEde"]]
    if (is.null(iseede_data)) {
        iseede_data <- DataFrame(row.names = rownames(se))
    }
    if (name %in% names(iseede_data)) {
        msg <- sprintf(
            "Results already exist under name %s.
        Replacing with new results.\n",
            sQuote(name)
        )
        warning(paste(strwrap(msg), collapse = "\n"))
    }
    iseede_data[[name]] <- x
    rowData(se)[["iSEEde"]] <- iseede_data
    se
}

#' @export
#'
#' @param x Object to be embedded.
#' @param se A \linkS4class{SummarizedExperiment} object.
#' @param name Identifier for the embedded object.
#' @param class Class to use for embedding `x`. Only used when `class(x)` does
#' not uniquely identify the package that generated the object.
#' @param ... Arguments passed to and from other methods.
#'
#' @return An updated \linkS4class{SummarizedExperiment} object that contains the
#' embedded object.
#'
#' @rdname utils-SummarizedExperiment
#' @aliases embedContrastResults,data.frame-method
setMethod("embedContrastResults", "data.frame", function(x, se, name, class, ...) {
    if (!class %in% names(embedContrastResultsMethods)) {
        msg <- sprintf(
            "argument %s must be a value in %s,
      for signature %s.",
            sQuote("class"), sQuote("names(embedContrastResultsMethods)"),
            sQuote("x=data.frame")
        )
        stop(paste(strwrap(msg), collapse = "\n"))
    }
    constructor <- get(embedContrastResultsMethods[class])
    res <- constructor(x, row.names = rownames(se))
    embedContrastResults(res, se, name, ...)
})

#' @export
setMethod("embedContrastResults", "iSEELimmaResults", function(x, se, name, ...) {
    .embed_de_result(x, se, name)
})

#' @export
#' @importClassesFrom DESeq2 DESeqResults
setMethod("embedContrastResults", "DESeqResults", function(x, se, name, ...) {
    res <- iSEEDESeq2Results(x, row.names = rownames(se))
    embedContrastResults(res, se, name)
})

#' @export
setMethod("embedContrastResults", "iSEEDESeq2Results", function(x, se, name, ...) {
    .embed_de_result(x, se, name)
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
