# embedContrastResultsMethods ----

#' @export
#'
#' @rdname utils-SummarizedExperiment
#' @format `embedContrastResultsMethods`: Named character vector mapping keywords to class names designed to store differential expression results.
embedContrastResultsMethods <- c(
    "limma" = "iSEELimmaResults"
)

# embedContrastResults ----

#' @export
#'
#' @rdname utils-SummarizedExperiment
#' @aliases embedContrastResults,ANY-method
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
        iseede_data <- DataFrame(row.names = seq_len(nrow(se)))
    }
    if (name %in% names(iseede_data)) {
        msg <- sprintf(
            "Results already exist under name %s.
        Replacing with new results.\n",
            sQuote(name)
        )
        warning(paste(strwrap(msg), collapse = "\n"))
    }
    # reorder results to match rownames(se)
    x <- x[rownames(se), , drop = FALSE]
    # remove rownames of embedded results; use rownames(se) instead
    rownames(x) <- NULL
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
    # rownames(se) ensures that results are embedded in matching order
    res <- constructor(x, row.names = rownames(se))
    embedContrastResults(res, se, name, ...)
})

#' @export
#'
#' @rdname utils-SummarizedExperiment
setMethod("embedContrastResults", "iSEELimmaResults", function(x, se, name, ...) {
    .embed_de_result(x, se, name)
})

#' @export
#'
#' @rdname utils-SummarizedExperiment
#' @importClassesFrom DESeq2 DESeqResults
setMethod("embedContrastResults", "DESeqResults", function(x, se, name, ...) {
    res <- iSEEDESeq2Results(x, row.names = rownames(se))
    embedContrastResults(res, se, name)
})

#' @export
#'
#' @rdname utils-SummarizedExperiment
setMethod("embedContrastResults", "iSEEDESeq2Results", function(x, se, name, ...) {
    .embed_de_result(x, se, name)
})

#' @export
#'
#' @rdname utils-SummarizedExperiment
#' @importClassesFrom edgeR TopTags
setMethod("embedContrastResults", "TopTags", function(x, se, name, ...) {
    ## Remove other rowData columns that might have been picked up by edgeR:::SE2DGEList()
    x_clean <- x[, c("logFC", "logCPM", "LR", "PValue", "FDR")]
    res <- iSEEedgeRResults(x_clean, row.names = rownames(se))
    embedContrastResults(res, se, name)
})


#' @export
#'
#' @rdname utils-SummarizedExperiment
setMethod("embedContrastResults", "iSEEedgeRResults", function(x, se, name, ...) {
    .embed_de_result(x, se, name)
})

# contrastResultsNames ----

#' @export
#' @rdname contrastResults
contrastResultsNames <- function(object){
    colnames(rowData(object)[["iSEEde"]])
}

#' Extract contrast results embedded in a SummarizedExperiment object
#' 
#' @description
#' `contrastResults` returns either all contrasts results stored in `object` or a single contrast result by name.
#' 
#' `contrastResultsNames` returns the names of contrast results embedded in `object`.
#'
#' @param object A [SummarizedExperiment-class] object.
#' @param name Name of a single contrast result name to extract.
#' Use `contrastResultsNames(object)` to list available names.
#'
#' @return
#' For `contrastResultsNames`: the names of embedded contrast results available.
#' 
#' For `contrastResults`: a `DataFrame` of differential expression statistics.
#' 
#' If `name` is missing, a nested [`DataFrame-class`] in which each column contains the results of a single contrast.
#' If `name` is given, a [`DataFrame-class`] that contains the results of a single contrast.
#' 
#' @export
#'
#' @examples
#' library("iSEEde")
#' library("airway")
#' library("DESeq2")
#' library("iSEE")
#' 
#' ##
#' # Example data ----
#' ##
#' 
#' data("airway")
#' airway$dex <- relevel(airway$dex, "untrt")
#' 
#' dds <- DESeqDataSet(airway, ~ 0 + dex + cell)
#' 
#' dds <- DESeq(dds)
#' res_deseq2 <- results(dds, contrast = list("dextrt", "dexuntrt"))
#' airway <- embedContrastResults(res_deseq2, airway, name = "dex: trt vs untrt")
#' 
#' ##
#' # List result names ---
#' ##
#' 
#' contrastResultsNames(airway)
#' 
#' ##
#' # Extract results ---
#' ##
#' 
#' contrastResults(airway)
#' contrastResults(airway, "dex: trt vs untrt")
contrastResults <- function(object, name) {
    results_df <- rowData(object)[["iSEEde"]]
    
    if (missing(name)) {
        if (!is.null(results_df)) {
            rownames(results_df) <- rownames(object)
        }
        return(results_df)
    }
    
    if (!name %in% colnames(results_df)) {
        msg <- sprintf(
            "'%s' is not a valid contrast result name,
      use contrastResultsNames(object) to list valid names.",
      name
        )
        stop(paste(strwrap(msg), collapse = "\n"))
    }
    
    results_df <- results_df[[name]]
    rownames(results_df) <- rownames(object)
    return(results_df)
    
}
