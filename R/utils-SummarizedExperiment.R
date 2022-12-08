#' @export
#' 
#' @rdname utils-SummarizedExperiment
embedResultsMethods <- c(
  "limma" = "iSEELimmaResults"
)

#' @export
setMethod("embedResults", "ANY", function(x, se, name, ...) {
  msg <- sprintf("no 'embedResults' method defined for object
      of class %s, consider defining your own.",
      sQuote(class(x)))
  stop(paste(strwrap(msg), collapse="\n"))
})

#' @importFrom SummarizedExperiment rowData rowData<-
#' @importFrom S4Vectors DataFrame
.embed_de_result <- function(x, se, name) {
  iseede_data <- rowData(se)[["iSEEde"]]
  if (is.null(iseede_data)) {
    iseede_data <- DataFrame(row.names = rownames(se))
  }
  if (name %in% names(iseede_data)) {
    msg <- sprintf("Results already exist under name %s.
        Replacing with new results.\n",
        sQuote(name))
    warning(paste(strwrap(msg), collapse="\n"))
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
#' 
#' @rdname utils-SummarizedExperiment
#' @aliases embedResults,data.frame-method
setMethod("embedResults", "data.frame", function(x, se, name, class, ...) {
  if (!class %in% names(embedResultsMethods)) {
    msg <- sprintf("argument %s must be a value in %s,
      for signature %s.",
      sQuote("class"), sQuote("names(embedResultsMethods)"),
      sQuote("x=data.frame"))
    stop(paste(strwrap(msg), collapse="\n"))
  }
  constructor <- get(embedResultsMethods[class])
  res <- constructor(x, row.names=rownames(se))
  embedResults(res, se, name, ...)
})

#' @export
setMethod("embedResults", "iSEELimmaResults", function(x, se, name, ...) {
  .embed_de_result(x, se, name)
})

#' @export
#' @importClassesFrom DESeq2 DESeqResults
setMethod("embedResults", "DESeqResults", function(x, se, name, ...) {
  res <- iSEEDESeq2Results(x, row.names = rownames(se))
  embedResults(res, se, name)
})

#' @export
setMethod("embedResults", "iSEEDESeq2Results", function(x, se, name, ...) {
  .embed_de_result(x, se, name)
})

#' @export
#' @importClassesFrom edgeR TopTags
setMethod("embedResults", "TopTags", function(x, se, name, ...) {
  res <- iSEEedgeRResults(x, row.names = rownames(se))
  embedResults(res, se, name)
})

#' @export
setMethod("embedResults", "iSEEedgeRResults", function(x, se, name, ...) {
  .embed_de_result(x, se, name)
})
