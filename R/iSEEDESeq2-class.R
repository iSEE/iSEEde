setClass("iSEEDESeq2Results", contains = "DFrame")

#' @export
#' @importFrom methods new
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
setMethod("pvalue", "iSEEDESeq2Results", function(x, ...) {
  out <- x[["pvalue"]]
  names(out) <- rownames(x)
  out
})

#' @export
setMethod("logfc", "iSEEDESeq2Results", function(x, row.names = rownames(x), ...) {
  out <- x[["log2FoldChange"]]
  names(out) <- rownames(x)
  out
})

#' @export
setMethod("average", "iSEEDESeq2Results", function(x, row.names = rownames(x), ...) {
  out <- x[["baseMean"]]
  names(out) <- rownames(x)
  out
})
