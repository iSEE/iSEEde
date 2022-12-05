setClass("iSEELimmaResults", contains = "DFrame")

#' @export
#' @importFrom methods new
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
setMethod("pvalue", "iSEELimmaResults", function(x, ...) {
  out <- x[["P.Value"]]
  names(out) <- rownames(x)
  out
})

#' @export
setMethod("logfc", "iSEELimmaResults", function(x, row.names = rownames(x), ...) {
  out <- x[["logFC"]]
  names(out) <- rownames(x)
  out
})

#' @export
setMethod("average", "iSEELimmaResults", function(x, row.names = rownames(x), ...) {
  out <- x[["AveExpr"]]
  names(out) <- rownames(x)
  out
})
