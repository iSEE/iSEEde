#' @export
setClass("iSEELimmaResults",
         slots = c(data = "data.frame"))

#' @export
#' @importFrom methods new
iSEELimmaResults <- function(data, ...) {
  new("iSEELimmaResults", data=data, ...)
}

#' @export
setMethod("show", "iSEELimmaResults", function(object) {
  cat("class:", class(object), fill = TRUE)
  cat("dim:", paste0(dim(object@data), collapse = " "))
})

#' @export
setMethod("rownames", "iSEELimmaResults", function(x, do.NULL = TRUE, prefix = "row") {
  rownames(x@data)
})

#' @export
setMethod("pvalue", "iSEELimmaResults", function(x, row.names = rownames(x), ...) {
  x@data[row.names, "P.Value"]
})

#' @export
setMethod("logfc", "iSEELimmaResults", function(x, row.names = rownames(x), ...) {
  x@data[row.names, "logFC"]
})

#' @export
setMethod("average", "iSEELimmaResults", function(x, row.names = rownames(x), ...) {
  x@data[row.names, "AveExpr"]
})
