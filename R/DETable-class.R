#' The DETable class
#'
#' The DETable is a \linkS4class{RowTable} subclass that is dedicated to creating a volcano plot.
#' It retrieves the log-fold change and p-value from and creates a row-based plot where each point represents a feature.
#'
#' @docType methods
#' @aliases DETable DETable-class
#' initialize,DETable-method
#' .fullName,DETable-method
#' .panelColor,DETable-method
#'
#' @name DETable-class
#'
#' @examples
#' x <- DETable()
NULL

#' @export
#' @importClassesFrom iSEE RowTable
setClass("DETable",
    contains = "RowTable",
    slots = c(ContrastName = "character")
)

#' @export
#' @importMethodsFrom iSEE .fullName
setMethod(".fullName", "DETable", function(x) "Differential expression table")

#' @export
#' @importMethodsFrom iSEE .panelColor
setMethod(".panelColor", "DETable", function(x) "#DEAE10")


#' @export
#' @importMethodsFrom methods initialize
#' @importFrom methods callNextMethod
setMethod("initialize", "DETable", function(.Object,
                                                ContrastName = NA_character_, ...) {
    args <- list(ContrastName = ContrastName, ...)

    do.call(callNextMethod, c(list(.Object), args))
})

#' @export
#' @importFrom methods new
DETable <- function(...) {
    new("DETable", ...)
}

#' @export
#' @importMethodsFrom iSEE .generateTable
#' @importFrom SummarizedExperiment rowData
setMethod(".generateTable", "DETable", function(x, envir) {
  cmds <- sprintf("tab <- as.data.frame(rowData(se)[['iSEEde']][['%s']])", slot(x, .contrastName))
  if (exists("row_selected", envir = envir, inherits = FALSE)) {
    cmds <- c(cmds, "tab <- tab[unique(unlist(row_selected)), , drop=FALSE]")
  }
  .textEval(cmds, envir)
  cmds
})
