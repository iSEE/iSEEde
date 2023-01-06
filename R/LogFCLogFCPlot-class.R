#' The LogFCLogFCPlot class
#' 
#' The LogFCLogFCPlot class is a \linkS4class{RowDataPlot} subclass that is dedicated to comparing the log-fold-change value of two contrasts.
#' It retrieves the log-fold change of the two selected contrasts and creates a row-based plot where each point represents a feature.
#' 
#' @docType methods
#' @aliases LogFCLogFCPlot LogFCLogFCPlot-class
#' initialize,LogFCLogFCPlot-method
#' .fullName,LogFCLogFCPlot-method
#' .panelColor,LogFCLogFCPlot-method
#' 
#' @name LogFCLogFCPlot-class
#' 
#' @examples 
#' x <- LogFCLogFCPlot()
#' x[["ContrastNameX"]]
#' x[["ContrastNameX"]] <- "treatment1 vs control"
#' x[["ContrastNameY"]]
#' x[["ContrastNameY"]] <- "treatment2 vs control"
NULL

#' @export
#' @importClassesFrom iSEE RowDotPlot
setClass("LogFCLogFCPlot",
    contains="RowDotPlot",
    slots = c(
        ContrastNameX = "character",
        ContrastNameY = "character"
    )
)

#' @export
#' @importMethodsFrom iSEE .fullName
setMethod(".fullName", "LogFCLogFCPlot", function(x) "LogFC-LogFC plot")

#' @export
#' @importMethodsFrom iSEE .panelColor
setMethod(".panelColor", "LogFCLogFCPlot", function(x) "#DEAE10")

#' @export
#' @importMethodsFrom methods initialize
#' @importFrom methods callNextMethod
setMethod("initialize", "LogFCLogFCPlot", function(.Object,
    ContrastNameX=NA_character_, ContrastNameY=NA_character_, ...)
{
  args <- list(
    ContrastNameX=ContrastNameX,
    ContrastNameY=ContrastNameY,
    ...)
  
  do.call(callNextMethod, c(list(.Object), args))
})

#' @export
#' @importFrom methods new
LogFCLogFCPlot <- function(...) {
  new("LogFCLogFCPlot", ...)
}

#' @importFrom S4Vectors setValidity2
setValidity2("LogFCLogFCPlot", function(object) {
  return(TRUE)
})
