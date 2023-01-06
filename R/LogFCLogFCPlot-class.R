#' The LogFCLogFCPlot class
#' 
#' The LogFCLogFCPlot class is a \linkS4class{RowDataPlot} subclass that is dedicated to comparing the log-fold-change value of two contrasts.
#' It retrieves the log-fold change of the two selected contrasts and creates a row-based plot where each point represents a feature.
#' 
#' @docType methods
#' @aliases LogFCLogFCPlot LogFCLogFCPlot-class
#' initialize,LogFCLogFCPlot-method
#' .fullName,LogFCLogFCPlot-method
#' .generateDotPlotData,LogFCLogFCPlot-method
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

#' @export
#' @importMethodsFrom iSEE .generateDotPlotData
#' @importFrom iSEE .textEval
setMethod(".generateDotPlotData", "LogFCLogFCPlot", function(x, envir) {
  data_cmds <- list()
  
  y_lab <- sprintf("%s (log2FC)", x[[.contrastNameY]])
  
  # NOTE: deparse() automatically adds quotes, AND protects against existing quotes/escapes.
  data_cmds[["y"]] <- c(
    "plot.data <- data.frame(row.names=rownames(se))",
    sprintf("plot.data$Y <- iSEEde::log2FoldChange(rowData(se)[['iSEEde']][['%s']])", x[[.contrastNameY]])
  )
  
  # Prepare X-axis data.
  x_lab <- sprintf("%s (log2FC)", x[[.contrastNameX]])
  data_cmds[["x"]] <- sprintf("plot.data$X <- iSEEde::log2FoldChange(rowData(se)[['iSEEde']][['%s']])", x[[.contrastNameX]])
  
  plot_title <- sprintf("%s vs %s", x[[.contrastNameY]], x[[.contrastNameX]])
  
  data_cmds <- unlist(data_cmds)
  .textEval(data_cmds, envir)
  
  list(commands=data_cmds, labels=list(title=plot_title, X=x_lab, Y=y_lab))
})
