#' @export
setClass("DEPlot", contains="RowDotPlot")

#' @export
setMethod(".fullName", "DEPlot", function(x) "Differential expression plot")

#' @export
setMethod(".panelColor", "DEPlot", function(x) "#DEAE10")

#' @export
#' @importFrom methods callNextMethod
setMethod("initialize", "DEPlot", function(.Object, PValueThreshold=0.05,
                                                LogFCThreshold=0, PValueCorrection="BH", ...)
{
  args <- list(...)
  
  do.call(callNextMethod, c(list(.Object), args))
})

#' @export
#' @importFrom methods new
DEPlot <- function(...) {
  new("DEPlot", ...)
}

setValidity2("DEPlot", function(object) {
  return(TRUE)
})

#' @export
#' @importFrom methods callNextMethod
setMethod(".cacheCommonInfo", "DEPlot", function(x, se) {
  if (!is.null(.getCachedCommonInfo(se, "DEPlot"))) {
    return(se)
  }
  
  se <- callNextMethod()
})

#' @export
#' @importFrom methods callNextMethod
setMethod(".refineParameters", "DEPlot", function(x, se) {
  x <- callNextMethod() # Trigger warnings from base classes.
  if (is.null(x)) {
    return(NULL)
  }
  
  x
})

#' @export
#' @importFrom methods callNextMethod
#' @importFrom shiny selectInput hr
setMethod(".defineDataInterface", "DEPlot", function(x, se, select_info) {
  plot_name <- .getEncodedName(x)
  input_FUN <- function(field) paste0(plot_name, "_", field)
  
  callNextMethod()
})

#' @export
setMethod(".generateDotPlotData", "DEPlot", function(x, envir) {
  data_cmds <- list()
  
  y_lab <- "-log(p-value)"
  
  data_cmds[["edgeR"]] <- "de_table <- metadata(se)[['iSEEde']][['dextrt vs dexuntrt']]"
  
  # NOTE: deparse() automatically adds quotes, AND protects against existing quotes/escapes.
  data_cmds[["y"]] <- c(
    sprintf(
      "plot.data <- data.frame(Y=-log10(de_table[, 'P.Value']), row.names=rownames(de_table));"
    )
  )
  
  # Prepare X-axis data.
  x_lab <- "Fold-change"
  x_title <- "vs fold-change"
  data_cmds[["x"]] <- "plot.data$X <- de_table[, 'dextrt'] - de_table[, 'dexuntrt']"
  
  plot_title <- sprintf("dextrt vs dexuntrt", y_lab, x_title)
  
  data_cmds <- unlist(data_cmds)
  .textEval(data_cmds, envir)
  print(envir$plot.data)
  list(commands=data_cmds, labels=list(title=plot_title, X=x_lab, Y=y_lab))
})

