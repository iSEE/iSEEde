#' The LogFCLogFCPlot class
#' 
#' The LogFCLogFCPlot class is a \linkS4class{RowDataPlot} subclass that is dedicated to comparing the log-fold-change value of two contrasts.
#' It retrieves the log-fold change of the two selected contrasts and creates a row-based plot where each point represents a feature.
#' 
#' @section Slot overview:
#' The following slots control the test procedure:
#' \itemize{
#' \item `ContrastNameX`, a character scalar indicating the name of the contrast to display on the x-axis.
#' \item `ContrastNameY`, a character scalar indicating the name of the contrast to display on the y-axis.
#' }
#' 
#' In addition, this class inherits all slots from its parent [RowDotPlot-class], [DotPlot-class], and [Panel-class] classes.
#' 
#' @docType methods
#' @aliases LogFCLogFCPlot LogFCLogFCPlot-class
#' initialize,LogFCLogFCPlot-method
#' .cacheCommonInfo,LogFCLogFCPlot-method
#' .createObservers,LogFCLogFCPlot-method
#' .defineDataInterface,LogFCLogFCPlot-method
#' .fullName,LogFCLogFCPlot-method
#' .generateDotPlotData,LogFCLogFCPlot-method
#' .panelColor,LogFCLogFCPlot-method
#' .refineParameters,LogFCLogFCPlot-method
#' 
#' @name LogFCLogFCPlot-class
#' 
#' @examples 
#' x <- LogFCLogFCPlot()
#' x
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
#' @importMethodsFrom iSEE .cacheCommonInfo
#' @importFrom iSEE .getCachedCommonInfo .setCachedCommonInfo
#' @importFrom methods callNextMethod
#' @importFrom SummarizedExperiment rowData
setMethod(".cacheCommonInfo", "LogFCLogFCPlot", function(x, se) {
  if (!is.null(.getCachedCommonInfo(se, "LogFCLogFCPlot"))) {
    return(se)
  }
  
  se <- callNextMethod()
  
  contrast_names <- contrastResultsNames(se)
  
  .setCachedCommonInfo(se, "LogFCLogFCPlot", valid.contrast.names = contrast_names)
})

#' @export
#' @importMethodsFrom iSEE .refineParameters
#' @importFrom iSEE .getCachedCommonInfo .replaceMissingWithFirst
#' @importFrom methods callNextMethod
setMethod(".refineParameters", "LogFCLogFCPlot", function(x, se) {
  x <- callNextMethod() # Trigger warnings from base classes.
  if (is.null(x)) {
    return(NULL)
  }
  
  contrast_names <- .getCachedCommonInfo(se, "LogFCLogFCPlot")$valid.contrast.names
  x <- .replaceMissingWithFirst(x, .contrastNameX, contrast_names)
  x <- .replaceMissingWithFirst(x, .contrastNameY, contrast_names)
  
  x
})

#' @export
#' @importMethodsFrom iSEE .createObservers
#' @importFrom iSEE .getEncodedName .createProtectedParameterObservers
#' @importFrom methods callNextMethod
setMethod(".createObservers", "LogFCLogFCPlot", function(x, se, input, session, pObjects, rObjects) {
    callNextMethod()

    plot_name <- .getEncodedName(x)

    .createProtectedParameterObservers(plot_name,
        fields=c(.contrastNameX, .contrastNameY),
        input=input, pObjects=pObjects, rObjects=rObjects)
    
    invisible(NULL)
})

#' @export
#' @importMethodsFrom iSEE .defineDataInterface
#' @importFrom methods callNextMethod
#' @importFrom shiny hr
#' @importFrom iSEE .addSpecificTour .getCachedCommonInfo .getEncodedName
#' .selectInput.iSEE
setMethod(".defineDataInterface", "LogFCLogFCPlot", function(x, se, select_info) {
  plot_name <- .getEncodedName(x)
  input_FUN <- function(field) paste0(plot_name, "_", field)
  # nocov start
  .addSpecificTour(class(x), .contrastNameX, function(plot_name) {
    data.frame(
      rbind(
        c(
          element=paste0("#", plot_name, "_", sprintf("%s + .selectize-control", .contrastNameX)),
          intro="Here, we select the name of the contrast to visualise on the x-axis amongst the choice of differential expression results available."
        )
      )
    )
  })
  .addSpecificTour(class(x), .contrastNameY, function(plot_name) {
    data.frame(
      rbind(
        c(
          element=paste0("#", plot_name, "_", sprintf("%s + .selectize-control", .contrastNameY)),
          intro="Here, we select the name of the contrast to visualise on the x-axis amongst the choice of differential expression results available."
        )
      )
    )
  })
  # nocov end
  cached <- .getCachedCommonInfo(se, "LogFCLogFCPlot")
  
  extra_inputs <- list(
    .selectInput.iSEE(x, .contrastNameX, 
                      label="Contrast (X):",
                      selected=x[[.contrastNameX]],
                      choices=cached$valid.contrast.names),
    .selectInput.iSEE(x, .contrastNameY, 
                      label="Contrast (Y):",
                      selected=x[[.contrastNameY]],
                      choices=cached$valid.contrast.names)
  )
  
  c(callNextMethod(), 
    list(hr()), 
    extra_inputs
  )
})

#' @export
#' @importMethodsFrom iSEE .generateDotPlotData
#' @importFrom iSEE .textEval
setMethod(".generateDotPlotData", "LogFCLogFCPlot", function(x, envir) {
  data_cmds <- list()
  
  y_lab <- parse(text = paste(
    dQuote(x[[.contrastNameY]], FALSE),
    dQuote(" (", FALSE),
    "log[2]",
    dQuote(" fold change)", FALSE),
    sep = "*"))
  
  # NOTE: deparse() automatically adds quotes, AND protects against existing quotes/escapes.
  data_cmds[["y"]] <- c(
    "plot.data <- data.frame(row.names=rownames(se))",
    sprintf("plot.data$Y <- iSEEde::log2FoldChange(contrastResults(se, '%s'))", x[[.contrastNameY]])
  )
  
  # Prepare X-axis data.
  x_lab <- parse(text = paste(
    dQuote(x[[.contrastNameX]], FALSE),
    dQuote(" (", FALSE),
    "log[2]",
    dQuote(" fold change)", FALSE),
    sep = "*"))
  data_cmds[["x"]] <- sprintf("plot.data$X <- iSEEde::log2FoldChange(contrastResults(se, '%s'))", x[[.contrastNameX]])
  
  plot_title <- sprintf("%s vs %s", x[[.contrastNameY]], x[[.contrastNameX]])
  
  data_cmds <- unlist(data_cmds)
  .textEval(data_cmds, envir)
  
  list(commands=data_cmds, labels=list(title=plot_title, X=x_lab, Y=y_lab))
})
