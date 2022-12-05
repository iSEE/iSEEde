#' The MAPlot class
#' 
#' The MAPlot is a \linkS4class{RowDataPlot} subclass that is dedicated to creating a volcano plot.
#' It retrieves the log-fold change and p-value from and creates a row-based plot where each point represents a feature.
#' 
#' @docType methods
#' @aliases MAPlot MAPlot-class
#' 
#' @name MAPlot-class
NULL

#' @export
#' @importClassesFrom iSEE RowDotPlot
setClass("MAPlot", contains="RowDotPlot",
         slots = c(ContrastName = "character"))

#' @export
#' @importMethodsFrom iSEE .fullName
setMethod(".fullName", "MAPlot", function(x) "MA plot")

#' @export
#' @importMethodsFrom iSEE .panelColor
setMethod(".panelColor", "MAPlot", function(x) "#DEAE10")

#' @export
#' @importFrom methods callNextMethod
setMethod("initialize", "MAPlot", function(.Object,
                                                ContrastName=NA_character_, ...)
{
  args <- list(ContrastName=ContrastName, ...)
  
  do.call(callNextMethod, c(list(.Object), args))
})

#' @export
#' @importFrom methods new
MAPlot <- function(...) {
  new("MAPlot", ...)
}

#' @importFrom S4Vectors setValidity2
setValidity2("MAPlot", function(object) {
  return(TRUE)
})

#' @export
#' @importMethodsFrom iSEE .cacheCommonInfo
#' @importFrom iSEE .setCachedCommonInfo
#' @importFrom methods callNextMethod
setMethod(".cacheCommonInfo", "MAPlot", function(x, se) {
  if (!is.null(.getCachedCommonInfo(se, "MAPlot"))) {
    return(se)
  }
  
  se <- callNextMethod()
  
  contrast_names <- names(metadata(se)[["iSEEde"]])
  
  .setCachedCommonInfo(se, "MAPlot", valid.contrast.names = contrast_names)
})

#' @export
#' @importMethodsFrom iSEE .refineParameters
#' @importFrom iSEE .replaceMissingWithFirst
#' @importFrom methods callNextMethod
setMethod(".refineParameters", "MAPlot", function(x, se) {
  x <- callNextMethod() # Trigger warnings from base classes.
  if (is.null(x)) {
    return(NULL)
  }
  
  contrast_names <- .getCachedCommonInfo(se, "MAPlot")$valid.contrast.names
  x <- .replaceMissingWithFirst(x, .contrastName, contrast_names)
  
  x
})

#' @export
#' @importMethodsFrom iSEE .defineDataInterface
#' @importFrom methods callNextMethod
#' @importFrom shiny hr
#' @importFrom iSEE .selectInput.iSEE
setMethod(".defineDataInterface", "MAPlot", function(x, se, select_info) {
  plot_name <- .getEncodedName(x)
  input_FUN <- function(field) paste0(plot_name, "_", field)
  
  .addSpecificTour(class(x), .contrastName, function(plot_name) {
    data.frame(
      rbind(
        c(
          element=paste0("#", plot_name, "_", sprintf("%s + .selectize-control", .contrastName)),
          intro="Here, we select the name of the contrast to visualise amongst the choice of differential expression results available."
        )
      )
    )
  })
  
  cached <- .getCachedCommonInfo(se, "MAPlot")
  
  extra_inputs <- list(
    .selectInput.iSEE(x, .contrastName, 
                      label="Contrast:",
                      selected=x[[.contrastName]],
                      choices=cached$valid.contrast.names)
  )
  
  c(callNextMethod(), 
    list(hr()), 
    extra_inputs
  )
})

#' @export
#' @importMethodsFrom iSEE .generateDotPlotData
setMethod(".generateDotPlotData", "MAPlot", function(x, envir) {
  data_cmds <- list()
  
  y_lab <- "logFC"
  
  data_cmds[["edgeR"]] <- sprintf("de_table <- metadata(se)[['iSEEde']][['%s']]", x[[.contrastName]])
  
  # NOTE: deparse() automatically adds quotes, AND protects against existing quotes/escapes.
  data_cmds[["y"]] <- c(
    sprintf(
      "plot.data <- data.frame(Y=de_table[, 'logFC'], row.names=rownames(de_table));"
    )
  )
  
  # Prepare X-axis data.
  x_lab <- "AveExpr"
  data_cmds[["x"]] <- "plot.data$X <- de_table[, 'AveExpr']"
  
  plot_title <- x[[.contrastName]]
  
  data_cmds <- unlist(data_cmds)
  .textEval(data_cmds, envir)
  
  list(commands=data_cmds, labels=list(title=plot_title, X=x_lab, Y=y_lab))
})
