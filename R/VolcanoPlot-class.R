#' The VolcanoPlot class
#' 
#' The VolcanoPlot is a \linkS4class{RowDataPlot} subclass that is dedicated to creating a volcano plot.
#' It retrieves the log-fold change and p-value from and creates a row-based plot where each point represents a feature.
#' 
#' @docType methods
#' @aliases VolcanoPlot VolcanoPlot-class
#' 
#' @name VolcanoPlot-class
NULL

#' @export
#' @importClassesFrom iSEE RowDotPlot
setClass("VolcanoPlot", contains="RowDotPlot",
         slots = c(ContrastName = "character"))

#' @export
#' @importMethodsFrom iSEE .fullName
setMethod(".fullName", "VolcanoPlot", function(x) "Volcano plot")

#' @export
#' @importMethodsFrom iSEE .panelColor
setMethod(".panelColor", "VolcanoPlot", function(x) "#DEAE10")

#' @export
#' @importFrom methods callNextMethod
setMethod("initialize", "VolcanoPlot", function(.Object,
    ContrastName=NA_character_, ...)
{
  args <- list(ContrastName=ContrastName, ...)
  
  do.call(callNextMethod, c(list(.Object), args))
})

#' @export
#' @importFrom methods new
VolcanoPlot <- function(...) {
  new("VolcanoPlot", ...)
}

#' @importFrom S4Vectors setValidity2
setValidity2("VolcanoPlot", function(object) {
  return(TRUE)
})

#' @export
#' @importMethodsFrom iSEE .cacheCommonInfo
#' @importFrom iSEE .setCachedCommonInfo
#' @importFrom methods callNextMethod
setMethod(".cacheCommonInfo", "VolcanoPlot", function(x, se) {
  if (!is.null(.getCachedCommonInfo(se, "VolcanoPlot"))) {
    return(se)
  }
  
  se <- callNextMethod()
  
  contrast_names <- names(metadata(se)[["iSEEde"]])

  .setCachedCommonInfo(se, "VolcanoPlot", valid.contrast.names = contrast_names)
})

#' @export
#' @importMethodsFrom iSEE .refineParameters
#' @importFrom iSEE .replaceMissingWithFirst
#' @importFrom methods callNextMethod
setMethod(".refineParameters", "VolcanoPlot", function(x, se) {
  x <- callNextMethod() # Trigger warnings from base classes.
  if (is.null(x)) {
    return(NULL)
  }
  
  contrast_names <- .getCachedCommonInfo(se, "VolcanoPlot")$valid.contrast.names
  x <- .replaceMissingWithFirst(x, .contrastName, contrast_names)
  
  x
})

#' @export
#' @importMethodsFrom iSEE .defineDataInterface
#' @importFrom methods callNextMethod
#' @importFrom shiny hr
#' @importFrom iSEE .selectInput.iSEE
setMethod(".defineDataInterface", "VolcanoPlot", function(x, se, select_info) {
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
  
  cached <- .getCachedCommonInfo(se, "VolcanoPlot")
  
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
setMethod(".generateDotPlotData", "VolcanoPlot", function(x, envir) {
  data_cmds <- list()
  
  y_lab <- "-log10(P.Value)"
  
  data_cmds[["edgeR"]] <- sprintf("de_table <- metadata(se)[['iSEEde']][['%s']]", x[[.contrastName]])
  
  # NOTE: deparse() automatically adds quotes, AND protects against existing quotes/escapes.
  data_cmds[["y"]] <- c(
    sprintf(
      "plot.data <- data.frame(Y=-log10(de_table[, 'P.Value']), row.names=rownames(de_table));"
    )
  )
  
  # Prepare X-axis data.
  x_lab <- "logFC"
  data_cmds[["x"]] <- "plot.data$X <- de_table[, 'logFC']"
  
  plot_title <- x[[.contrastName]]
  
  data_cmds <- unlist(data_cmds)
  .textEval(data_cmds, envir)
  print(envir$plot.data)
  list(commands=data_cmds, labels=list(title=plot_title, X=x_lab, Y=y_lab))
})
