#' The DETable class
#'
#' The DETable class is a \linkS4class{RowTable} subclass that is dedicated to creating a volcano plot.
#' It retrieves the log-fold change and p-value from and creates a row-based plot where each point represents a feature.
#'
#' @docType methods
#' @aliases DETable DETable-class
#' initialize,DETable-method
#' .cacheCommonInfo,DETable-method
#' .createObservers,LogFCLogFCPlot-method
#' .defineDataInterface,LogFCLogFCPlot-method
#' .fullName,DETable-method
#' .generateTable,DETable-method
#' .panelColor,DETable-method
#' .refineParameters,DETable-method
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

#' @importFrom S4Vectors setValidity2
setValidity2("DETable", function(object) {
    return(TRUE)
})

#' @export
#' @importMethodsFrom iSEE .cacheCommonInfo
#' @importFrom iSEE .getCachedCommonInfo .setCachedCommonInfo
#' @importFrom methods callNextMethod
#' @importFrom SummarizedExperiment rowData
setMethod(".cacheCommonInfo", "DETable", function(x, se) {
    if (!is.null(.getCachedCommonInfo(se, "DETable"))) {
        return(se)
    }

    se <- callNextMethod()

    contrast_names <- colnames(rowData(se)[["iSEEde"]])

    .setCachedCommonInfo(se, "DETable", valid.contrast.names = contrast_names)
})

#' @export
#' @importMethodsFrom iSEE .refineParameters
#' @importFrom iSEE .getCachedCommonInfo .replaceMissingWithFirst
#' @importFrom methods callNextMethod
setMethod(".refineParameters", "DETable", function(x, se) {
    x <- callNextMethod() # Trigger warnings from base classes.
    if (is.null(x)) {
        return(NULL)
    }

    contrast_names <- .getCachedCommonInfo(se, "DETable")$valid.contrast.names
    x <- .replaceMissingWithFirst(x, .contrastName, contrast_names)

    x
})

#' @export
#' @importMethodsFrom iSEE .createObservers
#' @importFrom iSEE .getEncodedName .createProtectedParameterObservers
#' @importFrom methods callNextMethod
setMethod(".createObservers", "DETable", function(x, se, input, session, pObjects, rObjects) {
    callNextMethod()

    plot_name <- .getEncodedName(x)

    .createProtectedParameterObservers(plot_name,
        fields = c(.contrastName),
        input = input, pObjects = pObjects, rObjects = rObjects
    )

    invisible(NULL)
})

#' @export
#' @importMethodsFrom iSEE .defineDataInterface
#' @importFrom methods callNextMethod
#' @importFrom shiny hr
#' @importFrom iSEE .addSpecificTour .getCachedCommonInfo .getEncodedName
#' .selectInput.iSEE
setMethod(".defineDataInterface", "DETable", function(x, se, select_info) {
    plot_name <- .getEncodedName(x)
    input_FUN <- function(field) paste0(plot_name, "_", field)
    # nocov start
    .addSpecificTour(class(x), .contrastName, function(plot_name) {
        data.frame(
            rbind(
                c(
                    element = paste0("#", plot_name, "_", sprintf("%s + .selectize-control", .contrastName)),
                    intro = "Here, we select the name of the contrast to visualise amongst the choice of differential expression results available."
                )
            )
        )
    })
    # nocov end
    cached <- .getCachedCommonInfo(se, "VolcanoPlot")

    extra_inputs <- list(
        .selectInput.iSEE(x, .contrastName,
            label = "Contrast:",
            selected = x[[.contrastName]],
            choices = cached$valid.contrast.names
        )
    )

    c(
        callNextMethod(),
        list(hr()),
        extra_inputs
    )
})

#' @export
#' @importMethodsFrom iSEE .generateTable
#' @importFrom SummarizedExperiment rowData
setMethod(".generateTable", "DETable", function(x, envir) {
  cmds <- sprintf("tab <- as.data.frame(rowData(se)[['iSEEde']][['%s']])", x[[.contrastName]])
  if (exists("row_selected", envir = envir, inherits = FALSE)) {
    cmds <- c(cmds, "tab <- tab[unique(unlist(row_selected)), , drop=FALSE]")
  }
  .textEval(cmds, envir)
  cmds
})
