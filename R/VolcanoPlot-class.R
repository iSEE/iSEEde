#' The VolcanoPlot class
#'
#' The VolcanoPlot is a \linkS4class{RowDataPlot} subclass that is dedicated to creating a volcano plot.
#' It retrieves the log-fold change and p-value from and creates a row-based plot where each point represents a feature.
#'
#' @docType methods
#' @aliases VolcanoPlot VolcanoPlot-class
#' initialize,VolcanoPlot-method
#' .cacheCommonInfo,VolcanoPlot-method
#' .createObservers,VolcanoPlot-method
#' .defineDataInterface,VolcanoPlot-method
#' .fullName,VolcanoPlot-method
#' .generateDotPlotData,VolcanoPlot-method
#' .panelColor,VolcanoPlot-method
#' .refineParameters,VolcanoPlot-method
#'
#' @name VolcanoPlot-class
#'
#' @examples
#' x <- VolcanoPlot()
#' x[["ContrastName"]]
#' x[["ContrastName"]] <- "treated vs control"
NULL

#' @export
#' @importClassesFrom iSEE RowDotPlot
setClass("VolcanoPlot",
    contains = "RowDotPlot",
    slots = c(ContrastName = "character")
)

#' @export
#' @importMethodsFrom iSEE .fullName
setMethod(".fullName", "VolcanoPlot", function(x) "Volcano plot")

#' @export
#' @importMethodsFrom iSEE .panelColor
setMethod(".panelColor", "VolcanoPlot", function(x) "#DEAE10")

#' @export
#' @importMethodsFrom methods initialize
#' @importFrom methods callNextMethod
setMethod("initialize", "VolcanoPlot", function(.Object,
                                                ContrastName = NA_character_, ...) {
    args <- list(ContrastName = ContrastName, ...)

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
#' @importFrom iSEE .getCachedCommonInfo .setCachedCommonInfo
#' @importFrom methods callNextMethod
#' @importFrom SummarizedExperiment rowData
setMethod(".cacheCommonInfo", "VolcanoPlot", function(x, se) {
    if (!is.null(.getCachedCommonInfo(se, "VolcanoPlot"))) {
        return(se)
    }

    se <- callNextMethod()

    contrast_names <- contrastResultsNames(se)

    .setCachedCommonInfo(se, "VolcanoPlot", valid.contrast.names = contrast_names)
})

#' @export
#' @importMethodsFrom iSEE .refineParameters
#' @importFrom iSEE .getCachedCommonInfo .replaceMissingWithFirst
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
#' @importMethodsFrom iSEE .createObservers
#' @importFrom iSEE .getEncodedName .createProtectedParameterObservers
#' @importFrom methods callNextMethod
setMethod(".createObservers", "VolcanoPlot", function(x, se, input, session, pObjects, rObjects) {
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
setMethod(".defineDataInterface", "VolcanoPlot", function(x, se, select_info) {
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
#' @importMethodsFrom iSEE .generateDotPlotData
#' @importFrom iSEE .textEval
setMethod(".generateDotPlotData", "VolcanoPlot", function(x, envir) {
    data_cmds <- list()

    y_lab <- expression(-log[10]*" ("*italic(P)*"-value)")

    data_cmds[["de_table"]] <- sprintf("de_table <- contrastResults(se, '%s')", x[[.contrastName]])

    # NOTE: deparse() automatically adds quotes, AND protects against existing quotes/escapes.
    data_cmds[["y"]] <- c(
        "plot.data <- data.frame(row.names=rownames(se))",
        "plot.data$Y <- -log10(iSEEde::pValue(de_table))"
    )

    # Prepare X-axis data.
    x_lab <- expression(log[2]*" fold change")
    data_cmds[["x"]] <- "plot.data$X <- iSEEde::log2FoldChange(de_table)"

    plot_title <- x[[.contrastName]]

    data_cmds <- unlist(data_cmds)
    .textEval(data_cmds, envir)

    list(commands = data_cmds, labels = list(title = plot_title, X = x_lab, Y = y_lab))
})
