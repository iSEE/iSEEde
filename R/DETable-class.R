#' The DETable class
#'
#' The DETable class is a \linkS4class{RowTable} subclass that is dedicated to creating a volcano plot.
#' It retrieves the log-fold change and p-value from and creates a row-based plot where each point represents a feature.
#' 
#' @section Slot overview:
#' The following slots control the test procedure:
#' \itemize{
#' \item `ContrastName`, a character scalar indicating the name of the contrast to display.
#' \item `RoundDigits`, a logical scalar indicating whether to round numeric values (see `SignifDigits`).
#' \item `SignifDigits`, an integer scalar indicating the number of significant digits to use for rounding numbers  (see `RoundDigits`).
#' }
#' 
#' In addition, this class inherits all slots from its parent [RowTable-class] and [Table-class] classes.
#'
#' @docType methods
#' @aliases DETable DETable-class
#' initialize,DETable-method
#' .cacheCommonInfo,DETable-method
#' .createObservers,DETable-method
#' .defineDataInterface,DETable-method
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
    slots = c(ContrastName = "character",
              RoundDigits = "logical",
              SignifDigits = "integer")
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
setMethod("initialize", "DETable", function(.Object, ...) {
    args <- list(...)
    
    args <- .emptyDefault(args, .contrastName, NA_character_)
    args <- .emptyDefault(args, .roundDigits, getPanelDefault(.roundDigits))
    args <- .emptyDefault(args, .significantDigits, getPanelDefault(.significantDigits))

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

    contrast_names <- contrastResultsNames(se)

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
    
    if (is.null(contrast_names)) {
      return(NULL)
    }
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
    
    .createUnprotectedParameterObservers(plot_name,
        fields = c(.roundDigits, .significantDigits),
        input = input, pObjects = pObjects, rObjects = rObjects)

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
    cached <- .getCachedCommonInfo(se, "DETable")

    extra_inputs <- list(
        .selectInput.iSEE(x, .contrastName,
            label = "Contrast:",
            selected = x[[.contrastName]],
            choices = cached$valid.contrast.names
        ),
        hr(),
        .checkboxInput.iSEE(x, .roundDigits,
            label = "Round digits?",
            value = x[[.roundDigits]]),
        .conditionalOnCheckSolo(
          paste0(plot_name, "_", .roundDigits),
          on_select = TRUE,
          .numericInput.iSEE(x, .significantDigits,
                label = "Significant digits:",
                value = x[[.significantDigits]],
                min = 1L,
                max = 6L))
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
  cmds <- sprintf("tab <- as.data.frame(contrastResults(se, '%s'))", x[[.contrastName]])
  if (exists("row_selected", envir = envir, inherits = FALSE)) {
    cmds <- c(cmds, "tab <- tab[unique(unlist(row_selected)), , drop=FALSE]")
  }
  
  cmds <- c(cmds, .define_table_rounding_commands(x))
  
  .textEval(cmds, envir)
  
  cmds
})
