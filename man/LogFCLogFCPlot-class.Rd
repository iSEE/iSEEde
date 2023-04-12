% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LogFCLogFCPlot-class.R
\docType{methods}
\name{LogFCLogFCPlot-class}
\alias{LogFCLogFCPlot-class}
\alias{LogFCLogFCPlot}
\alias{initialize,LogFCLogFCPlot-method}
\alias{.cacheCommonInfo,LogFCLogFCPlot-method}
\alias{.createObservers,LogFCLogFCPlot-method}
\alias{.defineDataInterface,LogFCLogFCPlot-method}
\alias{.fullName,LogFCLogFCPlot-method}
\alias{.generateDotPlotData,LogFCLogFCPlot-method}
\alias{.panelColor,LogFCLogFCPlot-method}
\alias{.refineParameters,LogFCLogFCPlot-method}
\title{The LogFCLogFCPlot class}
\description{
The LogFCLogFCPlot class is a \linkS4class{RowDataPlot} subclass that is dedicated to comparing the log-fold-change value of two contrasts.
It retrieves the log-fold change of the two selected contrasts and creates a row-based plot where each point represents a feature.
}
\examples{
x <- LogFCLogFCPlot()
x[["ContrastNameX"]]
x[["ContrastNameX"]] <- "treatment1 vs control"
x[["ContrastNameY"]]
x[["ContrastNameY"]] <- "treatment2 vs control"
}