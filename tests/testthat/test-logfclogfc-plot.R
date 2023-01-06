test_that("LogFCLogFCPlot() constructor works", {
    out <- LogFCLogFCPlot()

    expect_s4_class(out, "LogFCLogFCPlot")
})

test_that(".fullName(LogFCLogFCPlot) works", {
    x <- LogFCLogFCPlot()
    out <- .fullName(x)

    expect_identical(out, "LogFC-LogFC plot")
})

test_that(".panelColor(LogFCLogFCPlot) works", {
    x <- LogFCLogFCPlot()
    out <- .panelColor(x)

    expect_identical(out, "#DEAE10")
})

test_that("cacheCommonInfo(LogFCLogFCPlot) works", {
    x <- LogFCLogFCPlot()
    se0 <- se
    se0 <- embedResults(res_edger, se0, name = "edgeR")
    out <- .cacheCommonInfo(x, se0)

    expect_s4_class(out, "SummarizedExperiment")
    expect_identical(metadata(out)$iSEE$cached$LogFCLogFCPlot$valid.contrast.names, "edgeR")

    # Run one more time on the output object for complete coverage
    .cacheCommonInfo(x, out)
})

test_that(".refineParameters(LogFCLogFCPlot) works", {
    x <- LogFCLogFCPlot()
    se0 <- se
    se0 <- embedResults(res_edger, se0, name = "edgeR1")
    se0 <- embedResults(res_edger, se0, name = "edgeR2")
    se0 <- .cacheCommonInfo(x, se0)
    out <- .refineParameters(x, se0)

    expect_s4_class(out, "LogFCLogFCPlot")
    expect_identical(out[["ContrastNameX"]], "edgeR1")
    expect_identical(out[["ContrastNameY"]], "edgeR1")
})

test_that(".refineParameters(LogFCLogFCPlot) works on NULL object", {
    FUN <- getMethod(".refineParameters", "LogFCLogFCPlot")
    expect_null(FUN(NULL, se))
})

test_that(".createObservers(LogFCLogFCPlot) works", {
    x <- LogFCLogFCPlot()
    out <- .createObservers(x, se, NULL, NULL, NULL, NULL)

    expect_null(out)
})

test_that(".defineDataInterface(MAplot) works", {
    x <- LogFCLogFCPlot(PanelId = 1L)
    out <- .defineDataInterface(x, se)

    expect_true(any(grepl("LogFCLogFCPlot1_ContrastName", unlist(out))))
})

test_that(".generateDotPlotData(MAplot) works", {
    x <- LogFCLogFCPlot()
    x[["ContrastNameX"]] <- "edgeR"
    x[["ContrastNameY"]] <- "DESeq2"
    se0 <- se
    se0 <- embedResults(res_edger, se0, name = "edgeR")
    se0 <- embedResults(res_edger, se0, name = "DESeq2")
    se0 <- .cacheCommonInfo(x, se0)
    x <- .refineParameters(x, se0)
    env <- new.env()
    env$se <- se0
    out <- .generateDotPlotData(x, env)

    expect_identical(out$commands[["x"]], "plot.data$X <- iSEEde::log2FoldChange(rowData(se)[['iSEEde']][['edgeR']])")
    expect_identical(out$commands[["y2"]], "plot.data$Y <- iSEEde::log2FoldChange(rowData(se)[['iSEEde']][['DESeq2']])")
})
