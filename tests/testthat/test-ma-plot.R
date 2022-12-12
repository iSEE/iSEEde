test_that("MAPlot() constructor works", {
    out <- MAPlot()

    expect_s4_class(out, "MAPlot")
})

test_that(".fullName(MAPlot) works", {
    x <- MAPlot()
    out <- .fullName(x)

    expect_identical(out, "MA plot")
})

test_that(".panelColor(MAPlot) works", {
    x <- MAPlot()
    out <- .panelColor(x)

    expect_identical(out, "#DEAE10")
})

test_that("cacheCommonInfo(MAPlot) works", {
    x <- MAPlot()
    se0 <- se
    se0 <- embedResults(res_edger, se0, name = "edgeR")
    out <- .cacheCommonInfo(x, se0)

    expect_s4_class(out, "SummarizedExperiment")
    expect_identical(metadata(out)$iSEE$cached$MAPlot$valid.contrast.names, "edgeR")

    # Run one more time on the output object for complete coverage
    .cacheCommonInfo(x, out)
})

test_that(".refineParameters(MAPlot) works", {
    x <- MAPlot()
    se0 <- se
    se0 <- embedResults(res_edger, se0, name = "edgeR")
    se0 <- .cacheCommonInfo(x, se0)
    out <- .refineParameters(x, se0)

    expect_s4_class(out, "MAPlot")
    expect_identical(out[["ContrastName"]], "edgeR")
})

test_that(".refineParameters(MAPlot) works on NULL object", {
    FUN <- getMethod(".refineParameters", "MAPlot")
    expect_null(FUN(NULL, se))
})


test_that(".createObservers(MAPlot) works", {
    x <- MAPlot()
    out <- .createObservers(x, se, NULL, NULL, NULL, NULL)

    expect_null(out)
})

test_that(".defineDataInterface(MAplot) works", {
    x <- MAPlot(PanelId = 1L)
    out <- .defineDataInterface(x, se)

    expect_true(any(grepl("MAPlot1_ContrastName", unlist(out))))
})

test_that(".generateDotPlotData(MAplot) works", {
    x <- MAPlot()
    x[["ContrastName"]] <- "edgeR"
    se0 <- se
    se0 <- embedResults(res_edger, se0, name = "edgeR")
    se0 <- .cacheCommonInfo(x, se0)
    x <- .refineParameters(x, se0)
    env <- new.env()
    env$se <- se0
    out <- .generateDotPlotData(x, env)

    expect_match(out$commands["x"], "iSEEde::averageLog2")
    expect_true(any(grepl("iSEEde::log2FoldChange", unlist(out$commands))))
})
