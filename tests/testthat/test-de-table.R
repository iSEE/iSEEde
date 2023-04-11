test_that("DETable() constructor works", {
    out <- DETable()

    expect_s4_class(out, "DETable")
})

test_that(".fullName(DETable) works", {
    x <- DETable()
    out <- .fullName(x)

    expect_identical(out, "Differential expression table")
})

test_that(".panelColor(DETable) works", {
    x <- DETable()
    out <- .panelColor(x)

    expect_identical(out, "#DEAE10")
})

test_that("cacheCommonInfo(DETable) works", {
    x <- DETable()
    se0 <- se
    se0 <- embedContrastResults(res_edger, se0, name = "edgeR")
    out <- .cacheCommonInfo(x, se0)

    expect_s4_class(out, "SummarizedExperiment")
    expect_identical(metadata(out)$iSEE$cached$DETable$valid.contrast.names, "edgeR")

    # Run one more time on the output object for complete coverage
    .cacheCommonInfo(x, out)
})

test_that(".refineParameters(DETable) works", {
    x <- DETable()
    se0 <- se
    se0 <- embedContrastResults(res_edger, se0, name = "edgeR")
    se0 <- .cacheCommonInfo(x, se0)
    out <- .refineParameters(x, se0)

    expect_s4_class(out, "DETable")
    expect_identical(out[["ContrastName"]], "edgeR")
})

test_that(".refineParameters(DETable) works on NULL object", {
    FUN <- getMethod(".refineParameters", "DETable")
    expect_null(FUN(NULL, se))
})

test_that(".createObservers(DETable) works", {
    x <- DETable()
    out <- .createObservers(x, se, NULL, NULL, NULL, NULL)

    expect_null(out)
})

test_that(".defineDataInterface(MAplot) works", {
    x <- DETable(PanelId = 1L)
    out <- .defineDataInterface(x, se)

    expect_true(any(grepl("DETable1_ContrastName", unlist(out))))
})

test_that(".generateTable(MAplot) works", {
    x <- DETable()
    x[["ContrastName"]] <- "edgeR"
    se0 <- se
    se0 <- embedContrastResults(res_edger, se0, name = "edgeR")
    se0 <- .cacheCommonInfo(x, se0)
    x <- .refineParameters(x, se0)
    env <- new.env()
    env$se <- se0
    out <- .generateTable(x, env)

    expect_identical(out, "tab <- as.data.frame(contrastResults(se, 'edgeR'))")
})

test_that(".generateTable(MAplot) works with incoming selection", {
    x <- DETable(RowSelectionSource = "VolcanoPlot1")
    se0 <- se
    se0 <- embedContrastResults(res_edger, se0, name = "edgeR")
    se0 <- .cacheCommonInfo(x, se0)
    x <- .refineParameters(x, se0)
    
    env <- new.env()
    env$se <- se0
    env$row_selected <- head(rownames(se0))
    out <- .generateTable(x, env)

    expect_identical(out[2], "tab <- tab[unique(unlist(row_selected)), , drop=FALSE]")
})
