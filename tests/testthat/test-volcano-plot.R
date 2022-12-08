test_that("VolcanoPlot() constructor works", {
  
  out <- VolcanoPlot()
  
  expect_s4_class(out, "VolcanoPlot")
  
})

test_that(".fullName(VolcanoPlot) works", {
  
  x <- VolcanoPlot()
  out <- .fullName(x)
  
  expect_identical(out, "Volcano plot")
  
})

test_that(".panelColor(VolcanoPlot) works", {
  
  x <- VolcanoPlot()
  out <- .panelColor(x)
  
  expect_identical(out, "#DEAE10")
  
})

test_that("cacheCommonInfo(VolcanoPlot) works", {
  
  x <- VolcanoPlot()
  se0 <- se
  se0 <- embedResults(res_edger, se0, name = "edgeR")
  out <- .cacheCommonInfo(x, se0)
  
  expect_s4_class(out, "SummarizedExperiment")
  expect_identical(metadata(out)$iSEE$cached$VolcanoPlot$valid.contrast.names, "edgeR")
  
  # Run one more time on the output object for complete coverage
  .cacheCommonInfo(x, out)
  
})

test_that(".refineParameters(VolcanoPlot) works", {
  
  x <- VolcanoPlot()
  se0 <- se
  se0 <- embedResults(res_edger, se0, name = "edgeR")
  se0 <- .cacheCommonInfo(x, se0)
  out <- .refineParameters(x, se0)
  
  expect_s4_class(out, "VolcanoPlot")
  expect_identical(out[["ContrastName"]], "edgeR")
  
})

test_that(".refineParameters(VolcanoPlot) works on NULL object", {
  
  FUN <- getMethod(".refineParameters", "VolcanoPlot")
  expect_null(FUN(NULL, se))
  
})

test_that(".createObservers(VolcanoPlot) works", {
  
  x <- VolcanoPlot()
  out <- .createObservers(x, se, NULL, NULL, NULL, NULL)
  
  expect_null(out)
  
})

test_that(".defineDataInterface(MAplot) works", {
  
  x <- VolcanoPlot(PanelId=1L)
  out <- .defineDataInterface(x, se)
  
  expect_true(any(grepl("VolcanoPlot1_ContrastName", unlist(out))))
  
})

test_that(".generateDotPlotData(MAplot) works", {
  
  x <- VolcanoPlot()
  x[["ContrastName"]] <- "edgeR"
  se0 <- se
  se0 <- embedResults(res_edger, se0, name = "edgeR")
  se0 <- .cacheCommonInfo(x, se0)
  x <- .refineParameters(x, se0)
  env <- new.env()
  env$se <- se0
  out <- .generateDotPlotData(x, env)
  
  expect_match(out$commands["x"], "iSEEde::log2FoldChange")
  expect_true(any(grepl("iSEEde::pValue", unlist(out$commands))))
  
})
