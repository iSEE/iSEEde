test_that("iSEELimmaResults() constructor works", {
  
  out <- iSEELimmaResults(res_limma, row.names = rownames(se))
  
  expect_s4_class(out, "iSEELimmaResults")
  
})

test_that("showAsCell(iSEELimmaResults) works", {
  
  x <- iSEELimmaResults(res_limma, row.names = rownames(se))
  out <- DataFrame(row.names = rownames(se))
  out[["limma"]] <- x
  
  expect_output(show(out), "DataFrame with 100 rows and 1 column")
  expect_output(show(out), "<iSEELimmaResults>")
  
})

test_that("pValue(iSEELimmaResults) works", {
  
  x <- iSEELimmaResults(res_limma, row.names = rownames(se))
  out <- pValue(x)
  
  expect_type(out, "double")
  expect_length(out, nrow(x))
  expect_named(out, rownames(x))
  
})

test_that("log2FoldChange(iSEELimmaResults) works", {
  
  x <- iSEELimmaResults(res_limma, row.names = rownames(se))
  out <- log2FoldChange(x)
  
  expect_type(out, "double")
  expect_length(out, nrow(x))
  expect_named(out, rownames(x))
  
})

test_that("averageLog2(iSEELimmaResults) works", {
  
  x <- iSEELimmaResults(res_limma, row.names = rownames(se))
  out <- averageLog2(x)
  
  expect_type(out, "double")
  expect_length(out, nrow(x))
  expect_named(out, rownames(x))
  
})
