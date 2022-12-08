test_that("iSEEedgeRResults() constructor works", {
  
  out <- iSEEedgeRResults(res_edger, row.names = rownames(se))
  
  expect_s4_class(out, "iSEEedgeRResults")
  
})

test_that("show(iSEEedgeRResults) works", {
  
  x <- iSEEedgeRResults(res_edger, row.names = rownames(se))
  x <- DataFrame(edgeR = I(x))
  
  expect_output(show(x), "DataFrame with 100 rows and 1 column")
  expect_output(show(x), "<iSEEedgeRResults>")
  
})

test_that("pValue(iSEEedgeRResults) works", {
  
  x <- iSEEedgeRResults(res_edger, row.names = rownames(se))
  out <- pValue(x)
  
  expect_type(out, "double")
  expect_length(out, nrow(x))
  expect_named(out, rownames(x))
  
})

test_that("log2FoldChange(iSEEedgeRResults) works", {
  
  x <- iSEEedgeRResults(res_edger, row.names = rownames(se))
  out <- log2FoldChange(x)
  
  expect_type(out, "double")
  expect_length(out, nrow(x))
  expect_named(out, rownames(x))
  
})

test_that("averageLog2(iSEEedgeRResults) works", {
  
  x <- iSEEedgeRResults(res_edger, row.names = rownames(se))
  out <- averageLog2(x)
  
  expect_type(out, "double")
  expect_length(out, nrow(x))
  expect_named(out, rownames(x))
  
})
