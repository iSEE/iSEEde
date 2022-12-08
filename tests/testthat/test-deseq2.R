test_that("iSEEDESeq2Results() constructor works", {
  
  out <- iSEEDESeq2Results(res_deseq2, row.names = rownames(se))
  
  expect_s4_class(out, "iSEEDESeq2Results")
  
})

test_that("show(iSEEDESeq2Results) works", {
  
  x <- iSEEDESeq2Results(res_deseq2, row.names = rownames(se))
  x <- DataFrame(DESeq2 = I(x))
  
  expect_output(show(x), "DataFrame with 100 rows and 1 column")
  expect_output(show(x), "<iSEEDESeq2Results>")
  
})

test_that("pValue(iSEEDESeq2Results) works", {
  
  x <- iSEEDESeq2Results(res_deseq2, row.names = rownames(se))
  out <- pValue(x)
  
  expect_type(out, "double")
  expect_length(out, nrow(x))
  expect_named(out, rownames(x))
  
})

test_that("log2FoldChange(iSEEDESeq2Results) works", {
  
  x <- iSEEDESeq2Results(res_deseq2, row.names = rownames(se))
  out <- log2FoldChange(x)
  
  expect_type(out, "double")
  expect_length(out, nrow(x))
  expect_named(out, rownames(x))
  
})

test_that("averageLog2(iSEEDESeq2Results) works", {
  
  x <- iSEEDESeq2Results(res_deseq2, row.names = rownames(se))
  out <- averageLog2(x)
  
  expect_type(out, "double")
  expect_length(out, nrow(x))
  expect_named(out, rownames(x))
  
})
