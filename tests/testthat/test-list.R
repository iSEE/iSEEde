test_that("embedContrastResults(DESeqResults) works", {
    out <- contrastResultsNames(se)
    expect_identical(out, NULL)
    
    se <- embedContrastResults(res_limma, se, name = "limma", class = "limma")
    out <- contrastResultsNames(se)
    expect_identical(out, "limma")
    
    se <- embedContrastResults(res_deseq2, se, name = "DESeq2")
        
    out <- contrastResultsNames(se)
    expect_identical(out, c("limma", "DESeq2"))
    
    se <- embedContrastResults(res_deseq2, se, name = "edgeR")
    out <- contrastResultsNames(se)
    expect_identical(out, c("limma", "DESeq2", "edgeR"))
})
