test_that("embedContrastResults(ANY) works", {
    x <- character()

    expect_error(
        embedContrastResults(x, se, name = "ERROR"),
        "no 'embedContrastResults' method defined for object of class"
    )
})

test_that("embedContrastResults(data.frame) works", {
    expect_error(
        embedContrastResults(res_limma, se, name = "limma"),
        'argument "class" is missing, with no default'
    )

    expect_error(
        embedContrastResults(res_limma, se, name = "limma", class = "ERROR"),
        "must be a value in"
    )

    out <- embedContrastResults(res_limma, se, name = "limma", class = "limma")

    expect_s4_class(out, "SummarizedExperiment")
    expect_named(rowData(out), "iSEEde")
    expect_named(rowData(out)[["iSEEde"]], "limma")

    expect_warning(
        embedContrastResults(res_limma, out, name = "limma", class = "limma"),
        "Results already exist under name"
    )
})


test_that("embedContrastResults(DESeqResults) works", {
    out <- embedContrastResults(res_deseq2, se, name = "DESeq2")

    expect_s4_class(out, "SummarizedExperiment")
    expect_named(rowData(out), "iSEEde")
    expect_named(rowData(out)[["iSEEde"]], "DESeq2")
})


test_that("embedContrastResults(TopTags) works", {
    out <- embedContrastResults(res_deseq2, se, name = "edgeR")

    expect_s4_class(out, "SummarizedExperiment")
    expect_named(rowData(out), "iSEEde")
    expect_named(rowData(out)[["iSEEde"]], "edgeR")
})

test_that("contrastResultsNames() works", {
    out <- contrastResultsNames(se)
    expect_identical(out, NULL)
    
    se <- embedContrastResults(res_limma, se, name = "limma", class = "limma")
    out <- contrastResultsNames(se)
    expect_identical(out, "limma")
    
    se <- embedContrastResults(res_deseq2, se, name = "DESeq2")
    
    out <- contrastResultsNames(se)
    expect_identical(out, c("limma", "DESeq2"))
    
    se <- embedContrastResults(res_edger, se, name = "edgeR")
    out <- contrastResultsNames(se)
    expect_identical(out, c("limma", "DESeq2", "edgeR"))
})

test_that("contrastResults()", {
    
    out <- contrastResults(se)
    expect_identical(out, NULL)
    
    se <- embedContrastResults(res_limma, se, name = "limma", class = "limma")
    se <- embedContrastResults(res_deseq2, se, name = "DESeq2")
    se <- embedContrastResults(res_edger, se, name = "edgeR")
    
    out <- contrastResults(se)
    expect_s4_class(out, "DataFrame")
    expect_identical(colnames(out), c("limma", "DESeq2", "edgeR"))
    
    out <- contrastResults(se, name = "limma")
    expect_s4_class(out, "DataFrame")
    expect_identical(colnames(out), c("logFC", "AveExpr", "t", "P.Value", "adj.P.Val", "B"))
    
    out <- contrastResults(se, name = "DESeq2")
    expect_s4_class(out, "DataFrame")
    expect_identical(colnames(out), c("baseMean", "log2FoldChange", "lfcSE", "stat", "pvalue", "padj" ))
    
    out <- contrastResults(se, name = "edgeR")
    expect_s4_class(out, "DataFrame")
    expect_identical(colnames(out), c("logFC", "logCPM", "LR", "PValue", "FDR"))
})
