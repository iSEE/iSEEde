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
