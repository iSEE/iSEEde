test_that("embedResults(ANY) works", {
    x <- character()

    expect_error(
        embedResults(x, se, name = "ERROR"),
        "no 'embedResults' method defined for object of class"
    )
})

test_that("embedResults(data.frame) works", {
    expect_error(
        embedResults(res_limma, se, name = "limma"),
        'argument "class" is missing, with no default'
    )

    expect_error(
        embedResults(res_limma, se, name = "limma", class = "ERROR"),
        "must be a value in"
    )

    out <- embedResults(res_limma, se, name = "limma", class = "limma")

    expect_s4_class(out, "SummarizedExperiment")
    expect_named(rowData(out), "iSEEde")
    expect_named(rowData(out)[["iSEEde"]], "limma")

    expect_warning(
        embedResults(res_limma, out, name = "limma", class = "limma"),
        "Results already exist under name"
    )
})


test_that("embedResults(DESeqResults) works", {
    out <- embedResults(res_deseq2, se, name = "DESeq2")

    expect_s4_class(out, "SummarizedExperiment")
    expect_named(rowData(out), "iSEEde")
    expect_named(rowData(out)[["iSEEde"]], "DESeq2")
})


test_that("embedResults(TopTags) works", {
    out <- embedResults(res_deseq2, se, name = "edgeR")

    expect_s4_class(out, "SummarizedExperiment")
    expect_named(rowData(out), "iSEEde")
    expect_named(rowData(out)[["iSEEde"]], "edgeR")
})
