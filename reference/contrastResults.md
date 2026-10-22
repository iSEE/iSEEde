# Extract contrast results embedded in a SummarizedExperiment object

`contrastResults` returns either all contrasts results stored in
`object` or a single contrast result by name.

`contrastResultsNames` returns the names of contrast results embedded in
`object`.

## Usage

``` r
contrastResultsNames(object)

contrastResults(object, name)
```

## Arguments

- object:

  A
  [SummarizedExperiment](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html)
  object.

- name:

  (Optional) Name of a single contrast result name to extract. Use
  `contrastResultsNames(object)` to list available names.

## Value

For `contrastResultsNames`: the names of embedded contrast results
available.

For `contrastResults`: a `DataFrame` of differential expression
statistics.

If `name` is missing, `contrastResults` returns a nested
[`DataFrame`](https://rdrr.io/pkg/S4Vectors/man/DataFrame-class.html) in
which each column contains the results of a single contrast. If `name`
is given, `contrastResults` returns a
[`DataFrame`](https://rdrr.io/pkg/S4Vectors/man/DataFrame-class.html)
that contains the results of a single contrast.

## Examples

``` r
library("iSEEde")
library("airway")
library("DESeq2")
library("iSEE")

##
# Example data ----
##

data("airway")
airway$dex <- relevel(airway$dex, "untrt")

dds <- DESeqDataSet(airway, ~ 0 + dex + cell)

dds <- DESeq(dds)
#> estimating size factors
#> estimating dispersions
#> gene-wise dispersion estimates
#> mean-dispersion relationship
#> final dispersion estimates
#> fitting model and testing
res_deseq2 <- results(dds, contrast = list("dextrt", "dexuntrt"))
airway <- embedContrastResults(res_deseq2, airway, name = "dex: trt vs untrt")

##
# List result names ---
##

contrastResultsNames(airway)
#> [1] "dex: trt vs untrt"

##
# Extract results ---
##

contrastResults(airway)
#> DataFrame with 63677 rows and 1 column
#>                   dex: trt vs untrt
#>                 <iSEEDESeq2Results>
#> ENSG00000000003 <iSEEDESeq2Results>
#> ENSG00000000005 <iSEEDESeq2Results>
#> ENSG00000000419 <iSEEDESeq2Results>
#> ENSG00000000457 <iSEEDESeq2Results>
#> ENSG00000000460 <iSEEDESeq2Results>
#> ...                             ...
#> ENSG00000273489 <iSEEDESeq2Results>
#> ENSG00000273490 <iSEEDESeq2Results>
#> ENSG00000273491 <iSEEDESeq2Results>
#> ENSG00000273492 <iSEEDESeq2Results>
#> ENSG00000273493 <iSEEDESeq2Results>
contrastResults(airway, "dex: trt vs untrt")
#> iSEEDESeq2Results with 63677 rows and 6 columns
#>                  baseMean log2FoldChange     lfcSE      stat      pvalue
#>                 <numeric>      <numeric> <numeric> <numeric>   <numeric>
#> ENSG00000000003  708.6022     -0.3812539  0.100654 -3.787752 0.000152016
#> ENSG00000000005    0.0000             NA        NA        NA          NA
#> ENSG00000000419  520.2979      0.2068127  0.112219  1.842944 0.065337213
#> ENSG00000000457  237.1630      0.0379205  0.143445  0.264356 0.791505314
#> ENSG00000000460   57.9326     -0.0881679  0.287142 -0.307054 0.758802543
#> ...                   ...            ...       ...       ...         ...
#> ENSG00000273489  0.275899       1.483744   3.51398  0.422240    0.672850
#> ENSG00000273490  0.000000             NA        NA        NA          NA
#> ENSG00000273491  0.000000             NA        NA        NA          NA
#> ENSG00000273492  0.105978      -0.463688   3.52312 -0.131613    0.895290
#> ENSG00000273493  0.106142      -0.521372   3.53142 -0.147638    0.882628
#>                       padj
#>                  <numeric>
#> ENSG00000000003 0.00128292
#> ENSG00000000005         NA
#> ENSG00000000419 0.19646961
#> ENSG00000000457 0.91141884
#> ENSG00000000460 0.89500551
#> ...                    ...
#> ENSG00000273489         NA
#> ENSG00000273490         NA
#> ENSG00000273491         NA
#> ENSG00000273492         NA
#> ENSG00000273493         NA
```
