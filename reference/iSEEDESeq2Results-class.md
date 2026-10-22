# The iSEEDESeq2Results class

The `iSEEDESeq2Results` class is used to provide a common interface to
differential expression results produced by the DESeq2 package. It
provides methods to access common differential expression statistics
(e.g., log2 fold-change, p-value, log2 average abundance).

## Details

This class inherits all its slots directly from its parent class
[DataFrame](https://rdrr.io/pkg/S4Vectors/man/DataFrame-class.html).

## Constructor

`iSEEDESeq2Results(data, row.names = rownames(data))` creates an
instance of a `iSEEDESeq2Results` class, with:

- `data`:

  A `data.frame` produced by
  [`DESeq2::results()`](https://rdrr.io/pkg/DESeq2/man/results.html) or
  [`DESeq2::lfcShrink()`](https://rdrr.io/pkg/DESeq2/man/lfcShrink.html).

- `row.names`:

  The character vector of rownames for the
  [SummarizedExperiment](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html)
  object in which the object is to be embedded. Must be a superset of
  `rownames(data)`.

## Supported methods

- `embedContrastResults(x, se, name, ...)` embeds `x` in `se` under the
  identifier `name`. See
  [`embedContrastResults()`](https://isee.github.io/iSEEde/reference/utils-SummarizedExperiment.md)
  for more details.

- `pValue(x)` returns the vector of raw p-values.

- `log2FoldChange(x)` returns the vector of log2-fold-change values.

- `averageLog2(x)` returns the vector of average log2-expression values.

## Author

Kevin Rue-Albrecht

## Examples

``` r
library(DESeq2)

##
# From DESeq2::DESeq() ----
##

cnts <- matrix(rnbinom(n = 1000, mu = 100, size = 1 / 0.5), ncol = 10)
rownames(cnts) <- paste("Gene", 1:100)
cond <- factor(rep(1:2, each = 5))

# object construction
dds <- DESeqDataSetFromMatrix(cnts, DataFrame(cond), ~cond)
#> converting counts to integer mode

# standard analysis
dds <- DESeq(dds)
#> estimating size factors
#> estimating dispersions
#> gene-wise dispersion estimates
#> mean-dispersion relationship
#> -- note: fitType='parametric', but the dispersion trend was not well captured by the
#>    function: y = a/x + b, and a local regression fit was automatically substituted.
#>    specify fitType='local' or 'mean' to avoid this message next time.
#> final dispersion estimates
#> fitting model and testing
res <- results(dds)
head(res)
#> log2 fold change (MLE): cond 2 vs 1 
#> Wald test p-value: cond 2 vs 1 
#> DataFrame with 6 rows and 6 columns
#>         baseMean log2FoldChange     lfcSE      stat    pvalue      padj
#>        <numeric>      <numeric> <numeric> <numeric> <numeric> <numeric>
#> Gene 1  102.5201      -0.727757  0.804034 -0.905132  0.365395  0.960799
#> Gene 2   83.0402      -0.409929  0.563552 -0.727402  0.466979  0.974797
#> Gene 3   85.6498       0.101207  0.521651  0.194013  0.846166  0.974797
#> Gene 4  100.5620       0.197987  0.592194  0.334328  0.738132  0.974797
#> Gene 5   82.6255      -0.541261  0.622164 -0.869965  0.384320  0.960799
#> Gene 6  100.7767      -0.256369  0.715565 -0.358274  0.720138  0.974797

##
# iSEEDESeq2Results ----
##

# Embed the DESeq2 results in the SummarizedExperiment object
dds <- embedContrastResults(res, dds, name = "DESeq2")

##
# Access ----
##

contrastResultsNames(dds)
#> [1] "DESeq2"
contrastResults(dds)
#> DataFrame with 100 rows and 1 column
#>                       DESeq2
#>          <iSEEDESeq2Results>
#> Gene 1   <iSEEDESeq2Results>
#> Gene 2   <iSEEDESeq2Results>
#> Gene 3   <iSEEDESeq2Results>
#> Gene 4   <iSEEDESeq2Results>
#> Gene 5   <iSEEDESeq2Results>
#> ...                      ...
#> Gene 96  <iSEEDESeq2Results>
#> Gene 97  <iSEEDESeq2Results>
#> Gene 98  <iSEEDESeq2Results>
#> Gene 99  <iSEEDESeq2Results>
#> Gene 100 <iSEEDESeq2Results>
contrastResults(dds, "DESeq2")
#> iSEEDESeq2Results with 100 rows and 6 columns
#>           baseMean log2FoldChange     lfcSE       stat    pvalue      padj
#>          <numeric>      <numeric> <numeric>  <numeric> <numeric> <numeric>
#> Gene 1    102.5201      -0.727757  0.804034  -0.905132  0.365395  0.960799
#> Gene 2     83.0402      -0.409929  0.563552  -0.727402  0.466979  0.974797
#> Gene 3     85.6498       0.101207  0.521651   0.194013  0.846166  0.974797
#> Gene 4    100.5620       0.197987  0.592194   0.334328  0.738132  0.974797
#> Gene 5     82.6255      -0.541261  0.622164  -0.869965  0.384320  0.960799
#> ...            ...            ...       ...        ...       ...       ...
#> Gene 96   129.3170     -0.0744697  0.503853 -0.1478006  0.882500  0.974797
#> Gene 97    81.2144     -0.3133681  0.577976 -0.5421822  0.587693  0.974797
#> Gene 98    63.7106     -0.1904979  0.613499 -0.3105104  0.756173  0.974797
#> Gene 99   144.0617      0.1101995  0.693533  0.1588958  0.873751  0.974797
#> Gene 100  102.9799      0.0354005  0.528068  0.0670377  0.946552  0.989373

head(pValue(contrastResults(dds, "DESeq2")))
#>    Gene 1    Gene 2    Gene 3    Gene 4    Gene 5    Gene 6 
#> 0.3653954 0.4669795 0.8461658 0.7381317 0.3843196 0.7201381 
head(log2FoldChange(contrastResults(dds, "DESeq2")))
#>     Gene 1     Gene 2     Gene 3     Gene 4     Gene 5     Gene 6 
#> -0.7277568 -0.4099289  0.1012071  0.1979872 -0.5412611 -0.2563687 
head(averageLog2(contrastResults(dds, "DESeq2")))
#>   Gene 1   Gene 2   Gene 3   Gene 4   Gene 5   Gene 6 
#> 6.679763 6.375739 6.420378 6.651942 6.368515 6.655019 
```
