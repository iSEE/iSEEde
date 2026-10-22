# The iSEELimmaResults class

The `iSEELimmaResults` class is used to provide a common interface to
differential expression results produced by the limma package. It
provides methods to access common differential expression statistics
(e.g., log fold-change, p-value, log2 average abundance).

## Details

This class inherits all its slots directly from its parent class
[DataFrame](https://rdrr.io/pkg/S4Vectors/man/DataFrame-class.html).

## Constructor

`iSEELimmaResults(data, row.names = rownames(data))` creates an instance
of a `iSEELimmaResults` class, with:

- `data`:

  A `data.frame` produced by
  [`limma::topTable()`](https://rdrr.io/pkg/limma/man/toptable.html).

- `row.names`:

  The character vector of rownames for the
  [SummarizedExperiment](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html)
  object in which the object is to be embedded. Must be a superset of
  `rownames(data)`.

## Supported methods

- `embedContrastResults(x, se, name, class = "limma", ...)` embeds `x`
  in `se` under the identifier `name`. See
  [`embedContrastResults()`](https://isee.github.io/iSEEde/reference/utils-SummarizedExperiment.md)
  for more details.

- `pValue(x)` returns the vector of raw p-values.

- `log2FoldChange(x)` returns the vector of log2-fold-change values.

- `averageLog2(x)` returns the vector of average log2-expression values.

## Author

Kevin Rue-Albrecht

## Examples

``` r
library(limma)
#> 
#> Attaching package: ‘limma’
#> The following object is masked from ‘package:DESeq2’:
#> 
#>     plotMA
#> The following object is masked from ‘package:BiocGenerics’:
#> 
#>     plotMA
library(SummarizedExperiment)

##
# From limma::lmFit() ----
##

sd <- 0.3 * sqrt(4 / rchisq(100, df = 4))
y <- matrix(rnorm(100 * 6, sd = sd), 100, 6)
rownames(y) <- paste("Gene", 1:100)
y[1:2, 4:6] <- y[1:2, 4:6] + 2
design <- cbind(Grp1 = 1, Grp2vs1 = c(0, 0, 0, 1, 1, 1))

fit <- lmFit(y, design)
fit <- eBayes(fit)
tt <- topTable(fit, coef = 2)
head(tt)
#>              logFC     AveExpr         t      P.Value   adj.P.Val         B
#> Gene 2   2.2640913  0.92510941 11.350832 1.402741e-05 0.001402741  3.852453
#> Gene 78 -2.2011871 -0.49464729 -4.289664 4.117023e-03 0.205851148 -2.336193
#> Gene 1   1.9110762  1.35369919  3.728917 8.166075e-03 0.272202489 -3.080341
#> Gene 61 -0.6188866 -0.03854689 -3.322904 1.381124e-02 0.345281103 -3.645980
#> Gene 74 -0.5626678 -0.14647983 -2.617643 3.632140e-02 0.695593282 -4.667721
#> Gene 22 -0.4379104 -0.22422247 -2.420919 4.803481e-02 0.695593282 -4.956539

##
# iSEELimmaResults ----
##

# Simulate the original SummarizedExperiment object
se <- SummarizedExperiment(assays = list(counts = y))

# Embed the Limma-Voom results in the SummarizedExperiment object
se <- embedContrastResults(tt, se, name = "Limma-Voom", class = "limma")

##
# Access ----
##

contrastResultsNames(se)
#> [1] "Limma-Voom"
contrastResults(se)
#> DataFrame with 100 rows and 1 column
#>                  Limma-Voom
#>          <iSEELimmaResults>
#> Gene 1   <iSEELimmaResults>
#> Gene 2   <iSEELimmaResults>
#> Gene 3   <iSEELimmaResults>
#> Gene 4   <iSEELimmaResults>
#> Gene 5   <iSEELimmaResults>
#> ...                     ...
#> Gene 96  <iSEELimmaResults>
#> Gene 97  <iSEELimmaResults>
#> Gene 98  <iSEELimmaResults>
#> Gene 99  <iSEELimmaResults>
#> Gene 100 <iSEELimmaResults>
contrastResults(se, "Limma-Voom")
#> iSEELimmaResults with 100 rows and 6 columns
#>              logFC   AveExpr         t     P.Value  adj.P.Val         B
#>          <numeric> <numeric> <numeric>   <numeric>  <numeric> <numeric>
#> Gene 1     1.91108  1.353699   3.72892 8.16607e-03 0.27220249  -3.08034
#> Gene 2     2.26409  0.925109  11.35083 1.40274e-05 0.00140274   3.85245
#> Gene 3          NA        NA        NA          NA         NA        NA
#> Gene 4          NA        NA        NA          NA         NA        NA
#> Gene 5          NA        NA        NA          NA         NA        NA
#> ...            ...       ...       ...         ...        ...       ...
#> Gene 96         NA        NA        NA          NA         NA        NA
#> Gene 97         NA        NA        NA          NA         NA        NA
#> Gene 98         NA        NA        NA          NA         NA        NA
#> Gene 99         NA        NA        NA          NA         NA        NA
#> Gene 100        NA        NA        NA          NA         NA        NA

head(pValue(contrastResults(se, "Limma-Voom")))
#>       Gene 1       Gene 2       Gene 3       Gene 4       Gene 5       Gene 6 
#> 8.166075e-03 1.402741e-05           NA           NA           NA           NA 
head(log2FoldChange(contrastResults(se, "Limma-Voom")))
#>   Gene 1   Gene 2   Gene 3   Gene 4   Gene 5   Gene 6 
#> 1.911076 2.264091       NA       NA       NA       NA 
head(averageLog2(contrastResults(se, "Limma-Voom")))
#>    Gene 1    Gene 2    Gene 3    Gene 4    Gene 5    Gene 6 
#> 1.3536992 0.9251094        NA        NA        NA        NA 
```
