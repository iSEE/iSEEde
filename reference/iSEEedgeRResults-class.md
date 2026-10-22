# The iSEEedgeRResults class

The `iSEEedgeRResults` class is used to provide a common interface to
differential expression results produced by the edgeR package. It
provides methods to access common differential expression statistics
(e.g., log fold-change, p-value, log2 average abundance).

## Details

This class inherits all its slots directly from its parent class
[DataFrame](https://rdrr.io/pkg/S4Vectors/man/DataFrame-class.html).

## Constructor

`iSEEedgeRResults(data, row.names = rownames(data))` creates an instance
of a `iSEEedgeRResults` class, with:

- `data`:

  A `data.frame` produced by
  [`edgeR::topTags()`](https://rdrr.io/pkg/edgeR/man/topTags.html).

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
library(edgeR)
#> 
#> Attaching package: ‘edgeR’
#> The following objects are masked from ‘package:SingleCellExperiment’:
#> 
#>     cpm, tpm
library(SummarizedExperiment)

##
# From edgeR::glmLRT() ----
##

nlibs <- 3
ngenes <- 100
dispersion.true <- 0.1

# Make first gene respond to covariate x
x <- 0:2
design <- model.matrix(~x)
beta.true <- cbind(Beta1=2,Beta2=c(2,rep(0,ngenes-1)))
mu.true <- 2^(beta.true %*% t(design))

# Generate count data
y <- rnbinom(ngenes*nlibs,mu=mu.true,size=1/dispersion.true)
y <- matrix(y,ngenes,nlibs)
colnames(y) <- c("x0","x1","x2")
rownames(y) <- paste("gene",1:ngenes,sep=".")
d <- DGEList(y)

# Normalize
d <- calcNormFactors(d)
#> calcNormFactors has been renamed to normLibSizes

# Fit the NB GLMs
fit <- glmFit(d, design, dispersion=dispersion.true)

# Likelihood ratio tests for trend
results <- glmLRT(fit, coef=2)
tt <- topTags(results)

##
# iSEEedgeRResults ----
##

# Simulate the original SummarizedExperiment object
se <- SummarizedExperiment(assays = list(counts = d$counts))

# Embed the edgeR results in the SummarizedExperiment object
se <- embedContrastResults(tt, se, name = "edgeR")

##
# Access ----
##

contrastResultsNames(se)
#> [1] "edgeR"
contrastResults(se)
#> DataFrame with 100 rows and 1 column
#>                       edgeR
#>          <iSEEedgeRResults>
#> gene.1   <iSEEedgeRResults>
#> gene.2   <iSEEedgeRResults>
#> gene.3   <iSEEedgeRResults>
#> gene.4   <iSEEedgeRResults>
#> gene.5   <iSEEedgeRResults>
#> ...                     ...
#> gene.96  <iSEEedgeRResults>
#> gene.97  <iSEEedgeRResults>
#> gene.98  <iSEEedgeRResults>
#> gene.99  <iSEEedgeRResults>
#> gene.100 <iSEEedgeRResults>
contrastResults(se, "edgeR")
#> iSEEedgeRResults with 100 rows and 5 columns
#>              logFC    logCPM        LR      PValue         FDR
#>          <numeric> <numeric> <numeric>   <numeric>   <numeric>
#> gene.1      2.7426   16.9178   40.5228 1.94336e-10 1.94336e-08
#> gene.2          NA        NA        NA          NA          NA
#> gene.3          NA        NA        NA          NA          NA
#> gene.4          NA        NA        NA          NA          NA
#> gene.5          NA        NA        NA          NA          NA
#> ...            ...       ...       ...         ...         ...
#> gene.96         NA        NA        NA          NA          NA
#> gene.97         NA        NA        NA          NA          NA
#> gene.98         NA        NA        NA          NA          NA
#> gene.99         NA        NA        NA          NA          NA
#> gene.100        NA        NA        NA          NA          NA

head(pValue(contrastResults(se, "edgeR")))
#>       gene.1       gene.2       gene.3       gene.4       gene.5       gene.6 
#> 1.943360e-10           NA           NA           NA           NA 1.068182e-02 
head(log2FoldChange(contrastResults(se, "edgeR")))
#>   gene.1   gene.2   gene.3   gene.4   gene.5   gene.6 
#> 2.742604       NA       NA       NA       NA 1.710502 
head(averageLog2(contrastResults(se, "edgeR")))
#>   gene.1   gene.2   gene.3   gene.4   gene.5   gene.6 
#> 16.91776       NA       NA       NA       NA 13.71058 
```
