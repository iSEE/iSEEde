# Generics for Differential Expression Results

An overview of the generics for accessing common pieces of information
in differential expression results.

## Definitions

- `pValue(x)` returns a named numeric vector of raw p-values.

- `log2FoldChange(x)` returns a named numeric vector of log2-fold-change
  values.

- `averageLog2(x)` returns a named numeric vector of average
  log2-expression values.

## Author

Kevin Rue-Albrecht

## Examples

``` r
showMethods(pValue)
#> Function: pValue (package iSEEde)
#> x="iSEEDESeq2Results"
#> x="iSEELimmaResults"
#> x="iSEEedgeRResults"
#> 
showMethods(log2FoldChange)
#> Function: log2FoldChange (package iSEEde)
#> x="iSEEDESeq2Results"
#> x="iSEELimmaResults"
#> x="iSEEedgeRResults"
#> 
showMethods(averageLog2)
#> Function: averageLog2 (package iSEEde)
#> x="iSEEDESeq2Results"
#> x="iSEELimmaResults"
#> x="iSEEedgeRResults"
#> 
```
