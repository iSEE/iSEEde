# Generics for Embbedding Results into a SummarizedExperiment Object

An overview of the generics for embedding results into a
[SummarizedExperiment](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html)
object, in a format compatible with iSEEde.

## Usage

``` r
embedContrastResults(x, se, name, ...)

embedContrastResultsMethods

# S4 method for class 'ANY'
embedContrastResults(x, se, name, ...)

# S4 method for class 'data.frame'
embedContrastResults(x, se, name, class, ...)
```

## Format

`embedContrastResultsMethods`: Named character vector mapping keywords
to class names designed to store differential expression results.

## Arguments

- x:

  Object to be embedded.

- se:

  A
  [SummarizedExperiment](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html)
  object.

- name:

  Identifier for the embedded object.

- ...:

  Arguments passed to and from other methods.

- class:

  Class to use for embedding `x`. Only used when `class(x)` does not
  uniquely identify the package that generated the object.

## Value

An updated
[SummarizedExperiment](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html)
object that contains the embedded object.

## Definitions

- `embedContrastResults(x, se, name, ...)` embeds the results `x` in the
  [SummarizedExperiment](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html)
  `se`.

## Author

Kevin Rue-Albrecht

## Examples

``` r
embedContrastResultsMethods
#>              limma 
#> "iSEELimmaResults" 

showMethods(embedContrastResults)
#> Function: embedContrastResults (package iSEEde)
#> x="ANY"
#> x="DESeqResults"
#> x="TopTags"
#> x="data.frame"
#> x="iSEEDESeq2Results"
#> x="iSEELimmaResults"
#> x="iSEEedgeRResults"
#> 
```
