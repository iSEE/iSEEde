
<!-- README.md is generated from README.Rmd. Please edit that file -->

# iSEEde

<!-- badges: start -->

[![GitHub
issues](https://img.shields.io/github/issues/iSEE/iSEEde)](https://github.com/iSEE/iSEEde/issues)
[![GitHub
pulls](https://img.shields.io/github/issues-pr/iSEE/iSEEde)](https://github.com/iSEE/iSEEde/pulls)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check-bioc](https://github.com/iSEE/iSEEde/workflows/R-CMD-check-bioc/badge.svg)](https://github.com/iSEE/iSEEde/actions)
[![Codecov test
coverage](https://codecov.io/gh/iSEE/iSEEde/branch/main/graph/badge.svg)](https://app.codecov.io/gh/iSEE/iSEEde?branch=main)
<!-- badges: end -->

The goal of `iSEEde` is to provide panels that facilitate the
interactive visualisation of differential expression results in
*[iSEE](https://bioconductor.org/packages/3.16/iSEE)* applications.

## Installation instructions

Get the latest stable `R` release from
[CRAN](http://cran.r-project.org/). Then install `iSEEde` from
[Bioconductor](http://bioconductor.org/) using the following code:

``` r
if (!requireNamespace("BiocManager", quietly = TRUE)) {
    install.packages("BiocManager")
}

BiocManager::install("iSEEde")
```

And the development version from
[GitHub](https://github.com/iSEE/iSEEde) with:

``` r
BiocManager::install("iSEE/iSEEde")
```

## Example

This is a basic example which shows you how to load the package:

``` r
library("iSEEde")
library("airway")
library("DESeq2")
library("iSEE")

# Example data ----

data("airway")
airway$dex <- relevel(airway$dex, "untrt")

dds <- DESeqDataSet(airway, ~ 0 + dex + cell)

dds <- DESeq(dds)
res_deseq2 <- results(dds, contrast = list("dextrt", "dexuntrt"))

# iSEE / iSEEde ---

airway <- embedContrastResults(res_deseq2, airway, name = "dex: trt vs untrt")

app <- iSEE(airway, initial = list(
  DETable(ContrastName="dex: trt vs untrt", HiddenColumns = c("baseMean", 
    "lfcSE", "stat"), PanelWidth = 4L),
  VolcanoPlot(ContrastName="dex: trt vs untrt", PanelWidth = 4L),
  MAPlot(ContrastName="dex: trt vs untrt", PanelWidth = 4L)
))

if (interactive()) {
    shiny::runApp(app)
}
```

## Citation

Below is the citation output from using `citation('iSEEde')` in R.
Please run this yourself to check for any updates on how to cite
**iSEEde**.

``` r
print(citation("iSEEde"), bibtex = TRUE)
#> 
#> To cite package 'iSEEde' in publications use:
#> 
#>   Rue-Albrecht K (2022). _iSEEde: iSEE extension for panels related to
#>   differential expression analysis_. R package version 0.99.0,
#>   <https://github.com/iSEE/iSEEde>.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {iSEEde: iSEE extension for panels related to differential expression analysis},
#>     author = {Kevin Rue-Albrecht},
#>     year = {2022},
#>     note = {R package version 0.99.0},
#>     url = {https://github.com/iSEE/iSEEde},
#>   }
```

Please note that the `iSEEde` was only made possible thanks to many
other R and bioinformatics software authors, which are cited either in
the vignettes and/or the paper(s) describing this package.

## Code of Conduct

Please note that the `iSEEde` project is released with a [Contributor
Code of Conduct](http://bioconductor.org/about/code-of-conduct/). By
contributing to this project, you agree to abide by its terms.

## Development tools

- Continuous code testing is possible thanks to [GitHub
  actions](https://www.tidyverse.org/blog/2020/04/usethis-1-6-0/)
  through *[usethis](https://CRAN.R-project.org/package=usethis)*,
  *[remotes](https://CRAN.R-project.org/package=remotes)*, and
  *[rcmdcheck](https://CRAN.R-project.org/package=rcmdcheck)* customized
  to use [Bioconductor’s docker
  containers](https://www.bioconductor.org/help/docker/) and
  *[BiocCheck](https://bioconductor.org/packages/3.16/BiocCheck)*.
- Code coverage assessment is possible thanks to
  [codecov](https://codecov.io/gh) and
  *[covr](https://CRAN.R-project.org/package=covr)*.
- The [documentation website](http://iSEE.github.io/iSEEde) is
  automatically updated thanks to
  *[pkgdown](https://CRAN.R-project.org/package=pkgdown)*.
- The code is styled automatically thanks to
  *[styler](https://CRAN.R-project.org/package=styler)*.
- The documentation is formatted thanks to
  *[devtools](https://CRAN.R-project.org/package=devtools)* and
  *[roxygen2](https://CRAN.R-project.org/package=roxygen2)*.

For more details, check the `dev` directory.

This package was developed using
*[biocthis](https://bioconductor.org/packages/3.16/biocthis)*.

## Code of Conduct

Please note that the iSEEde project is released with a [Contributor Code
of Conduct](http://bioconductor.org/about/code-of-conduct/). By
contributing to this project, you agree to abide by its terms.
