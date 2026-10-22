# iSEEde

The goal of *[iSEEde](https://bioconductor.org/packages/iSEEde)* is to
provide panels that facilitate the interactive visualisation of
differential expression results in
*[iSEE](https://bioconductor.org/packages/iSEE)* applications.

## Installation instructions

Get the latest stable `R` release from
[CRAN](http://cran.r-project.org/). Then install
*[iSEEde](https://bioconductor.org/packages/iSEEde)* from
[Bioconductor](http://bioconductor.org/) using the following code:

\
`if`` ``(``!`[`requireNamespace`](https://rdrr.io/r/base/ns-load.html)`(``"BiocManager"``, quietly ``=`` ``TRUE``)``)`` ``{`\
`    `[`install.packages`](https://rdrr.io/r/utils/install.packages.html)`(``"BiocManager"``)`\
`}`\
\
`BiocManager``::`[`install`](https://bioconductor.github.io/BiocManager/reference/install.html)`(``"iSEEde"``)`

And the bleeding edge development version from
[GitHub](https://github.com/iSEE/iSEEde) with:

\
`BiocManager``::`[`install`](https://bioconductor.github.io/BiocManager/reference/install.html)`(``"iSEE/iSEEde"``)`

## Example

This is a basic example which shows you how to interactively explore
differential expression results for an example data set in an
*[iSEE](https://bioconductor.org/packages/iSEE)* application, using
designated panels implemented in
*[iSEEde](https://bioconductor.org/packages/iSEEde)*:

\
[`library`](https://rdrr.io/r/base/library.html)`(`[`"iSEEde"`](https://github.com/iSEE/iSEEde)`)`\
[`library`](https://rdrr.io/r/base/library.html)`(``"airway"``)`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`"DESeq2"`](https://github.com/thelovelab/DESeq2)`)`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`"iSEE"`](https://isee.github.io/iSEE/)`)`\
\
`# Example data ----`\
\
[`data`](https://rdrr.io/r/utils/data.html)`(``"airway"``)`\
`airway``$``dex`` ``<-`` `[`relevel`](https://rdrr.io/r/stats/relevel.html)`(``airway``$``dex``, ``"untrt"``)`\
\
`dds`` ``<-`` `[`DESeqDataSet`](https://rdrr.io/pkg/DESeq2/man/DESeqDataSet.html)`(``airway``, ``~`` ``0`` ``+`` ``dex`` ``+`` ``cell``)`\
\
`dds`` ``<-`` `[`DESeq`](https://rdrr.io/pkg/DESeq2/man/DESeq.html)`(``dds``)`\
`res_deseq2`` ``<-`` `[`results`](https://rdrr.io/pkg/DESeq2/man/results.html)`(``dds``, contrast ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``"dextrt"``, ``"dexuntrt"``)``)`\
\
`# iSEE / iSEEde ---`\
\
`airway`` ``<-`` `[`embedContrastResults`](https://isee.github.io/iSEEde/reference/utils-SummarizedExperiment.md)`(``res_deseq2``, ``airway``, name ``=`` ``"dex: trt vs untrt"``)`\
\
`app`` ``<-`` `[`iSEE`](https://isee.github.io/iSEE/reference/iSEE.html)`(``airway``, initial ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(`\
`  `[`DETable`](https://isee.github.io/iSEEde/reference/DETable-class.md)`(``ContrastName``=``"dex: trt vs untrt"``, HiddenColumns ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"baseMean"``, `\
`    ``"lfcSE"``, ``"stat"``)``, PanelWidth ``=`` ``4L``)``,`\
`  `[`VolcanoPlot`](https://isee.github.io/iSEEde/reference/VolcanoPlot-class.md)`(``ContrastName``=``"dex: trt vs untrt"``, PanelWidth ``=`` ``4L``)``,`\
`  `[`MAPlot`](https://isee.github.io/iSEEde/reference/MAPlot-class.md)`(``ContrastName``=``"dex: trt vs untrt"``, PanelWidth ``=`` ``4L``)`\
`)``)`\
\
`if`` ``(`[`interactive`](https://rdrr.io/r/base/interactive.html)`(``)``)`` ``{`\
`    ``shiny``::`[`runApp`](https://rdrr.io/pkg/shiny/man/runApp.html)`(``app``)`\
`}`

## Citation

Below is the citation output from using `citation('iSEEde')` in R.
Please run this yourself to check for any updates on how to cite
**iSEEde**.

\
[`print`](https://rdrr.io/r/base/print.html)`(`[`citation`](https://rdrr.io/r/utils/citation.html)`(``"iSEEde"``)``, bibtex ``=`` ``TRUE``)`\
`#> `\
`#> To cite package 'iSEEde' in publications use:`\
`#> `\
`#>   Rue-Albrecht K (2022). _iSEEde: iSEE extension for panels related to`\
`#>   differential expression analysis_. R package version 0.99.0,`\
`#>   <https://github.com/iSEE/iSEEde>.`\
`#> `\
`#> A BibTeX entry for LaTeX users is`\
`#> `\
`#>   @Manual{,`\
`#>     title = {iSEEde: iSEE extension for panels related to differential expression analysis},`\
`#>     author = {Kevin Rue-Albrecht},`\
`#>     year = {2022},`\
`#>     note = {R package version 0.99.0},`\
`#>     url = {https://github.com/iSEE/iSEEde},`\
`#>   }`

Please note that the
*[iSEEde](https://bioconductor.org/packages/iSEEde)* was only made
possible thanks to many other R and bioinformatics software authors,
which are cited either in the vignettes and/or the paper(s) describing
this package.

## Code of Conduct

Please note that the
*[iSEEde](https://bioconductor.org/packages/iSEEde)* project is released
with a [Contributor Code of
Conduct](http://bioconductor.org/about/code-of-conduct/). By
contributing to this project, you agree to abide by its terms.

## Development tools

- Continuous code testing is possible thanks to [GitHub
  actions](https://www.tidyverse.org/blog/2020/04/usethis-1-6-0/)
  through *[usethis](https://CRAN.R-project.org/package=usethis)*,
  *[remotes](https://CRAN.R-project.org/package=remotes)*, and
  *[rcmdcheck](https://CRAN.R-project.org/package=rcmdcheck)* customized
  to use [Bioconductor’s docker
  containers](https://www.bioconductor.org/help/docker/) and
  *[BiocCheck](https://bioconductor.org/packages/BiocCheck)*.
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
*[biocthis](https://bioconductor.org/packages/biocthis)*.

## Code of Conduct

Please note that the iSEEde project is released with a [Contributor Code
of Conduct](http://bioconductor.org/about/code-of-conduct/). By
contributing to this project, you agree to abide by its terms.
