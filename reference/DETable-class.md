# The DETable class

The DETable class is a
[RowTable](https://isee.github.io/iSEE/reference/RowTable-class.html)
subclass that is dedicated to creating a volcano plot. It retrieves the
table of results for the selected differential expression contrast and
creates an interactive table where each row represents a feature.

## Slot overview

The following slots control the test procedure:

- `ContrastName`, a character scalar indicating the name of the contrast
  to display.

- `RoundDigits`, a logical scalar indicating whether to round numeric
  values (see `SignifDigits`).

- `SignifDigits`, an integer scalar indicating the number of significant
  digits to use for rounding numbers (see `RoundDigits`).

In addition, this class inherits all slots from its parent
[RowTable](https://isee.github.io/iSEE/reference/RowTable-class.html)
and [Table](https://isee.github.io/iSEE/reference/Table-class.html)
classes.

## Examples

``` r
x <- DETable()
x
#> Panel object of class DETable
#>   Get or set individual parameters with ‘[[’ 
#>   Available parameters:
#>     ContrastName: NA
#>     DataBoxOpen: FALSE
#>     HiddenColumns: 
#>     PanelHeight: 500
#>     PanelId: NA
#>     PanelWidth: 4
#>     RoundDigits: FALSE
#>     RowSelectionDynamicSource: FALSE
#>     RowSelectionSource: ---
#>     Search: 
#>     SearchColumns: 
#>     Selected: NA
#>     SelectionBoxOpen: FALSE
#>     SignifDigits: 3
#>     VersionInfo: list of length 1
```
