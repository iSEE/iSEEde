# The MAPlot class

The MAPlot is a
[RowDataPlot](https://isee.github.io/iSEE/reference/RowDataPlot-class.html)
subclass that is dedicated to creating an MA plot. It retrieves the
log-fold change (M) and mean average (A) values and creates a row-based
plot where each point represents a feature.

## Slot overview

The following slots control the test procedure:

- `ContrastName`, a character scalar indicating the name of the contrast
  to display.

In addition, this class inherits all slots from its parent
[RowDotPlot](https://isee.github.io/iSEE/reference/RowDotPlot-class.html),
[DotPlot](https://isee.github.io/iSEE/reference/DotPlot-class.html), and
[Panel](https://isee.github.io/iSEE/reference/Panel-class.html) classes.

## Examples

``` r
x <- MAPlot()
x
#> Panel object of class MAPlot
#>   Get or set individual parameters with ‘[[’ 
#>   Available parameters:
#>     BrushData: 
#>     ColorBy: None
#>     ColorByDefaultColor: black
#>     ColorByFeatureDynamicSource: FALSE
#>     ColorByFeatureName: NA
#>     ColorByFeatureNameColor: red
#>     ColorByFeatureSource: ---
#>     ColorByRowData: NA
#>     ColorBySampleDynamicSource: FALSE
#>     ColorBySampleName: NA
#>     ColorBySampleNameAssay: logcounts
#>     ColorBySampleSource: ---
#>     ContourAdd: FALSE
#>     ContourColor: blue
#>     ContrastName: NA
#>     CustomLabels: FALSE
#>     CustomLabelsText: NA
#>     DataBoxOpen: FALSE
#>     Downsample: FALSE
#>     DownsampleResolution: 200
#>     FacetColumnBy: None
#>     FacetColumnByRowData: NA
#>     FacetRowBy: None
#>     FacetRowByRowData: NA
#>     FixAspectRatio: FALSE
#>     FontSize: 1
#>     HoverInfo: TRUE
#>     LabelCenters: FALSE
#>     LabelCentersBy: NA
#>     LabelCentersColor: black
#>     LegendPointSize: 1
#>     LegendPosition: Bottom
#>     PanelHeight: 500
#>     PanelId: NA
#>     PanelWidth: 4
#>     PointAlpha: 1
#>     PointSize: 1
#>     RowSelectionDynamicSource: FALSE
#>     RowSelectionRestrict: FALSE
#>     RowSelectionSource: ---
#>     SelectionAlpha: 0.1
#>     SelectionBoxOpen: FALSE
#>     SelectionHistory: 
#>     ShapeBy: None
#>     ShapeByRowData: NA
#>     SizeBy: None
#>     SizeByRowData: NA
#>     TooltipRowData: 
#>     VersionInfo: list of length 1
#>     ViolinAdd: TRUE
#>     VisualBoxOpen: FALSE
#>     VisualChoices: Color
#>     ZoomData: 
```
