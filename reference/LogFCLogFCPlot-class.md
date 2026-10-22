# The LogFCLogFCPlot class

The LogFCLogFCPlot class is a
[RowDataPlot](https://isee.github.io/iSEE/reference/RowDataPlot-class.html)
subclass that is dedicated to comparing the log-fold-change value of two
contrasts. It retrieves the log-fold change of the two selected
contrasts and creates a row-based plot where each point represents a
feature.

## Slot overview

The following slots control the test procedure:

- `ContrastNameX`, a character scalar indicating the name of the
  contrast to display on the x-axis.

- `ContrastNameY`, a character scalar indicating the name of the
  contrast to display on the y-axis.

In addition, this class inherits all slots from its parent
[RowDotPlot](https://isee.github.io/iSEE/reference/RowDotPlot-class.html),
[DotPlot](https://isee.github.io/iSEE/reference/DotPlot-class.html), and
[Panel](https://isee.github.io/iSEE/reference/Panel-class.html) classes.

## Examples

``` r
x <- LogFCLogFCPlot()
x
#> Panel object of class LogFCLogFCPlot
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
#>     ContrastNameX: NA
#>     ContrastNameY: NA
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
