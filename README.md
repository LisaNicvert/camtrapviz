# camtrapviz

*This package is currently under development*

`camtrapviz` is a R package designed to make it easier to visualize and summarize camera trap data for which species have already been identified.

For the moment, this package allows to:

-   read camera trap data from one or two CSV file(s) or the JSON metadata of a [camtrapDP data package](https://tdwg.github.io/camtrap-dp/)
-   format data for the analysis
-   summarize data (for now, it is only possible to summarize camera metadata)
-   plot data (for now, there are three plotting functions allowing to plot observations, cameras map and species count)

## Installation

To install the package, use:

``` r
devtools::install_github("https://github.com/LisaNicvert/camtrapviz", 
                         build_vignettes = TRUE)
```

## Shiny interface

A Shiny interface allows to analyze camera trap data interactively. To run the app, use:

``` r
run_camtrapviz()
```

Two example datasets are provided, and you can also choose data to analyze from your computer.

## Functions

The package also implements functions that can be used directly in R. These functions allow to:

-   read camera trap data with `read_data` (unique function to read camera trap data from one or two CSV file(s) or a JSON camtrapDP file)
-   format data with `clean_data` (cast column types and a unique CSV file into observations and cameras (if needed))
-   summarize data (for now, it is only possible to summarize camera metadata with `summarize_cameras`: activity start/end and sampling length)
-   plot data (plot observations with `plot_points`, cameras on an interactive map with `plot_map` (using `leaflet`) and species abundance with `plot_species_bars`)

### Vignettes

R vignettes are provided to exemplify a typical workflow of analysis (this is a work in progress).

The vignette `read-and-clean-data` explains how to read data from files and how to format data for further analyses.

``` r
vignette("read-and-clean-data", package = "camtrapviz")
```

The vignette `summarize` explains how to summarize camera information. There are also examples of plots.

``` r
vignette("summarize", package = "camtrapviz")
```
