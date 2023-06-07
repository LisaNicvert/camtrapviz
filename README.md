# camtrapviz

*This package is currently under development*

`camtrapviz` is a R and Shiny package designed to make it easier to visualize and summarize camera trap data for which species have already been identified.

For the moment, this package allows to:

-   read camera trap data from one or two CSV file(s) or the JSON metadata of a [camtrapDP data package](https://tdwg.github.io/camtrap-dp/)
-   format data for the analysis
-   summarize cameras and species observation data
-   plot data

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

-   Read camera trap data: the function `read_data` allows to read camera trap data from one or two CSV file(s) or a JSON camtrapDP file.
-   Format data: the function `clean_data` essentially casts column types. If camera metadata and species observations are in the same file, it will also separate it into observations and cameras.
-   Summarize data: it is possible to summarize camera metadata with `summarize_camera` to get sampling start, end and length. Species observations can also be summarized with `summarize_species`. to get species sightings count, the number and proportions of cameras where a species was captured.
-   Fit an activity curve: the functions `fit_vonMises` and `vonMises_density` allow to fit and compute the density curve.
-   Plot data: several functions allow to visualize data: plot observations with `plot_points`, plot cameras on an interactive map with `plot_map` (using `leaflet`), plot species abundance with `plot_species_bars`...

### Vignettes

R vignettes are provided to exemplify a typical workflow of analysis (this is a work in progress).

The vignette `read-and-clean-data` explains how to read data from files and how to format data for further analyses.

``` r
vignette("read-and-clean-data", package = "camtrapviz")
```

The vignette `summarize` explains how to summarize camera and species information.

``` r
vignette("summarize", package = "camtrapviz")
```

The vignette `plots` exemplifies different plots.

```r
vignette("plots", package = "camtrapviz")
```
