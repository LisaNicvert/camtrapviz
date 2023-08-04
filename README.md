# camtrapviz

<!-- badges: start -->

[![CRAN status](https://www.r-pkg.org/badges/version/camtrapviz)](https://CRAN.R-project.org/package=camtrapviz) [![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental) [![R-CMD-check](https://github.com/LisaNicvert/camtrapviz/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/LisaNicvert/camtrapviz/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

*This package is currently under development*

`camtrapviz` is a R and Shiny package to visualize and summarize camera trap data. It is intended for camera trap data where species have already been tagged and compiled in tables.

The package website can be found at: <https://lisanicvert.github.io/camtrapviz/>.

## Installation

To install the package, use:

``` r
devtools::install_github("https://github.com/LisaNicvert/camtrapviz", 
                         build_vignettes = TRUE)
```

## Functions

### Shiny interface

A Shiny interface run from your computer allows to analyze camera trap data interactively. To run the app, use:

``` r
run_camtrapviz()
```

![](man/figures/shinyapp.png "Overview of the Shiny interface")

Two example datasets are provided:

-   [data](https://jniedballa.github.io/camtrapR/reference/recordTableSample.html) and [cameras](https://jniedballa.github.io/camtrapR/reference/camtraps.html) from the camtrapR package

-   a [camtrapDP datapackage](https://inbo.github.io/camtraptor/reference/mica.html) from the camtraptor R package.

You can also choose data to analyze from your computer.

<img src="man/figures/shinyapp_data_import.png" alt="Data import module" width="50%">

The Shiny app uses [Shinymeta](https://rstudio.github.io/shinymeta/) to display and export the underlying code to the Shiny app.

<img src="man/figures/show_code.png" alt="In-app plot with a show code button" style="display: inline-block;" width="48%"> <img src="man/figures/show_code_shown.png" alt="Plotting code displayed in the app" style="display: inline-block;" width="48%">

### Data reading and formatting

The package implements functions to read and clean data (see vignettes on [data import and cleaning](https://lisanicvert.github.io/camtrapviz/articles/read-and-clean-data.html) and on [data filtering](https://lisanicvert.github.io/camtrapviz/articles/filter-data.html)).

### Dataviz

There are several functions to visualize data that are illustrated below (using the example dataset from the [camtrapR](https://jniedballa.github.io/camtrapR/) package).

First, you can plot the observations of each camera versus time:

![](man/figures/plot_points.png "Plot observations")

You can also plot the cameras on a map:

![](man/figures/map.png "Cameras map")

It is also possible to plot capture event counts:

![](man/figures/plot_spp_bars.png "Capture events counts per species")

You can also plot the activity histogram and curve of the species:

![](man/figures/plot_activity.png "Activity plot")

For more details on plots, see the [vignette on plots](https://lisanicvert.github.io/camtrapviz/articles/plots.html) and the [vignette on activity plots](https://lisanicvert.github.io/camtrapviz/articles/activity-patterns.html).

### Data summary and analysis

Other functions allow more specific analyses:

-   summarize species or cameras information (see [vignette](https://lisanicvert.github.io/camtrapviz/articles/summarize.html))
-   get diversity indices at cameras from a species occurrence dataframe with [`get_diversity_indices`](https://lisanicvert.github.io/camtrapviz/reference/get_diversity_indices.html) (vignette will be developed shortly).
