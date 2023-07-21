# camtrapviz

*This package is currently under development*

`camtrapviz` is a R and Shiny package designed to make it easier to visualize and summarize camera trap data. it is intended for data where species have already been tagged and compiled in tables. The package website can be found at: <https://lisanicvert.github.io/camtrapviz/>.

For the moment, this package allows to:

-   read and format camera trap data
-   plot data
-   summarize and analyze data

## Installation

To install the package, use:

``` r
devtools::install_github("https://github.com/LisaNicvert/camtrapviz", 
                         build_vignettes = TRUE)
```

## Shiny interface

A Shiny interface run from your computer allows to analyze camera trap data interactively. To run the app, use:

``` r
run_camtrapviz()
```

Two example datasets from the [`camtrapR`](https://jniedballa.github.io/camtrapR/) and [`camtraptor`](https://inbo.github.io/camtraptor/index.html) R packages are provided. You can also choose data to analyze from your computer.

## Data reading and formatting

The package implements functions to read and clean data. Below are a few examples:

-   [`read_data`](https://lisanicvert.github.io/camtrapviz/reference/read_data.html) allows to read camera trap data from one or two CSV file(s) or the JSON metadata of a [camtrapDP data package](https://tdwg.github.io/camtrap-dp/).
-   [`filter_data`](https://lisanicvert.github.io/camtrapviz/reference/filter_data.html) allows to select a subset of camera trap data based on species, cameras and dates.

## Dataviz

There are several functions to visualize data. Some are illustrated below (using the example dataset frop the [`camtrapR`](https://jniedballa.github.io/camtrapR/) package).

First, you can plot the observations of each camera versus time with the [`plot_points`](https://lisanicvert.github.io/camtrapviz/reference/plot_points.html) function:

![Plot observations](man/figures/plot_points.png)

You can also plot the cameras on a map with the [`plot_map`](https://lisanicvert.github.io/camtrapviz/reference/plot_map.html) function (interactive leaflet map):

![Cameras map](man/figures/map.png)

It is also possible to plot capture event counts with [`plot_species_bars`](https://lisanicvert.github.io/camtrapviz/reference/plot_species_bars.html):

![Capture events counts per species](man/figures/plot_spp_bars.png)

For more details on plots, see the [plots vignette](https://lisanicvert.github.io/camtrapviz/articles/plots.html).

## Data summary and analysis

Other functions allow more specific analyses:

-   [`summarize_camera`](https://lisanicvert.github.io/camtrapviz/reference/summarize_cameras.html) allows to get sampling start, end and length.
-   [`summarize_species`](https://lisanicvert.github.io/camtrapviz/reference/summarize_species.html) allows to get species sightings count, the number and proportions of cameras where a species was captured.
-   [`get_diversity_table`](https://lisanicvert.github.io/camtrapviz/reference/get_diversity_table.html) computes various diversity indices from a species occurrence dataframe.
-   [`fit_vonMises`](https://lisanicvert.github.io/camtrapviz/reference/fit_vonMises.html) and [`vonMises_density`](https://lisanicvert.github.io/camtrapviz/reference/vonMises_density.html) allow to fit and compute the activity density curve.

### Vignettes

R vignettes are provided to exemplify the packages functionalities (this is a work in progress). Vignettes are available on the [package's website](https://lisanicvert.github.io/camtrapviz/).
