
<!-- README.md is generated from README.Rmd. Please edit that file -->

# shinyExprPortal

<!-- badges: start -->

[![R-CMD-check](https://github.com/C4TB/shinyExprPortal/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/C4TB/shinyExprPortal/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

*shinyExprPortal* is a configurable Shiny portal for the visual
exploration and analysis showcase of molecular expression data and
phenotype data, such as observed clinical measures. The package is
designed for deploying portals using a text-based configuration file
with minimal programming. The portal includes modules for exploring
correlations between expression and measures, visualizing the results of
differential expression analysis and showcasing results of other
downstream analysis methods such as pathways and correlated networks.
The interface is designed for gene expression, but the computational
methods are compatible with any matrix with samples in columns and
entities, such as proteins, in rows.

<img src="man/figures/example.png"/>

For initial exploration, any expression matrix containing abundance or
counts and data frames containing observed measures can be loaded in.
For differential expression and other methods, the package is compatible
with the outputs of packages such as limma and WCGNA. The correlation
modules can be used early in the lifecycle of a bioinformatics project,
while the other modules can be added as new downstream analysis results
are produced. The use of the configuration file also means that the
portal can be easily versioned.

A demo of the portal is available
[here](https://rhenkin.shinyapps.io/ramap_demo/).

## How to start

You can install the latest version of the package using:

``` r
install.packages("shinyExprPortal")
```

Alternatively, you can install the current development version using:

``` r
devtools::install_github("C4TB/shinyExprPortal", dependencies = TRUE)
```

If you only have one set of set of samples for a group of subjects,
i.e. one sample per patient, you can check
[`vignette("shinyExprPortal")`](https://c4tb.github.io/shinyExprPortal/articles/shinyExprPortal.html)
for a quick start guide to set up the configuration file.

If your subjects have one more samples, e.g. collected over time or from
different tissues, you should check the data preparation guide in
[`vignette("dataprep")`](https://c4tb.github.io/shinyExprPortal/articles/dataprep.html)
and the full configuration guide
[`vignette("config")`](https://c4tb.github.io/shinyExprPortal/articles/config.html)

Once you have completed either of the steps above, you can open and run
the app.R file created to test the portal.

## Module documentation

The full configuration guide in
[`vignette("config")`](https://c4tb.github.io/shinyExprPortal/articles/config.html)
describes all the dependencies, file requirements, mandatory and
optional settings for each of the modules currently supported by the
package.

## Customization and extending the package

[`vignette("customization")`](https://c4tb.github.io/shinyExprPortal/articles/customization.html)
describes how to customize the visual style of the portal and how to add
new functionality through programming new modules. The package supports
customization via the configuration file at a global level using
[bslib](https://CRAN.R-project.org/package=bslib). Some modules also
support the customization of the colors used in the visualizations.

The package is also compatible with new modules that can be added
without modifying the source code of the portal or the package. New
functionality can be developed and tested without having to clone the
repository or recompile the package. We are also open for suggestions of
new modules, please use the GitHub issues page for that.
