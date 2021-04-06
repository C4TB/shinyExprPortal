
<!-- README.md is generated from README.Rmd. Please edit that file -->

# clinvisx

<!-- badges: start -->
<!-- badges: end -->

clinvisx is configurable Shiny app for visual exploration of clinical
and transcriptomics studies. It includes multiple modules to facilitate
exploring associations in the results of such studies, as well as
showcasing analysis results (i.e. volcano plots).

## Installation

You can install the latest version of clinvisx with:

``` r
devtools::install_github("clinvisx")
```

Note that the command above will only install basic package dependencies
– the modules in the app have specific dependencies that need to be
installed separately. However, you can also run the following command to
install all packages:

``` r
devtools::install_github("clinvisx", dependencies = TRUE)
```

## Configuration and usage

Running clinvisx, in addition to all data files, requires writing a
.yaml configuration file. There is a vignette detailing how to configure
the global aspects of the app and each currently supported module. You
can check it on the github pages website or by running
`vignette("clinvisx_setup")`. There is also an example file included in
the dev folder in this repository.
