# Contributing to shinyExprPortal

## Improvements

If you want to propose a small change to improve existing modules, please file an issue to go along with a pull request. To create a pull request, the following steps are useful:

* Fork and clone with `usethis::create_from_github("C4TB/shinyExprPortal", fork = TRUE)`
* Install all dependencies with `devtools::install_dev_deps()`
* Create a branch for the PR, e.g. `usethis::pr_init("what-is-changing")`
* Make changes and commit to your git, then run `usethis::pr_push` and wait for a review.

## New modules

We are happy to support the addition of new modules to the package when appropriate. If you have an idea for a module, you can use the support for custom modules to develop and test it before proposing to include in the package. Once you have a module that you think has a good number of use cases and will be relevant for more users, please file an issue on the GitHub and we take it from there.

To start with a new module, you can run the function `create_module_template("module_name")` inside the R folder of the repository (after forking and cloning it). Then you should follow the two guides below: adding a new module and design and programming principles. The principles should be followed unless there is a good reason not to.

## Adding a new module

### Structure

A module is always named with camelCase and, when finalised, should be included in the list of available modules in `inst/app-config.yml`. A module can require additional configuration settings or not, but to be included in an app, should have a value of TRUE in the configuration file, as follows:

```
[more configuration above]
moduleName:
  TRUE
[more configuration below]
```
The module will then consist of at least two files:

  ```
  mod_moduleName.R  
  mod_moduleName_config.R
  ```

The first file contains three functions: `mod_moduleName_ui`, `mod_moduleName_tab` and `mod_moduleName_server`. The second file should have a function called `mod_moduleName_config`, which receives `config` as argument. If, as above, the module does not require any configuration, the function will look as follows:
```
moduleNameConfig <- function(config, ...) { 
  message("Checking moduleName configuration")
  as.logical(config)
}
```
Otherwise, the function should return a transformed config object
```
moduleNameConfig <- function(config, ...) { 
  message("Checking moduleName configuration")
  config$argument <- doSomethingWith(config$argument)
  config
}
```

If required, any module-exclusive plotting or data functions can be included in a `mod_*_utils.R` file. The primary aim of the file is to reduce the amount of code in the main module file; but if any of the functions could potentially be used in more files, they should go in the general `utils_*.R` files. In the same way, if an author wants to use a function that exists in an existing `mod_*_utils.R` file in a more general way, they can rewrite it and transfer it to a `utils_*.R` file.

### Testing

The `run_module` function allows testing a newly developed module in isolation from the rest of the application (which enables using `browser()`, for example, or react logs). Before this is possible, you need to add the module name to the available_modules list in `inst/app-config.yml`, as mentioned above, and set up a .yaml configuration test file appropriately. This configuration file should be as similar as possible to a final configuration file, in terms of the data-related properties, and the new  module configuration. Then you can modify the function call in the end of the script `dev/run_module.R`, with `configuration.yaml` also located in `dev`:
```
# Run the application
run_module(module_name = "newModule",
           config_file = "configuration.yaml")
```

You can pass a data_folder argument to this function, e.g. `data_folder = "../testProject"`, if you want to keep data files and the configuration file on a separate folder.

### Documenting and deploying

The last step is the creation of a guide for configuring and using the new module. For that, you should modify the corresponding vignette (config.Rmd) and add an entry for the new module using the template from the other modules documented there.

## Design and programming principles

This section describes the principles guiding the design of the modules of shinyExprPortal. The aim is to establish the development boundaries (i.e. how much computation should be done within the app) and general readability vs. performance decisions. This should help to design and implement new modules.

### 1 Prioritizing base R

The app, as it is, already imports a relatively high  number of packages, and the aim is to avoid including new dependencies as much as possible for maintenance and customization reasons. It is better to preserve a cohesive app, modifying internal functions as required to support new functionality, rather than have modules with similar computations that rely on different packages.

The second reason is performance. It is tempting to use packages that include wrappers and convenience functions, e.g. to compute correlation and p-values simultaneously. With large expression matrices, it is probably better to use base R in most cases.

### 2 Reactives vs functions

The priority when writing a module is to separate reactive selections from the computations. The code inside reactive blocks will only serve to evaluate the required inputs and pass the arguments to functions. Example:

    computedData <- eventReactive(input$compute_button, { 
      
      req(input$slider_value)
      
      myFilterFunction(expression_matrix, input$slider_value)
    
    })
  
    output$plot <- renderPlot({ 
    
     plotBarChart(computedData())
    
    })
    
### 3 On functions

**Split and combine functions as needed:** If a plot or function result is similar to another, it is a good idea to combine them in a single function with varying parameters. At the same time, it's also a good idea to reduce a function to a bare minimum across modules, and have additional functions to complement it. Example: the basic scatterplot with a bit of customization as the base function, with faceting and fiting lines in separate functions.  

**Placement:** It is also better to leave module-specific functions in a module-specific file. In the future they can be moved to generic files if used across modules.

**Naming:** Most of the internal functions will not be exported, so it's preferable to give longer but descriptive names such as selectGenesIfTrue, instead of a generic name such as "filter".

### 4 Analysis results vs exploration trade-offs

The app enables visual exploration of data as well as results of the analysis, but it is not an actual analysis tool to do, e.g. clustering or differential expression analysis. Any results that require additional, non-standard packages (e.g. limma) should be computed separately and results included in files. Potential exceptions are those when file sizes are significantly higher than the computation time.