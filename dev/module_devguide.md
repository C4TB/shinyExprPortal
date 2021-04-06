# Module development guide

Created: 26/03/2021  
Latest update: 26/03/2021

### Intro

The Shiny app is intended to showcase analysis results and enable custom exploration of the underlying data used for analysis. This means that the scope of the app includes *showing* results of differential expression analysis, but does not extend to *computing* differentially expressed genes. Any new module should take that into account. In addition to the guidelines and principles mentioned in `devguide.md`, this file includes other requirements and suggestions to developing new modules for the app.

### Structure

A module is always named with camelCase and, when finalised, should be included in the list of available modules in `inst/golem_config.yml`. A module can require additional configuration settings or not, but to be included in an app, should have a value of TRUE in the configuration file, as follows:

```
[more configuration above]
moduleName:
  TRUE
[more configuration below]
```
The module will then consist of at least two files, which should be created and listed using `R/02_dev.R`, for tracking purposes. Any new dependency should also be added through this file.

  ```
  mod_moduleName.R  
  mod_moduleName_config.R
  ```

The first file contains three functions: `mod_moduleName_ui`, `mod_moduleName_tab` and `mod_moduleName_server` (see `dev/module_file_template.R`). The second file should have a function called `mod_moduleName_config`, which receives `config` as argument. If, as above, the module does not require any configuration, the function will look as follows:
```
moduleNameConfig <- function(config, data_folder = "") { 
  message("Checking moduleName configuration")
  as.logical(config)
}
```
Otherwise, the function should return a transformed config object
```
moduleNameConfig <- function(config, data_folder = "") { 
  message("Checking moduleName configuration")
  config$argument <- doSomethingWith(config$argument)
  config
}
```
Going back to the main module file, the `mod_*_ui` function receives `id` and `appdata` as arguments. The first argument is the module ID and will be taken from the configuration file; the second argument is the object that is parsed by the app when loading the configuration. This function then calls the `mod_*_tab` function with any arguments required by that function, e.g. particular module variables, global variables, etc. Finally, the `mod_*_server` function receives `id` and `appdata` as arguments just like the ui function.

If required, any module-exclusive plotting or data functions can be included in a `mod_*_utils.R` file. The primary aim of the file is to reduce the amount of code in the main module file; but if any of the functions could potentially be used in more files, they should go in the general `utils_*.R` files. In the same way, if an author wants to use a function that was in an existing `mod_*_utils.R` file in a more general way, they can rewrite it and transfer it to a `utils_*.R` file.

### Testing

The `run_module` function allows testing a newly develoepd module in an isolated manner, from the rest of the application (which enables using `browser()`, for example, or react logs). Before this is possible, you need to add the module name to the available_modules list in `inst/golem-config.yml`, as mentioned above, and set up a .yaml configuration file appropriately. This configuration file should be as similar as possible to a final configuration file, with name/logo, data and global sections, as well as the specific module configuration. Then you can modify the function call in the end of the script `dev/run_module.R`, with `configuration.yaml` also located in `dev`:
```
# Run the application
run_module(module_name = "newModule",
           config_file = "configuration.yaml")
```

### Deploying

The last step is the creation of a guide for configuring and using the new module. A vignette, similar to the others in `vignettes`, should be created and guide the user step by step through the configuration and end-user interaction with the module.
