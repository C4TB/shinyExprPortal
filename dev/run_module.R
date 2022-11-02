# Detach all loaded packages and clean your environment
rm(list=ls(all.names = TRUE))

# Document and reload your package
devtools::document()
devtools::load_all()

run_module(module_name = "moduleName",
           config_file = "../project/config.yaml",
           data_folder = "../project")
