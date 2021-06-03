# Set options here
options(golem.app.prod = FALSE) # TRUE = production mode, FALSE = development mode

# Detach all loaded packages and clean your environment
golem::detach_all_attached()
rm(list=ls(all.names = TRUE))

# Document and reload your package
golem::document_and_reload()
devtools::load_all()
# Run the application
# run_module(module_name = "cohortOverview",
#            config_file = "../ramap_test/test.yaml",
#            data_folder = "../ramap_test/")
run_module(module_name = "corrModules",
  config_file = "../psort_test/psort_reg.yaml",
  data_folder = "../psort_test")
