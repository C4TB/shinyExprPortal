# Set options here
options(golem.app.prod = FALSE) # TRUE = production mode, FALSE = development mode

# Detach all loaded packages and clean your environment
golem::detach_all_attached()
rm(list=ls(all.names = TRUE))

# Document and reload yo  ur package
golem::document_and_reload()

# Run the application
#run_app(config_file = "../ramap_test/test.yaml", data_folder = "../ramap_test/")
#run_app(config_file = "../ramap_test/config.yaml", data_folder = "../ramap_test/")
run_app(config_file = "../psort_test/psort_exp.yaml", data_folder = "../psort_test")
