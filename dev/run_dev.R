rm(list=ls(all.names = TRUE))

# Document and reload yo  ur package
devtools::document()
devtools::load_all()

# Run the application
run_app(config_file = "../project/config.yaml", data_folder = "../project", nthreads = 4)
