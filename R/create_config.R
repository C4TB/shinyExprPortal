#' Create configuration and app.R files
#'
#' This function runs an interactive wizard that guides the user through the
#' creation of a basic configuration file. The wizard will work with the simple
#' case of expression data where one sample matches exactly to one subject.
#'
#' Before you run the wizard, you should ensure that the target folder contains
#' at least the expression matrix and measures data files. The expression
#' matrix should follow the format of sample IDs in columns and genes in rows,
#' with gene names in the first column of the table. The measures file should
#' follow the format of subjects in rows and measures in columns, and you should
#' ensure that all subjects have one sample and vice-versa.
#'
#' @param target_dir location where the configuration will be saved
#'
#' @return Creates configuration file in `target_dir`
#'
#' @examples
#' if (interactive()) {
#'     dir.create("newapp")
#'     create_config_wizard("newapp")
#' }
#' @export
#' @importFrom cli cli_h1 cli_h2 cli_h3 cli_text cli_ol cli_alert_danger
#' cli_alert cli_alert_success cli_alert_info cli_abort
create_config_wizard <- function(target_dir) {
    if (!interactive()) {
        stop("create_config must be run in an interactive terminal")
    }

    config <- list()

    cli::cli_h1("Configuration wizard")

    cli::cli_h2("Intro")

    cli::cli_text("This wizard will guide you through the creation of the
configuration file for an expression matrix, a measures file and an
optional sample metadata file. Please read the documentation to
ensure that all files are in the right format.")

    cli::cli_text("")

    cli::cli_text("Use this wizard if your data has one sample per
patient/subject. If your data contains multiple samples per patients, see
vignette(\"dataprep\") or the package website for a data preparation tutorial.")

    cli::cli_text("")

    cli::cli_text("The configuration file will include correlation and
differential expression visualisation modules. See vignette(\"fullguide\") or
the package website for a complete module configuration guide.")

    continue_yn <- readline("Do you want to continue? (y/n)")

    if ((continue_yn == "") || (continue_yn == "n")) {
        stop("\r Stopping creation of configuration file", call. = FALSE)
    }

    cli::cli_h2("Files setup")

    cli::cli_text("At the end of this wizard, the following files may be created
    in the folder {.file {target_dir}}:")

    cli::cli_text()

    cli::cli_ol(c(
        "The {.emph lookup table} that matches sample ids with
        subject ids in the measures file, named {.file lookup_table.csv}.",
        "The {.emph YAML configuration file} create by this wizardl, named
            {.file config.yaml}",
        "The {.file app.R} file that contains the code to run the portal with
        the config.yaml file.",
        "The {.emph models table} file, named {.file models.tsv}, that
        contains the DE model results to be showcased in the portal",
        "The landing page markdown document, named {.file about.md}, that you
        can edit do add project title, authors, description of the dataset,
        links to other websites and other relevant information."
    ))

    cli::cli_text()

    cli::cli_alert_danger(
        "Please note that if any of the files above already exists,
        they will be overwritten.",
        wrap = TRUE
    )

    cli::cli_text()

    continue_yn <- readline("Do you want to continue? (y/n)")

    if ((continue_yn == "") || (continue_yn == "n")) {
        stop("\r Stopping creation of configuration file", call. = FALSE)
    }

    expression_file <- readline("Enter name of expression file: ")
    if (expression_file == "") {
        stop("\r Stopping creation of configuration file", call. = FALSE)
    }

    cli::cli_alert("Reading {expression_file} in folder {target_dir}")
    expression_matrix <- read_file(expression_file, "expression_matrix", target_dir)
    sample_id <- colnames(expression_matrix)
    cli::cli_alert_success("File loaded successfully.")

    measures_file <- readline("Enter name of measures file: ")
    if (measures_file == "") {
        stop("\r Stopping creation of configuration file", call. = FALSE)
    }

    cli::cli_alert("Reading {measures_file} in folder {target_dir}")
    measures_table <- read_file(measures_file, data_folder = target_dir)
    cli::cli_alert_success("File loaded successfully.")

    cli::cli_text("If your measures table has a special column with ids for
                subjects, type the name now. Leave it blank to use the
                first column.")

    subject_col <- readline("Enter name of column:")

    if (subject_col == "") subject_col <- colnames(measures_table)[[1]]

    lookup_table <- data.frame(
        sample_id,
        measures_table[[subject_col]]
    )
    colnames(lookup_table) <- c("sample_id", subject_col)

    cli::cli_text("If you have a separate file with metadata separate from the
                measures, enter the name of the file. Leave it blank if you
                don't have one.")

    metadata_file <- readline("Enter name of sample metadata file:")

    if (metadata_file != "") {
        cli::cli_alert("Reading {metadata_file} in folder {target_dir}")
        metadata_table <- read_file(metadata_file, data_folder = target_dir)
        cli::cli_alert_success("File loaded successfully.")
        lookup_table <- cbind(lookup_table, metadata_table)
    } else {
        lookup_table$source <- "All samples"
    }

    cli::cli_alert(
        "Writing lookup table file named {.file lookup_table.csv}"
    )

    data.table::fwrite(lookup_table, file.path(target_dir, "lookup_table.csv"))

    cli::cli_alert_success("Done!")

    data_list <- list(
        measures_data = measures_file,
        sample_lookup = "lookup_table.csv",
        expression_matrix = expression_file
    )

    cli::cli_alert("Setting up sample categories selection section.")
    sample_categories <-
        lapply(seq.int(from = 3, to = ncol(lookup_table)), function(j) {
            name <- colnames(lookup_table)[[j]]
            values <- unique(lookup_table[, j])
            list(name = name, label = name, values = c(values))
        })
    cli::cli_alert_success("Done!")

    cli::cli_h2("Creating the configuration file")

    cli::cli_text("In the next steps, you will be asked to provide some
    information about your project to customize the file and choose the modules
    to appear on the portal.")

    cli::cli_text()

    cli::cli_text("The portal display the contents of a text, markdown or HTML
                file on the landing page. You can copy such a file to the
                folder now and enter its name in the prompt, or leave it blank
                so that a placeholder {.file about.md} file will be created.")

    about_file <- readline("Enter filename:")

    if (about_file == "") {
        about_file <- "about.md"
        cli::cli_alert("Creating placeholder {.file about.md} file")
        about_md <- file(file_path(target_dir, about_file))
        about_lines <- c(
            "This is a placeholder about.md file.",
            "Please edit it using your preferred text editor."
        )
        writeLines(about_lines, about_md)
        close(about_md)
        cli::cli_alert_success("Done!")
    } else if (!file.exists(file_path(target_dir, about_file))) {
        cli::cli_abort("File {.file {about_file}} not found in {.file {target_dir}}")
    }

    project_name <-
    readline("(optional) Enter a project acronym to appear on top left page:")

    if (project_name != "") {
        config$name <- project_name
    }
    config$about <- about_file
    config$data <- data_list
    config$sample_variable <- "sample_id"
    config$subject_variable <- subject_col
    config$sample_categories <- sample_categories

    cli::cli_h2("Modules setup")

    cli::cli_text("The last step of the wizard is about setting up modules for
    exploring correlations and visualizing results of differential expression
    analysis.")

    cli::cli_text()

    cli::cli_text("You can also skip this section and complete the rest of the
                configuration file by following the documentation on the
                website.")

    continue_yn <- ""

    while ((continue_yn != "n") && (continue_yn != "y")) {
        continue_yn <- readline("Do you want to continue? (y/n)")
    }

    if (continue_yn == "n") {
        cli::cli_alert("The configuration file will now be created.")
        cli::cli_alert_info("Please check the package documnetation to complete
                        the configuration file and set up the modules you
                        want to view on the portal.", wrap = TRUE)

        cli::cli_alert("Creating {.file config.yaml} file in {.file {target_dir}}")
        yaml::write_yaml(config, file.path(target_dir, "config.yaml"))
        cli::cli_alert_success("Done!")
    }

    cli::cli_h3("Correlation modules")

    cli::cli_text("The correlation modules will be set up to include all
                numeric variables found in the measures table. After this
                wizard is complete, you can open the configuration file and
                change as you see fit.")

    cli::cli_text()

    cli::cli_text("For these modules, there is also the option to enable
    advanced settings to enable excluding outliers and modifying the correlation
    measure.")

    advanced_yn <- ""
    while ((advanced_yn != "n") && (advanced_yn != "y")) {
        advanced_yn <- readline("Do you want to enable these settings? (y/n)")
    }

    numeric_variables <- measures_table %>%
        select(where(is.numeric)) %>%
        colnames()

    chunk_cols <-
        function(x, n) split(x, ceiling(seq_along(x) / n))

    numeric_var_groups <- chunk_cols(numeric_variables, 5)
    names(numeric_var_groups) <- paste("Group", names(numeric_var_groups))

    sgcConfig <- list(
        tabs = list(
            list(
                name = "Measures",
                scale = "independent",
                variables = numeric_variables
            )
        )
    )
    svcConfig <- list()
    mvcConfig <- list(heatmap_variables = numeric_var_groups)

    if (advanced_yn == "y") {
        advanced_config <- list(
            measures_data = TRUE,
            correlation_method = TRUE,
            fit_method = TRUE
        )
        sgcConfig$advanced <- advanced_config
        svcConfig$advanced <- advanced_config
        mvcConfig$advanced <- advanced_config
    }

    config$singleGeneCorr <- sgcConfig
    config$singleMeasureCorr <- svcConfig
    config$multiMeasureCorr <- mvcConfig

    Sys.sleep(1)
    cli::cli_alert_success("Done!")

    cli::cli_h3("DE Module")
    cli::cli_text("The module for visualizing differential expression results
                requires that all model results files are placed inside a
                {.file models} folder inside {.file {target_dir}}. Any file created
                using DESeq2, limma or edgeR can be used.")
    cli::cli_alert_info("The folder will now be checked for files. If you have
                not placed the files there, you can do it now, then
                type y in the prompt to continue. If the folder does
                not exist, this step will be skipped.", wrap = TRUE)

    continue_yn <- ""
    while ((continue_yn != "n") && (continue_yn != "y")) {
        continue_yn <- readline("Are you ready to continue? (y/n)")
    }

    if (continue_yn == "y") {
        if (dir.exists(file.path(target_dir, "models"))) {
            file_list <- list.files(file.path(target_dir, "models"))
            cli::cli_text("Adding the following files to files table:")
            cli::cli_li(file_list)
            models_table <- data.frame(
                "Category" = rep("Models", length(file_list)),
                "Model" = file_list
            )
            cli::cli_alert("Creating {.file models.tsv} file in {.file {target_dir}}")
            data.table::fwrite(models_table, file = "models.tsv", sep = "\t")
            cli::cli_alert_success("Done!")
            degDetailsConfig <- list(
                models = "models.tsv",
                category_variable = "Category"
            )
            config$degDetails <- degDetailsConfig
        } else {
            cli::cli_alert_warning("Skipping DE module configuration. Please
            read the package documentation to manually set it up",
                wrap = TRUE
            )
        }
    }

    cli::cli_h2("Final steps")
    cli::cli_alert("The configuration file will now be created.")
    cli::cli_alert_info("Please check the package documentation to complete
    the configuration file, set up additional modules and customize the
    portal.", wrap = TRUE)
    Sys.sleep(1)
    cli::cli_alert("Creating {.file config.yaml} file in {.file {target_dir}}")
    yaml::write_yaml(config, file.path(target_dir, "config.yaml"))
    cli::cli_alert_success("Done!")
    Sys.sleep(1)
    cli::cli_alert("Creating {.file app.R} file in {.file {target_dir}}")
    app_r <- file(file_path(target_dir, "app.R"))
    app_lines <- c(
        "library(shinyExprPortal)",
        "run_app(config = \"config.yaml\")"
    )
    writeLines(app_lines, app_r)
    close(app_r)

    cli::cli_alert_success("Done!")
    cli::cli_text()
    cli::cli_alert_success("The configuration wizard ends here. Please \\
    check the package documentation and website to setup \\
    additional modules and further customisation.", wrap = TRUE)
    cli::cli_text("To test the portal, open the app.R file and source it.")
}


#' Create a bare-bones configuration file template
#'
#' The resulting file contain placeholder text in upper case for you to edit
#' according to your needs. It also includes the three correlation modules by
#' default.
#'
#' @param target_dir location to create the configuration file
#' @param filename optional file name, default is config.yaml
#'
#' @return Creates configuration file in `target_dir`
#'
#' @examples
#' if (interactive()) {
#'     dir.create("newapp")
#'     create_config_template("newapp")
#' }
#'
#' @export
create_config_template <-
    function(target_dir, filename = "config.yaml") {
        config <- list(
            name = "REPLACE_WITH_PROJECT_NAME",
            logo = "PROJECT_LOGO.png",
            about = "about.md",
            data = list(
                measures_data = "MEASURES_TABLE_FILENAME",
                lookup_table = "LOOKUP_TABLE_FILENAME",
                expression_matrix = "EXPRESSION_MATRIX_FILENAME"
            ),
            sample_variable = "SAMPLE_COLUMN_IN_LOOKUPTABLE",
            subject_variable = "SUBJECT_COLUMN_IN_LOOKUPTABLE",
            sample_categories = list(
                list(
                    name = "SUBSET_VARIABLE_1",
                    label = "LABEL_FOR_INTERFACE_1",
                    values = c("VAR1_VALUE1", "VAR2_VALUE")
                ),
                list(
                    name = "SUBSET_VARIABLE_2",
                    label = "LABEL_FOR_INTERFACE_2",
                    values = c("VAR2_VALUE1", "VAR2_VALUE2")
                )
            ),
            singleGeneCorr = list(
                tabs = list(
                    list(
                        name = "TAB NAME",
                        scale = "independent",
                        variables = c("MEASURES_VAR_1", "MEASURES_VAR_2")
                    )
                ),
                advanced = list(
                    correlation_method = TRUE
                )
            ),
            singleMeasureCorr = list(
                advanced = list(
                    correlation_method = TRUE
                )
            ),
            multiMeasureCorr = list(
                heatmap_variables = list(
                    list(
                        GROUP_NAME = c("MEASURESL_VAR_1", "MEASURES_VAR_2")
                    )
                ),
                advanced = list(
                    correlation_method = TRUE
                )
            )
        )

        yaml::write_yaml(config, file.path(target_dir, filename))
    }
