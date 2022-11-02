#' Create example files
#'
#' @param path (optional) a directory where to create the files, otherwise uses
#'  the output of getwd()
#' @param full (optional) TRUE or FALSE (default); if TRUE, creates models and
#'  modules example files
#'
#' @export
#'
create_example <- function(path = NULL, full = FALSE) {
  set.seed(123)

  dir <- path %||% getwd()

  # app_r <- file(file_path(dir, "app.R"))
  # app_lines <- c(
  #   "library(clinvisx)",
  #   "run_app(config = \"config.yaml\")"
  # )
  # writeLines(app_lines, app_r)
  # close(app_r)

  n <- 100

  times <- c("m0", "m3", "m6")
  tissues <- c("A", "B")
  ids <- paste0("PROJ_", 1:n)

  sid_times <- paste0(rep(ids, each = 6), "_S", seq_along(times))
  tissue_suffix <- paste0("_T", rep(seq_along(tissues), each = 3))
  sids <- paste0(sid_times, tissue_suffix)

  da_m0 <- stats::rpois(n, 2)
  da_m3 <- pmax(0, da_m0 + sample(c(-1, 1), n, replace = T))
  da_m6 <- pmax(0, da_m3 + sample(c(-1, 1), n, replace = T))

  base_hb <- stats::rnorm(n, 10) + 10
  hb_cor <- 0.95
  hb_m3 <- hb_cor * base_hb + sqrt(1 - hb_cor * hb_cor) * stats::rnorm(n)
  hb_m6 <- hb_cor * base_hb + sqrt(1 - hb_cor * hb_cor) * stats::rnorm(n)

  measures_data <- data.frame(
    Subject_ID = ids,
    Haemoglobin_m0 = base_hb,
    Haemoglobin_m3 = hb_m3,
    Haemoglobin_m6 = hb_m6,
    Platelets_m0 = stats::rnorm(n, 100, 50) + 100,
    Platelets_m3 = stats::rnorm(n, 100, 50) + 100,
    Platelets_m6 = stats::rnorm(n, 100, 50) + 100,
    DiseaseActivity_m0 = da_m0,
    DiseaseActivity_m3 = da_m3,
    DiseaseActivity_m6 = da_m6,
    Sex = sample(c("F", "M"), n, TRUE),
    Age = floor(50 + 10 * stats::rnorm(n)),
    Drug = sample(c("Drug A", "Drug B"), n, TRUE),
    DrugNaive = ifelse(sample(stats::rpois(n, 3), n, FALSE) > 3, "Yes", "No")
  )

  utils::write.csv(measures_data,
                   file_path(dir, "measures_data.csv"),
                   row.names = FALSE)

  # Create fake gene symbols
  m <- 5000
  preffix <- do.call(paste0, replicate(4, sample(LETTERS, m, T), F))
  suffix <- do.call(paste0, replicate(3, sample(1:9, m, T), F))
  gene_names <- paste0(preffix, suffix)

  expression <-
    matrix(
      stats::rnorm(m * length(sids)),
      nrow = m,
      ncol = length(sids),
      dimnames = list(gene_names, sids)
    )

  utils::write.csv(expression,
                   file_path(dir, "expression_matrix.csv"),
                   row.names = TRUE)

  lookup <- data.frame(
    Subject_ID = rep(ids, each = 6),
    Sample_ID = sids,
    Time = rep(rep(times, times = 2), times = length(sids) / 6),
    Tissue = rep(rep(tissues, each = 3), times = length(sids) / 6),
    Drug = rep(measures_data$Drug, each = 6)
  )

  utils::write.csv(lookup,
                   file_path(dir, "lookup_table.csv"),
                   row.names = FALSE)
}
