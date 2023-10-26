#' Create example files
#'
#' Create example files for measures, expression matrix and lookup table
#'
#' @param target_dir location where to create the files
#'
#' @return Create examples files in target_dir
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'     dir.create("newapp")
#'     create_example("newapp")
#' }
create_example <- function(target_dir) {

    n <- 100

    times <- c("m0", "m3", "m6")
    tissues <- c("A", "B")
    ids <- paste0("PROJ_", seq_len(n))

    sid_times <- paste0(rep(ids, each = 6), "_S", seq_along(times))
    tissue_suffix <- paste0("_T", rep(seq_along(tissues), each = 3))
    sids <- paste0(sid_times, tissue_suffix)

    da_m0 <- stats::rpois(n, 2)
    da_m3 <- pmax(0, da_m0 + sample(c(-1, 1), n, replace = TRUE))
    da_m6 <- pmax(0, da_m3 + sample(c(-1, 1), n, replace = TRUE))

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
        DrugNaive = ifelse(sample(stats::rpois(n, 3), n, FALSE) > 3,
            "Yes", "No")
    )

    utils::write.csv(measures_data,
        file_path(target_dir, "measures_data.csv"),
        row.names = FALSE
    )

    # Create fake gene symbols
    m <- 100
    preffix <- do.call(paste0, replicate(4, sample(LETTERS, m, TRUE), FALSE))
    suffix <- do.call(paste0, replicate(3, sample(1:9, m, TRUE), FALSE))
    gene_names <- paste0(preffix, suffix)

    expression <-
        matrix(
            round(stats::rnorm(m * length(sids)) +
                      stats::rpois(m*length(sids), 4), 2),
            nrow = m,
            ncol = length(sids),
            dimnames = list(gene_names, sids)
        )

    utils::write.csv(expression,
        file_path(target_dir, "expression_matrix.csv"),
        row.names = TRUE
    )

    lookup <- data.frame(
        Subject_ID = rep(ids, each = 6),
        Sample_ID = sids,
        Time = rep(rep(times, times = 2), times = length(sids) / 6),
        Tissue = rep(rep(tissues, each = 3), times = length(sids) / 6),
        Drug = rep(measures_data$Drug, each = 6)
    )

    utils::write.csv(lookup,
        file_path(target_dir, "lookup_table.csv"),
        row.names = FALSE
    )
}
