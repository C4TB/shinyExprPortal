library(ids)

n <- 100

clinical <- data.frame(
  Subject_ID =  toupper(random_id(n)),
  hb_m0 = rnorm(n, 10),
  hb_m6 = rnorm(n, 10),
  plt_m0 = rnorm(n, 100, 50) + 100,
  plt_m6 = rnorm(n, 100, 50) + 100,
  das_m0 = rpois(n, 2),
  das_m6 = rpois(n, 2)
)
saveRDS(clinical, "tests/testthat/clinical.rds")

sample_lookup <- data.frame(
  Subject_ID = rep(clinical$Subject_ID, 2),
  Sample_ID = c(paste0("r", 1:100) ,paste0("s", 1:100) ),
  Type = c(rep("r", 100), rep("s", 100))
)

saveRDS(sample_lookup, "tests/testthat/sample_lookup.rds")

expression <- rbind(
  rnorm(n * 2, 1) + 5,
  rnorm(n * 2) + 5,
  rnorm(n * 2) + 5
)
colnames(expression) <- sample_lookup$Sample_ID
rownames(expression) <- c("A", "B", "C")

saveRDS(expression, "tests/testthat/expression.rds")