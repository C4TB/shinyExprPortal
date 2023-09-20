lookup <- data.frame(
    sample_id = c("S1", "S2", "S3", "S4"),
    subject_id = c("P1", "P2", "P3", "P4"),
    col_a = c("treatment", "treatment", "control", "control"),
    col_b = c("low", "medium", "low", "high"),
    col_c = c("good", "*", "good", "good")
)

measures_data <- data.frame(
    subject_id = c("P1", "P2", "P3", "P4"),
    age = c(30, 35, 40, 45),
    sex = c("Male", "Male", "Male", "Female"),
    var_a = c(1, 2, 3, 4),
    var_b = c("a", "a", "b", "b")
)
