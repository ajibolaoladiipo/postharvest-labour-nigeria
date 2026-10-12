required <- c(
  "README.md", "CITATION.cff", "DATA_ACCESS.md", "REPRODUCIBILITY.md",
  "RUN_REPRODUCTION.R", "environment/requirements.R",
  "docs/SOURCE_MANIFEST.csv", "reference_outputs/PAPER_3_STAGE4_REFERENCE_OUTPUTS_MANIFEST.csv",
  "tests/numerical_checks/05_verify_release_benchmarks.R"
)
missing <- required[!file.exists(required)]
if (length(missing)) stop("Required release files missing: ", paste(missing, collapse = ", "))
restricted <- list.files(".", pattern = "\\.(dta|rds|RData)$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
if (length(restricted)) stop("Restricted or row-level files present: ", paste(restricted, collapse = ", "))
all_text <- list.files(".", recursive = TRUE, full.names = TRUE)
all_text <- all_text[!grepl("\\.(png|docx)$", all_text, ignore.case = TRUE)]
patterns <- c(paste0("/", "Users/"), paste0("gh", "p_"), paste0("github", "_pat_"),
              paste0("AK", "IA"), paste0("BEGIN [A-Z ]*", "PRIVATE KEY"))
hits <- unlist(lapply(patterns, function(p) {
  grep(p, all_text, value = TRUE, ignore.case = FALSE)
}), use.names = FALSE)
if (length(unique(hits))) stop("Potential sensitive content found in: ", paste(unique(hits), collapse = ", "))
message("PASS: safe repository package structure and restricted-data exclusion verified.")
