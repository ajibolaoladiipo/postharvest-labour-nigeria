source_manifest_path <- "docs/SOURCE_MANIFEST.csv"

sha256_file <- function(path) {
  out <- tryCatch(system2("shasum", c("-a", "256", shQuote(path)), stdout = TRUE), error = function(e) character())
  if (!length(out)) stop("Unable to calculate SHA-256. Install a shasum-compatible utility.")
  strsplit(out[[1]], "[[:space:]]+")[[1]][1]
}

verify_source_manifest <- function(raw_dir, manifest_path = source_manifest_path) {
  if (!dir.exists(raw_dir)) stop("Restricted raw-data directory does not exist: ", raw_dir)
  manifest <- read.csv(manifest_path, stringsAsFactors = FALSE, check.names = FALSE)
  required <- c("filename", "sha256")
  if (!all(required %in% names(manifest))) stop("Source manifest lacks required fields.")
  status <- lapply(seq_len(nrow(manifest)), function(i) {
    path <- file.path(raw_dir, manifest$filename[i])
    exists <- file.exists(path)
    observed <- if (exists) sha256_file(path) else NA_character_
    data.frame(filename = manifest$filename[i], exists = exists,
               expected_sha256 = manifest$sha256[i], observed_sha256 = observed,
               matches = isTRUE(exists) && identical(tolower(observed), tolower(manifest$sha256[i])),
               stringsAsFactors = FALSE)
  })
  status <- do.call(rbind, status)
  if (nrow(status) != 8L || !all(status$matches)) {
    print(status)
    stop("Source validation failed. Do not run a mismatched or incomplete release without revalidation.")
  }
  message("PASS: all eight restricted source-file checksums match the Stage 3 manifest.")
  invisible(status)
}
