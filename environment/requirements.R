required <- c("haven", "dplyr", "tidyr", "readr", "stringr", "survey", "purrr", "ggplot2")
versions <- c(haven="2.5.5", dplyr="1.2.1", tidyr="1.3.2", readr="2.2.0", stringr="1.6.0", survey="4.5", purrr="1.2.2")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly=TRUE)]
if (length(missing)) stop("Install missing packages: ", paste(missing, collapse=", "))
installed <- vapply(names(versions), function(x) as.character(packageVersion(x)), character(1))
if (any(installed != versions)) warning("Package versions differ from the Stage 3 verified environment.")
message("Environment check complete.")
