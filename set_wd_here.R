

#' When script is run, sets current directory for R to path location of script
# Attempt to locate path with base R
proj_path <-tryCatch(dirname(sys.frame(1)$ofile), dir1=NULL, error = function(e) {NULL})
# Attempt to locate path with rstudioapi package
if (is.null(proj_path)) {
  if(!require(rstudioapi)){ install.packages("rstudioapi") }
  library(rstudioapi)
  proj_path <-tryCatch( dirname(rstudioapi::getActiveDocumentContext()$path))
}
# Set w
setwd(proj_path)

# Create temp folder
dir.create(file.path(getwd(), "temp"), showWarnings = FALSE)
