# init.R

# Install and load required packages

# IMPORTANT: gdxrrw is deprecated and no longer maintained.
# It has been replaced by the 'gamstransfer' package, which is available on CRAN.
# 'gamstransfer' still requires a GAMS installation on your system.

required_packages <- c("gamstransfer", "dplyr", "jsonlite", "jsonvalidate")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(paste("Installing package:", pkg))
    install.packages(pkg)
  }
  # Check if the package loaded successfully, if not, try to install again (sometimes needed)
  if (!require(pkg, character.only = TRUE)) {
    warning(paste("Failed to load package '", pkg, "'. Attempting re-installation.", sep = ""))
    install.packages(pkg)
    library(pkg, character.only = TRUE) # Try loading again
  }
  message(paste("Package", pkg, "loaded successfully."))
}

# Note for gamstransfer users:
# 'gamstransfer' requires GAMS to be installed on your system.
# Ensure GAMS is installed and its system directory is accessible (e.g., in your system's PATH).
# If you encounter issues, you might need to set the GAMS system directory manually,
# for example: Sys.setenv(GMS_SYSDIR = "C:/GAMS/win64/44") (adjust path to your GAMS installation)