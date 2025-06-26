# init.R

# List required CRAN packages (all installable without system libraries)
required_packages <- c("gamstransfer", "dplyr", "jsonlite")

# Install and load packages
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message("Installing package: ", pkg)
    install.packages(pkg)
  }
  if (!require(pkg, character.only = TRUE)) {
    warning("Failed to load package '", pkg, "'. Retrying...")
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
  message("✔ Package ", pkg, " loaded successfully.")
}

# NOTE:
# 'gamstransfer' still requires GAMS to be installed and accessible on your system.
# If needed, manually set it: Sys.setenv(GMS_SYSDIR = "C:/GAMS/win64/44")
