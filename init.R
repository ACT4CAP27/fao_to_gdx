# init.R

# Install and load required packages

required_packages <- c("gdxrrw", "dplyr")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}

# Note for gdxrrw users:
# 'gdxrrw' requires GAMS to be installed and its path correctly set in your system.
