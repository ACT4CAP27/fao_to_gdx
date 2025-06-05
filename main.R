# Filename: convert_to_gdx.R

# Install and load required packages if not already installed
if (!requireNamespace("gdxrrw", quietly = TRUE)) {
  # Note: gdxrrw often requires a GAMS installation to work correctly.
  # Ensure GAMS is installed and its system directory is in your PATH.
  install.packages("gdxrrw")
}
library(gdxrrw)

if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
library(dplyr)

# --- Configuration ---
# Define the relative path to the folder containing the downloaded RDS files
input_folder <- "inputs"
# Define the name for the output GDX file
gdx_output_filename <- "faostat_data.gdx"

# --- Main Script ---

# Construct the full path to the GDX file
gdx_output_file <- file.path(input_folder, gdx_output_filename)

if (!dir.exists(input_folder)) {
  stop(paste("Error: Input folder '", input_folder, "' not found. Please run 'download_faostat.R' first.", sep = ""))
}

message(paste("Searching for FAOSTAT RDS files in:", file.path(getwd(), input_folder)))

# List all RDS files that match the expected pattern (e.g., 'QC_data.rds')
rds_files <- list.files(input_folder, pattern = "_data\\.rds$", full.names = TRUE)

if (length(rds_files) == 0) {
  stop(paste("Error: No FAOSTAT RDS files found in '", input_folder, "'. Please ensure files like 'CODE_data.rds' exist.", sep = ""))
}

message(paste("Found", length(rds_files), "RDS files to process."))

# Initialize an empty list to hold GAMS object definitions for gdxrrw::wgdx.lst
gams_objects_for_gdx <- list()

# Loop through each found RDS file
for (file_path in rds_files) {
  # Extract the FAOSTAT code from the filename (e.g., "QC_data.rds" -> "QC")
  file_basename <- basename(file_path) # e.g., "QC_data.rds"
  fao_code <- sub("_data\\.rds$", "", file_basename) # e.g., "QC"

  message(paste("Processing file:", file_basename, "(FAOSTAT Code:", fao_code, ")"))

  tryCatch({
    df <- readRDS(file_path)

    # Ensure necessary columns for GDX conversion exist in the data frame.
    required_cols_for_gdx <- c("Area", "Item", "Element", "Year", "Value")
    if (!all(required_cols_for_gdx %in% names(df))) {
      warning(paste0("Skipping GDX conversion for data from '", file_basename, "' as it's missing required columns: ",
                     paste(setdiff(required_cols_for_gdx, names(df)), collapse = ", "),
                     ". This dataset will not be included in the GDX file."))
      next # Skip to the next file
    }

    # Prepare data for GDX: select relevant columns and ensure correct types.
    # The order of columns matters for gdxrrw: dimensions first, then the value.
    df_for_gdx <- df %>%
      select(Area, Item, Element, Year, Value) %>%
      mutate(Year = as.character(Year)) %>% # GAMS sets are often strings
      mutate(Value = as.numeric(Value))    # Ensure value is numeric

    # Define the GAMS parameter name using the FAOSTAT code
    gams_param_name <- paste0("p_", fao_code) # e.g., p_QC

    # Add the parameter definition to our list for gdxrrw::wgdx.lst
    gams_objects_for_gdx[[gams_param_name]] <- list(
      name = gams_param_name,
      type = "parameter",
      dim = 4, # Number of dimensions: Area, Item, Element, Year
      val = df_for_gdx,
      domains = c("Area", "Item", "Element", "Year") # These will be the GAMS set names for the parameter's domains
    )
  }, error = function(e) {
    warning(paste("Failed to process file '", file_basename, "': ", e$message, sep = ""))
  })
}

# Write all defined parameters to a single GDX file
if (length(gams_objects_for_gdx) > 0) {
  message("\nAttempting to write data to GDX file...")
  tryCatch({
    wgdx.lst(gdx_output_file, gams_objects_for_gdx)
    message(paste("Successfully created GDX file:", gdx_output_file))
  }, error = function(e) {
    warning(paste("Failed to write GDX file '", gdx_output_file, "': ", e$message, sep = ""))
    message("Please ensure GAMS is installed and its system directory is in your system's PATH environmental variable.")
  })
} else {
  message("No valid datasets were successfully prepared for GDX conversion.")
}

message("GDX conversion script finished.")