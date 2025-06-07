# Filename: convert_csv_to_gdx_with_gamstransfer.R

# Install and load required packages if not already installed
if (!requireNamespace("gamstransfer", quietly = TRUE)) {
  # Install from CRAN (recommended)
  install.packages("gamstransfer")
  # Or from GAMS R-universe for latest version (often more up-to-date)
  # install.packages("gamstransfer", repos = c("https://gams-dev.r-universe.dev", "https://cloud.r-project.org"))
}
library(gamstransfer)

if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
library(dplyr)

# --- Configuration ---
# Define the relative path to the folder containing the downloaded CSV files
input_folder <- "inputs"
# Define the name for the output GDX file
gdx_output_filename <- "faostat_data.gdx"

# --- Main Script ---

# Construct the full path to the GDX file
gdx_output_file <- file.path(input_folder, gdx_output_filename)

if (!dir.exists(input_folder)) {
  stop(paste("Error: Input folder '", input_folder, "' not found. Please ensure your CSV files are in this directory.", sep = ""))
}

message(paste("Searching for FAOSTAT CSV files in:", file.path(getwd(), input_folder)))

# List all CSV files that match the expected pattern (e.g., 'QC_data.csv')
csv_files <- list.files(input_folder, pattern = "_data\\.csv$", full.names = TRUE)

if (length(csv_files) == 0) {
  stop(paste("Error: No FAOSTAT CSV files found in '", input_folder, "'. Please ensure files like 'CODE_data.csv' exist.", sep = ""))
}

message(paste("Found", length(csv_files), "CSV files to process."))

# Initialize a GAMS Container object
m <- Container$new()

# Loop through each found CSV file
for (file_path in csv_files) {
  # Extract the FAOSTAT code from the filename (e.g., "QC_data.csv" -> "QC")
  file_basename <- basename(file_path) # e.g., "QC_data.csv"
  fao_code <- sub("_data\\.csv$", "", file_basename) # e.g., "QC"

  message(paste("Processing file:", file_basename, "(FAOSTAT Code:", fao_code, ")"))

  tryCatch({
    df <- read.csv(file_path, stringsAsFactors = FALSE) # stringsAsFactors=FALSE is good practice for text columns

    # Ensure necessary columns for GDX conversion exist in the data frame.
    required_cols_for_gdx <- c("Area", "Item", "Element", "Year", "Value")
    if (!all(required_cols_for_gdx %in% names(df))) {
      warning(paste0("Skipping GDX conversion for data from '", file_basename, "' as it's missing required columns: ",
                     paste(setdiff(required_cols_for_gdx, names(df)), collapse = ", "),
                     ". This dataset will not be included in the GDX file."))
      next # Skip to the next file
    }

    # Prepare data for GDX: select relevant columns and ensure correct types.
    # GAMS Transfer automatically handles types for sets and parameters.
    # Make sure Year is treated as a string/factor if it's a dimension.
    df_for_transfer <- df %>%
      select(Area, Item, Element, Year, Value) %>%
      mutate(Year = as.character(Year)) # GAMS sets are often strings


    # Define the GAMS parameter name using the FAOSTAT code
    gams_param_name <- paste0("p_", fao_code) # e.g., p_QC

    # Create a new Parameter object in the GAMS container
    # The domains are inferred from the column names of df_for_transfer
    # excluding the last column (Value)
    p <- m$addParameter(gams_param_name,
                        domain = c("Area", "Item", "Element", "Year"), # Explicitly define domains
                        description = paste("FAOSTAT Data for", fao_code))

    # Add the data records to the parameter
    p$setRecords(df_for_transfer)

    message(paste("Successfully added data for:", code, "to GAMS container."))
  }, error = function(e) {
    warning(paste("Failed to process file '", file_basename, "': ", e$message, sep = ""))
  })
}

# Write all defined parameters in the container to a single GDX file
if (length(m$parameters) > 0) { # Check if any parameters were added
  message("\nAttempting to write data to GDX file using gamstransfer...")
  tryCatch({
    m$write(gdx_output_file)
    message(paste("Successfully created GDX file:", gdx_output_file))
  }, error = function(e) {
    warning(paste("Failed to write GDX file '", gdx_output_file, "': ", e$message, sep = ""))
    message("Please ensure GAMS is installed on your system and its system directory is accessible.")
  })
} else {
  message("No valid datasets were successfully prepared for GDX conversion.")
}

message("GDX conversion script finished.")