library(gamstransfer)
library(jsonlite)
library(dplyr)

# Configuration
input_folder <- "inputs"
output_gdx_path <- "outputs/faostat_data.gdx"
config_file <- file.path(input_folder, "mapping_conf.json")

# --- Load and validate config ---
if (!file.exists(config_file)) stop("Config file not found: ", config_file)
config <- fromJSON(config_file, simplifyVector = FALSE)

# Flatten keys
config$dimensions <- unlist(config$dimensions, use.names = FALSE)
config$values <- unlist(config$values, use.names = FALSE)

# --- Manual config validation ---
if (length(config$dimensions) == 0) stop("Invalid config: 'dimensions' must be a non-empty array.")
if (length(config$values) == 0) stop("Invalid config: 'values' must be a non-empty array.")
if (is.null(config$datasets) || !is.list(config$datasets)) stop("Invalid config: 'datasets' must be an object.")

for (code in names(config$datasets)) {
  if (is.null(config$datasets[[code]]$mapping)) {
    stop("Missing 'mapping' for FAO code: ", code)
  }
}

domain_dims <- config$dimensions
value_cols <- config$values
m <- Container$new()

# --- Process CSV files ---
csv_files <- list.files(input_folder, pattern = "_data\\.csv$", full.names = TRUE)
if (length(csv_files) == 0) stop("No *_data.csv files found in input folder.")

for (file_path in csv_files) {
  file_basename <- basename(file_path)
  fao_code <- sub("_data\\.csv$", "", file_basename)

  message("\n📄 Processing FAO code: ", fao_code, " from file: ", file_basename)

  if (!fao_code %in% names(config$datasets)) {
    message("⚠️ No mapping for FAO code: ", fao_code, ". Skipping.")
    next
  }

  df <- tryCatch(read.csv(file_path, stringsAsFactors = FALSE), error = function(e) {
    message("❌ Failed to read: ", file_basename)
    return(NULL)
  })
  if (is.null(df)) next

  cat("🧾 Columns in source file:\n")
  cat(paste0(" - ", names(df), collapse = "\n"), "\n\n")

  mapping_cfg <- config$datasets[[fao_code]]$mapping
  result_df <- data.frame(row_id = seq_len(nrow(df)))

  for (target_col in names(mapping_cfg)) {
    rule <- mapping_cfg[[target_col]]
    if (is.character(rule)) {
      source_col <- rule
      if (!source_col %in% names(df)) {
        message("⚠️ Column '", source_col, "' not found in ", file_basename)
        next
      }
      result_df[[target_col]] <- df[[source_col]]

    } else if (is.list(rule) && !is.null(rule$from) && !is.null(rule$file)) {
      source_col <- rule$from
      mapping_csv <- file.path(input_folder, rule$file)

      if (!source_col %in% names(df)) {
        message("⚠️ Source column '", source_col, "' not found in file ", file_basename)
        next
      }
      if (!file.exists(mapping_csv)) {
        message("⚠️ Mapping file missing: ", mapping_csv)
        next
      }

      map_df <- read.csv(mapping_csv, stringsAsFactors = FALSE)
      if (!(source_col %in% names(map_df)) || !(target_col %in% names(map_df))) {
        message("⚠️ Mapping file '", mapping_csv, "' must contain columns: ", source_col, " and ", target_col)
        next
      }

      mapped <- left_join(df[, source_col, drop = FALSE], map_df, by = setNames(source_col, source_col))
      fallback <- paste0(target_col, "_", df[[source_col]])
      mapped[[target_col]][is.na(mapped[[target_col]])] <- fallback[is.na(mapped[[target_col]])]
      result_df[[target_col]] <- mapped[[target_col]]

    } else {
      message("⚠️ Invalid mapping for target column '", target_col, "' in ", fao_code)
      next
    }
  }

  expected_cols <- c(domain_dims, value_cols)
  missing_cols <- setdiff(expected_cols, names(result_df))
  if (length(missing_cols) > 0) {
    message("⚠️ Missing mapped columns for ", fao_code, ": ", paste(missing_cols, collapse = ", "))
    next
  }

  # Coerce domain dims to character
  for (d in domain_dims) {
    result_df[[d]] <- as.character(result_df[[d]])
  }

  any_value_added <- FALSE

  for (val_col in value_cols) {
    value_vector <- suppressWarnings(as.numeric(result_df[[val_col]]))

    if (all(is.na(value_vector))) {
      message("⚠️ Column '", val_col, "' is not numeric or has no data in ", fao_code, ". Skipping.")
      next
    }

    final_df <- result_df[, c(domain_dims, val_col)]
    names(final_df)[1:length(domain_dims)] <- domain_dims  # Ensure exact domain names
    final_df[[val_col]] <- value_vector  # Ensure numeric

    # Confirm structure
    actual_domains <- names(final_df)[1:length(domain_dims)]
    if (!identical(actual_domains, domain_dims)) {
      stop("🛑 Domain mismatch: expected ", paste(domain_dims, collapse = ", "),
          " but got ", paste(actual_domains, collapse = ", "))
    }
    param_name <- paste0("p_", tolower(fao_code), "_", tolower(val_col))
    p <- m$addParameter(param_name, domain = domain_dims, description = paste(val_col, "for", fao_code))
    p$setRecords(final_df)

    message("🔢 Records in parameter ", param_name, ": ", nrow(final_df))

    message("✅ Added parameter: ", param_name)
    any_value_added <- TRUE
  }

  if (!any_value_added) {
    message("⚠️ No valid numeric values found for ", fao_code)
  } else {
    message("✅ Finished processing FAO code: ", fao_code)
  }
}

m$write(output_gdx_path)
message("\n🎉 GDX written to: ", output_gdx_path)