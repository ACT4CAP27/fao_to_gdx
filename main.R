# --- CONFIGURATION ---
input_folder <- "inputs"
output_gdx_path <- "outputs/faostat_data.gdx"
config_file <- file.path(input_folder, "mapping_conf.json")

# --- JSON SCHEMA (embedded inline) ---
config_schema <- '{
  "type": "object",
  "required": ["dimensions", "datasets"],
  "properties": {
    "dimensions": {
      "type": "array",
      "items": { "type": "string" },
      "minItems": 1
    },
    "datasets": {
      "type": "object",
      "patternProperties": {
        "^[A-Z0-9]+$": {
          "type": "object",
          "required": ["mapping"],
          "properties": {
            "mapping": {
              "type": "object",
              "minProperties": 1,
              "additionalProperties": {
                "oneOf": [
                  { "type": "string" },
                  {
                    "type": "object",
                    "required": ["from", "file"],
                    "properties": {
                      "from": { "type": "string" },
                      "file": { "type": "string" }
                    },
                    "additionalProperties": false
                  }
                ]
              }
            }
          },
          "additionalProperties": false
        }
      }
    },
    "comment": { "type": "string" }
  },
  "additionalProperties": false
}'

# --- Load and validate config ---
if (!file.exists(config_file)) stop("Config file not found: ", config_file)
config <- fromJSON(config_file, simplifyVector = FALSE)

tmp_schema <- tempfile(fileext = ".json")
writeLines(config_schema, tmp_schema)
validator <- json_validator(tmp_schema, engine = "ajv", schema = "draft7")
if (!validator(config_file)) stop("Config file does not conform to schema.")

dimensions <- config$dimensions
datasets <- config$datasets

# --- Begin processing ---
csv_files <- list.files(input_folder, pattern = "_data\\.csv$", full.names = TRUE)
if (length(csv_files) == 0) stop("No *_data.csv files found in input folder.")

m <- Container$new()

for (file_path in csv_files) {
  file_basename <- basename(file_path)
  fao_code <- sub("_data\\.csv$", "", file_basename)

  message("\nProcessing FAO code: ", fao_code, " from file: ", file_basename)

  if (!fao_code %in% names(datasets)) {
    warning("No config entry for FAO code: ", fao_code, ". Skipping.")
    next
  }

  df <- tryCatch(read.csv(file_path, stringsAsFactors = FALSE), error = function(e) {
    warning("Failed to read CSV file: ", file_basename)
    return(NULL)
  })
  if (is.null(df)) next

  mapping_cfg <- datasets[[fao_code]]$mapping
  result_df <- data.frame()

  for (target_dim in names(mapping_cfg)) {
    mapping_rule <- mapping_cfg[[target_dim]]

    if (is.character(mapping_rule)) {
      # Direct rename
      source_col <- mapping_rule
      if (!source_col %in% names(df)) {
        warning("Column '", source_col, "' not found in ", file_basename)
        next
      }
      result_df[[target_dim]] <- df[[source_col]]
    } else if (is.list(mapping_rule) && !is.null(mapping_rule$from) && !is.null(mapping_rule$file)) {
      source_col <- mapping_rule$from
      mapping_csv <- file.path(input_folder, mapping_rule$file)

      if (!source_col %in% names(df)) {
        warning("Source column '", source_col, "' not found in file ", file_basename)
        next
      }
      if (!file.exists(mapping_csv)) {
        warning("Mapping file not found: ", mapping_csv)
        next
      }

      map_df <- read.csv(mapping_csv, stringsAsFactors = FALSE)
      if (!(source_col %in% names(map_df)) || !(target_dim %in% names(map_df))) {
        warning("Mapping file ", mapping_csv, " must contain columns: ", source_col, " and ", target_dim)
        next
      }

      # Join mapping
      mapped <- left_join(df[, source_col, drop = FALSE], map_df, by = setNames(source_col, source_col))
      # Apply fallback
      mapped[[target_dim]][is.na(mapped[[target_dim]])] <- paste0(target_dim, "_", df[[source_col]][is.na(mapped[[target_dim]])])
      result_df[[target_dim]] <- mapped[[target_dim]]
    } else {
      warning("Invalid mapping format for '", target_dim, "' in FAO code: ", fao_code)
      next
    }
  }

  # Check for Value column
  if (!"Value" %in% names(result_df)) {
    warning("No 'Value' column found for ", fao_code, ". Skipping.")
    next
  }

  # Ensure all dimensions exist
  missing_dims <- setdiff(dimensions, names(result_df))
  if (length(missing_dims) > 0) {
    warning("Missing dimensions in ", fao_code, ": ", paste(missing_dims, collapse = ", "))
    next
  }

  # Order columns: dimensions + Value
  final_df <- result_df[, c(dimensions, "Value")]

  # Add parameter to GAMS container
  param_name <- paste0("p_", tolower(fao_code))
  p <- m$addParameter(param_name,
                      domain = dimensions,
                      description = paste("FAOSTAT data for", fao_code))
  p$setRecords(final_df)

  message("✔ Processed FAO code: ", fao_code)
}

# --- Write GDX ---
if (length(m$parameters) > 0) {
  tryCatch({
    m$write(output_gdx_path)
    message("\n✅ Successfully wrote GDX file to: ", output_gdx_path)
  }, error = function(e) {
    warning("❌ Failed to write GDX: ", e$message)
  })
} else {
  message("No parameters were added. No GDX file created.")
}
