# 📄 FAOSTAT to GDX Conversion Script

This script automates the conversion of FAOSTAT datasets into GAMS GDX format using user-defined dimension mappings and optional category derivations.

---

## 🧭 Purpose

The script processes FAOSTAT CSV data files and converts them into a structured `.gdx` file suitable for use in GAMS models. It supports flexible mapping of source columns to final dimension names, and can derive categories from auxiliary mapping CSVs.

---

## 📂 Expected Input Structure

All input files are placed under the `inputs/` directory:

```
inputs/
├── QI_data.csv
├── QC_data.csv
├── mapping_conf.json
└── category_mappings/
    ├── product_category_map.csv
    └── element_category_map.csv
```

### 1. FAOSTAT CSV Files

- Located in: `inputs/`
- Naming convention: `<FAO_CODE>_data.csv`
  - Example: `QI_data.csv`, `QC_data.csv`

These should be standard FAOSTAT downloads, containing columns like:

```
area, item, year, element, value, etc.
```

---

## ⚙️ Configuration File (`mapping_conf.json`)

Located in: `inputs/mapping_conf.json`

### 📌 Example Structure

```json
{
  "dimensions": ["Region", "ProductCategory", "Product", "Metric", "Year"],
  "values": ["Value", "Imports"],

  "datasets": {
    "QI": {
      "mapping": {
        "Region": "area",
        "Product": "item_code",
        "ProductCategory": {
          "from": "item_code",
          "file": "category_mappings/product_category_map.csv"
        },
        "Metric": "element",
        "Year": "year",
        "Value": "value",
        "Imports": "import_value"
      }
    }
  },

  "comment": "Final dimensions on the left; source or derivation on the right"
}
```

### 🧩 Key Sections

| Key | Description |
|-----|-------------|
| `dimensions` | List of domain dimensions (strings) in desired output order |
| `values` | List of numeric columns to be exported as GDX parameters |
| `datasets` | A map of FAO codes to their respective column mappings |

---

## 🔄 Mapping Rules

Each `mapping` block can define:

### ✅ Direct Mapping

```json
"Region": "area"
```

Means `"Region"` in output will come directly from `area` column in CSV.

### 🔁 Derived Mapping (via CSV)

```json
"ProductCategory": {
  "from": "item_code",
  "file": "category_mappings/product_category_map.csv"
}
```

This tells the script to derive `"ProductCategory"` from `item_code`, using a lookup CSV with the following structure:

---

## 📊 Mapping CSV Format

Each mapping CSV should contain at least two columns:

| source_column | target_dimension |
|---------------|------------------|
| wheat         | Cereals          |
| maize         | Cereals          |
| cattle        | Livestock        |

- The source column name must match `from` in the mapping rule
- The target column name must match the final dimension name

Example: `product_category_map.csv` must contain `item_code` and `ProductCategory`

---

## ▶️ Execution

Run the script using:

```sh
Rscript main.R
```

This will:

- Load all available FAO datasets (`*_data.csv`)
- Apply the configured dimension and value mappings
- Generate the output file:

```
outputs/faostat_data.gdx
```

---

## 📦 Output

- One `.gdx` file with one parameter per value column **per FAO dataset**
- Parameter naming convention: `p_<faocode>_<valuecolumn>`, e.g. `p_qi_value`, `p_qi_imports`

If you specify multiple value columns in the config, **a separate GDX parameter will be created for each one**, using the same dimensions.

---

## ⚠️ Notes

- Column names in the config and CSVs are **case-sensitive**
- Each value column must be numeric
- A mapping is required for every dimension and value in each dataset
- If any mapped column is missing or invalid, that dataset will be skipped

---