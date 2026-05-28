# ============================================================
# Reproduce OpenRefine cleaning steps in R
# Based on operations in: lhprism_openrefine_cleaning.json
# NOTE: This script was written by Co-Pilot and was not 
# verified to repeat the openrefine steps. 
# Please review carefully before running.
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(readr)
  library(tidyr)
})

# ---- File paths (edit these) ----
input_file  <- "raw_data.csv"     # <-- your raw export (CSV)
output_file <- "clean_data.csv"   # <-- cleaned output (CSV)

# ---- Helper functions ----

# Clean numeric fields that may include commas, units, "- 0" artifacts, etc.
clean_numeric <- function(x, na_strings = c("", "NA", "N/A", "n/a")) {
  x <- as.character(x)
  x <- str_trim(x)
  x[x %in% na_strings] <- NA_character_
  # remove commas
  x <- str_replace_all(x, ",", "")
  # remove obvious unit tokens (safe even if absent)
  x <- str_replace_all(x, regex("\\bsq\\s*ft\\b|\\bsqft\\b", ignore_case = TRUE), "")
  x <- str_trim(x)
  suppressWarnings(as.numeric(x))
}

# More specific cleaner used where OpenRefine removed "- 0"
clean_numeric_remove_dash0 <- function(x, replacement = "") {
  x <- as.character(x)
  x <- str_replace_all(x, "-\\s*0", replacement)  # "- 0" -> "" or "NA"
  clean_numeric(x)
}

# Handle FO_PERCENT-like mixed strings that embed a bin in parentheses,
# or are raw numbers that should be collapsed to bins.
normalize_percent_bin <- function(x) {
  x <- as.character(x)
  x <- str_trim(x)
  x[x %in% c("", "NA", "N/A", "n/a")] <- NA_character_
  
  # Standardize obvious percent signs
  x <- str_replace_all(x, "%", "")
  
  # If parentheses contain a bin like "(76-100)" or "(51-75)", extract it
  has_bin <- str_match(x, "\\((\\s*<?\\d+\\s*-\\s*\\d+\\s*|\\s*<?\\d+\\s*)\\)")[,2]
  out <- ifelse(!is.na(has_bin), str_replace_all(has_bin, "\\s+", ""), x)
  
  # Remove any leftover embedded notes like "sq ft" etc
  out <- str_replace_all(out, regex("sq\\s*ft.*$", ignore_case = TRUE), "")
  out <- str_trim(out)
  
  # Normalize some common textual bins (remove whitespace)
  out <- str_replace_all(out, "\\s+", "")
  out <- str_replace_all(out, "^<5$", "<10")  # in your FO_PERCENT edits, small values became <10
  
  # If it's a pure number, bin it (mirroring your recodes)
  num <- suppressWarnings(as.numeric(out))
  out2 <- case_when(
    is.na(out) ~ NA_character_,
    !is.na(num) & num == 100 ~ "100",
    !is.na(num) & num >= 76 ~ "76-100",
    !is.na(num) & num >= 51 ~ "51-75",
    !is.na(num) & num >= 26 ~ "26-50",
    !is.na(num) & num >= 10 ~ "10-25",
    !is.na(num) & num <  10 ~ "<10",
    TRUE ~ out
  )
  
  # Ensure consistent display for bins that may still include "%"
  out2 <- str_replace_all(out2, "%", "")
  out2
}

# Safe mutate: only apply if a column exists
mutate_if_present <- function(df, col, fn) {
  if (col %in% names(df)) {
    df %>% mutate("{col}" := fn(.data[[col]]))
  } else {
    df
  }
}

# ---- Read data ----
df <- read_csv(
  input_file,
  na = c("", "NA", "N/A", "n/a"),
  show_col_types = FALSE
)

# ---- Drop extraneous columns ("Column", "Column2") ----
df <- df %>%
  select(-any_of(c("Column", "Column2")))

# ============================================================
# 1) VISIT_STATUS: harmonize labels then split into NUM + CHAR
# ============================================================

if ("VISIT_STATUS" %in% names(df)) {
  
  df <- df %>%
    mutate(VISIT_STATUS = as.character(VISIT_STATUS)) %>%
    mutate(VISIT_STATUS = str_trim(VISIT_STATUS)) %>%
    mutate(
      VISIT_STATUS = case_when(
        VISIT_STATUS %in% c("No plants 2nd year", "No Plants 2nd year",
                            "No plants 2nd Year", "No plants 2nd year ",
                            "No plants 2nd year - 4", "4- No plants 2nd year",
                            "No Plants 2 Yrs", "No plants 2nd year-4") ~ "4 - no plants 2nd year",
        
        VISIT_STATUS %in% c("No Plants 3rd year", "No plants 3rd year",
                            "No Plants 3rd Year", "No plants 3rd year-5",
                            "no plants 3rd year-5") ~ "5 - no plants 3rd year",
        
        VISIT_STATUS %in% c("Completely Treated-2", "2- Completely Treated") ~ "2 - completely treated",
        
        VISIT_STATUS %in% c("No plants 4th year", "No plants 4th year-5") ~ "5 - no plants 4th year", # see OpenRefine edits
        
        VISIT_STATUS %in% c("0- Untreated", "Untreated-0") ~ "0 - untreated",
        
        VISIT_STATUS %in% c("3- No Plants 1 Yr", "No plants 1st year - 3") ~ "3 - no plants 1st year",
        
        VISIT_STATUS %in% c("3- no plants 1st year") ~ "3 - no plants 1st year",
        
        VISIT_STATUS %in% c("5- Eradicated", "Eradicated-5") ~ "5 - eradicated",
        
        VISIT_STATUS %in% c("Partially Treated-1") ~ "1 - partially treated",
        
        TRUE ~ VISIT_STATUS
      )
    ) %>%
    # Split "num - label" into two new columns; remove original
    separate(
      VISIT_STATUS,
      into = c("VISIT_STATUS_NUM", "VISIT_STATUS_CHAR"),
      sep = "\\s-\\s",
      remove = TRUE,
      extra = "merge",
      fill  = "right"
    ) %>%
    mutate(
      VISIT_STATUS_NUM  = suppressWarnings(as.numeric(VISIT_STATUS_NUM)),
      VISIT_STATUS_CHAR = str_trim(VISIT_STATUS_CHAR)
    )
}

# ============================================================
# 2) SEARCH_SQFT: remove commas, remove "- 0", convert to numeric
# ============================================================
df <- mutate_if_present(df, "SEARCH_SQFT", function(x) clean_numeric_remove_dash0(x, replacement = ""))

# ============================================================
# 3) PERCENT_COVER: standardize bins and NA codes
# ============================================================
if ("PERCENT_COVER" %in% names(df)) {
  df <- df %>%
    mutate(PERCENT_COVER = as.character(PERCENT_COVER)) %>%
    mutate(PERCENT_COVER = str_trim(PERCENT_COVER)) %>%
    mutate(
      PERCENT_COVER = case_when(
        PERCENT_COVER %in% c("76-100", "76-100%") ~ "76-100",
        PERCENT_COVER %in% c("51-75",  "51-75%")  ~ "51-75",
        PERCENT_COVER %in% c("26-50",  "26-50%")  ~ "26-50",
        PERCENT_COVER %in% c("5-25%", "5-25", "6-25", "6-25%") ~ "5-25",
        PERCENT_COVER %in% c("<5", "<5%") ~ "<5",
        PERCENT_COVER %in% c("25-May") ~ "5-25",
        PERCENT_COVER %in% c("N/A") ~ "NA",
        PERCENT_COVER %in% c("0", "4") ~ "<5",
        PERCENT_COVER %in% c("50") ~ "26-50",
        PERCENT_COVER %in% c("100") ~ "76-100",
        TRUE ~ PERCENT_COVER
      )
    ) %>%
    mutate(PERCENT_COVER = na_if(PERCENT_COVER, "NA"))
}

# ============================================================
# 4) GROSS_SQFT: remove 'sqft', trim, convert to numeric
# ============================================================
if ("GROSS_SQFT" %in% names(df)) {
  df <- df %>%
    mutate(GROSS_SQFT = as.character(GROSS_SQFT)) %>%
    mutate(GROSS_SQFT = str_replace_all(GROSS_SQFT, regex("sqft", ignore_case = TRUE), "")) %>%
    mutate(GROSS_SQFT = str_trim(GROSS_SQFT)) %>%
    mutate(GROSS_SQFT = clean_numeric(GROSS_SQFT))
}

# ============================================================
# 5) PLANT_COUNT: remove commas, remove "- 0", one-off fixes, numeric
# ============================================================
if ("PLANT_COUNT" %in% names(df)) {
  df <- df %>%
    mutate(PLANT_COUNT = as.character(PLANT_COUNT)) %>%
    mutate(PLANT_COUNT = str_trim(PLANT_COUNT)) %>%
    mutate(
      PLANT_COUNT = case_when(
        PLANT_COUNT %in% c(".498.8sqft") ~ "498.8",
        PLANT_COUNT %in% c("1469sqft") ~ "1469",
        PLANT_COUNT %in% c("17616sqft") ~ "17616",
        PLANT_COUNT %in% c("251+220") ~ as.character(251 + 220),
        PLANT_COUNT %in% c("48212.8 sq ft") ~ "48212.8",
        PLANT_COUNT %in% c("70561.2 sq ft") ~ "70561.2",
        PLANT_COUNT %in% c("n/a") ~ "NA",
        TRUE ~ PLANT_COUNT
      )
    ) %>%
    mutate(PLANT_COUNT = str_replace_all(PLANT_COUNT, ",", "")) %>%
    mutate(PLANT_COUNT = str_replace_all(PLANT_COUNT, "-\\s*0", "")) %>%
    mutate(PLANT_COUNT = str_replace_all(PLANT_COUNT, regex("sq\\s*ft|sqft", ignore_case = TRUE), "")) %>%
    mutate(PLANT_COUNT = str_trim(PLANT_COUNT)) %>%
    mutate(PLANT_COUNT = clean_numeric(PLANT_COUNT))
}

# ============================================================
# 6) AGE_SIZE: lowercase + harmonize categories + NA
# ============================================================
if ("AGE_SIZE" %in% names(df)) {
  df <- df %>%
    mutate(AGE_SIZE = as.character(AGE_SIZE)) %>%
    mutate(AGE_SIZE = str_trim(tolower(AGE_SIZE))) %>%
    mutate(
      AGE_SIZE = case_when(
        AGE_SIZE %in% c("seedlings, saplings, adults", "adults, saplings, seedlings",
                        "seedling, sapling, adults") ~ "seedlings, saplings, adults",
        
        AGE_SIZE %in% c("adults, saplings", "saplings, adults") ~ "saplings, adults",
        
        AGE_SIZE %in% c("sapling", "saplings") ~ "sapling",
        AGE_SIZE %in% c("seedling", "seedlings") ~ "seedling",
        AGE_SIZE %in% c("adults") ~ "adult",
        AGE_SIZE %in% c("n/a") ~ "NA",
        TRUE ~ AGE_SIZE
      )
    ) %>%
    mutate(AGE_SIZE = na_if(AGE_SIZE, "NA"))
}

# ============================================================
# 7) DISTRIBUTION / PHENOLOGY: lowercase + trim + NA
# ============================================================
df <- mutate_if_present(df, "DISTRIBUTION", function(x) {
  x <- str_trim(tolower(as.character(x)))
  x <- ifelse(x %in% c("n/a", "na", "n/a "), "NA", x)
  na_if(x, "NA")
})

df <- mutate_if_present(df, "PHENOLOGY", function(x) {
  x <- str_trim(tolower(as.character(x)))
  x <- ifelse(x %in% c("n/a", "na"), "NA", x)
  na_if(x, "NA")
})

# ============================================================
# 8) NATIVE / NON_NAT: lowercase + recode "med" -> "medium"
# ============================================================
df <- mutate_if_present(df, "NATIVE", function(x) {
  x <- str_trim(tolower(as.character(x)))
  x <- ifelse(x == "med", "medium", x)
  x
})

df <- mutate_if_present(df, "NON_NAT", function(x) {
  x <- str_trim(tolower(as.character(x)))
  x <- ifelse(x == "med", "medium", x)
  x
})

# ============================================================
# 9) PERCENT_TREATED: standardize bins + NA
# ============================================================
if ("PERCENT_TREATED" %in% names(df)) {
  df <- df %>%
    mutate(PERCENT_TREATED = as.character(PERCENT_TREATED)) %>%
    mutate(PERCENT_TREATED = str_trim(PERCENT_TREATED)) %>%
    mutate(
      PERCENT_TREATED = case_when(
        PERCENT_TREATED %in% c("76-100", "76-100%") ~ "76-100",
        PERCENT_TREATED %in% c("10-25", "10-25%") ~ "10-25",
        PERCENT_TREATED %in% c("26-50", "26-50%") ~ "26-50",
        PERCENT_TREATED %in% c("N/A") ~ "NA",
        PERCENT_TREATED %in% c("76", "100") ~ "76-100",
        PERCENT_TREATED %in% c("<5%") ~ "<10",
        PERCENT_TREATED %in% c("11-25", "5-25") ~ "10-25",
        TRUE ~ PERCENT_TREATED
      )
    ) %>%
    mutate(PERCENT_TREATED = na_if(PERCENT_TREATED, "NA"))
}

# ============================================================
# 10) GROSS_TREAT_SQFT: remove commas, "- 0" -> NA, numeric
# ============================================================
df <- mutate_if_present(df, "GROSS_TREAT_SQFT", function(x) clean_numeric_remove_dash0(x, replacement = "NA"))

# ============================================================
# 11) PU_COUNT: "n/a" -> NA
# ============================================================
df <- mutate_if_present(df, "PU_COUNT", function(x) {
  x <- str_trim(tolower(as.character(x)))
  x <- ifelse(x == "n/a", "NA", x)
  na_if(x, "NA")
})

# ============================================================
# 12) FO_PERCENT: collapse messy entries into bins (and some one-offs)
# ============================================================
if ("FO_PERCENT" %in% names(df)) {
  df <- df %>%
    mutate(FO_PERCENT = as.character(FO_PERCENT)) %>%
    mutate(FO_PERCENT = str_trim(FO_PERCENT)) %>%
    mutate(
      FO_PERCENT = case_when(
        FO_PERCENT %in% c("1", "4", "5") ~ "<10",
        FO_PERCENT %in% c("10-25%") ~ "10-25",
        FO_PERCENT %in% c("15", "20") ~ "10-25",
        FO_PERCENT %in% c("30", "35", "50") ~ "26-50",
        FO_PERCENT %in% c("100% (1000sq ft)", "2251.22 sq ft (100%)") ~ "100",
        TRUE ~ FO_PERCENT
      ),
      FO_PERCENT = normalize_percent_bin(FO_PERCENT)
    )
}

# ============================================================
# 13) CO_PERCENT: "76-100" (or %) -> "88" (as in OpenRefine)
# ============================================================
if ("CO_PERCENT" %in% names(df)) {
  df <- df %>%
    mutate(CO_PERCENT = as.character(CO_PERCENT)) %>%
    mutate(CO_PERCENT = str_trim(CO_PERCENT)) %>%
    mutate(
      CO_PERCENT = case_when(
        CO_PERCENT %in% c("76-100", "76-100%") ~ "88",
        TRUE ~ CO_PERCENT
      ),
      CO_PERCENT = clean_numeric(CO_PERCENT)
    )
}

# ============================================================
# 14) CS_COUNT: remove values that look like sqft artifacts
# ============================================================
if ("CS_COUNT" %in% names(df)) {
  df <- df %>%
    mutate(CS_COUNT = as.character(CS_COUNT)) %>%
    mutate(CS_COUNT = case_when(
      CS_COUNT %in% c("1469sqft", "5127.8sqft") ~ NA_character_,
      TRUE ~ CS_COUNT
    ))
}

# ============================================================
# 15) HERBICIDE_NAME / AI_CONCENTRATION / AMT_OF_PRODUCT: N/A -> NA; 0 -> NA for herbicide
# ============================================================
df <- mutate_if_present(df, "HERBICIDE_NAME", function(x) {
  x <- str_trim(as.character(x))
  x <- case_when(
    x == "N/A" ~ "NA",
    x == "0" ~ NA_character_,
    TRUE ~ x
  )
  na_if(x, "NA")
})

df <- mutate_if_present(df, "AI_CONCENTRATION", function(x) {
  x <- str_trim(as.character(x))
  x <- ifelse(x == "N/A", "NA", x)
  na_if(x, "NA")
})

df <- mutate_if_present(df, "AMT_OF_PRODUCT", function(x) {
  x <- str_trim(as.character(x))
  x <- ifelse(x == "N/A", "NA", x)
  na_if(x, "NA")
})

# ============================================================
# 16) MIX_UNITS / PRODUCT_UNITS: standardize unit strings + NA
# ============================================================
standardize_units <- function(x) {
  x <- str_trim(as.character(x))
  x <- case_when(
    x %in% c("N/A") ~ "NA",
    x %in% c("gal", "Gal", "GL") ~ "gal",
    x %in% c("FL", "FL ", "Fl", "fl") ~ "FL",
    x %in% c("kg", "KG") ~ "kg",
    x %in% c("ML", "mL") ~ "mL",
    x %in% c("OZ") ~ "oz",
    TRUE ~ x
  )
  na_if(x, "NA")
}

df <- mutate_if_present(df, "MIX_UNITS", standardize_units)
df <- mutate_if_present(df, "PRODUCT_UNITS", standardize_units)

# ============================================================
# 17) OBSERVER: trim
# ============================================================
df <- mutate_if_present(df, "OBSERVER", function(x) str_trim(as.character(x)))

# ============================================================
# 18) ORGANIZATION: harmonize organization names
# ============================================================
if ("ORGANIZATION" %in% names(df)) {
  df <- df %>%
    mutate(ORGANIZATION = as.character(ORGANIZATION)) %>%
    mutate(ORGANIZATION = str_trim(ORGANIZATION)) %>%
    mutate(
      ORGANIZATION = case_when(
        ORGANIZATION %in% c("Trillium Invasives Species Management Inc",
                            "Trillium Invasives Species Management Inc.") ~
          "Trillium Invasives Species Management Inc",
        ORGANIZATION %in% c("Lower Hudson PRISM (Blockbuster)",
                            "Lower Hudson PRISM (BlockBuster)") ~
          "Lower Hudson PRISM (Blockbuster)",
        ORGANIZATION %in% c("Trillium Invasives Species Management Inc") ~
          "Trillium Invasive Species Management Inc.",
        ORGANIZATION %in% c("Trillum Invasive Species Management Inc.") ~
          "Trillium Invasive Species Management Inc.",
        ORGANIZATION %in% c("NYSDEC") ~ "NYS DEC",
        ORGANIZATION %in% c("New York-New Jersey Trail Conference") ~
          "New York - New Jersey Trail Conference",
        TRUE ~ ORGANIZATION
      )
    )
}

# ============================================================
# 19) WIND_SPEED: "2 mph" -> "2"
# ============================================================
if ("WIND_SPEED" %in% names(df)) {
  df <- df %>%
    mutate(WIND_SPEED = as.character(WIND_SPEED)) %>%
    mutate(WIND_SPEED = str_trim(WIND_SPEED)) %>%
    mutate(WIND_SPEED = case_when(
      WIND_SPEED == "2 mph" ~ "2",
      TRUE ~ WIND_SPEED
    )) %>%
    mutate(WIND_SPEED = clean_numeric(WIND_SPEED))
}

# ---- Write cleaned data ----
write_csv(df, output_file, na = "")

message("Wrote cleaned dataset to: ", output_file)
``