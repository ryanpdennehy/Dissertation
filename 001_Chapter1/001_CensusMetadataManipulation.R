################################################################################
################################################################################
################################################################################
### RYAN P. DENNEHY - PhD DISSERTATION - REPLICATION CODE
### CHAPTER 1 - TOWARD AN AMERICAN SOCIOECONOMIC INDEX FOR USE IN POLITICAL...
### LAST UPDATED: APR. 1, 2025
### 001 - CENSUS METADATA MANIPULATION
### ======================================================================== ###
### Purpose: Read metadata from all raw Census tract files, extract and 
###          combine header rows, and build a master data frame tracking the 
###          yearly evolution of variable codes and descriptions across ACS 
###          releases (2010–2023). Output includes full raw headers and a 
###          cleaned evaluation table with presence, change, and continuity 
###          information for each variable by year.
################################################################################
################################################################################
################################################################################

# ---------------------------------------------------------------------------- #
# Load Libraries                                                               #
# ---------------------------------------------------------------------------- #
library(pacman)
pacman::p_load(
  dplyr,
  readr,
  stringr,
  tidyr
)

# ---------------------------------------------------------------------------- #
# Create Output Directory for Data Headers Evaluation CSV Files                #
# ---------------------------------------------------------------------------- #
output_folder <- file.path("001_Chapter1", "101_MetadataEvaluation")
if (!dir.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)
}

# ---------------------------------------------------------------------------- #
# Define File Paths and List Files for Data Files                              #
# ---------------------------------------------------------------------------- #
base_dir   <- "000_Data/001_CensusDataTracts"
subfolders <- c("DP02", "DP03", "DP04", "DP05")

file_paths <- lapply(
  subfolders,
  function(sf) {
    list.files(
      path       = file.path(base_dir, sf),
      pattern    = "-Data\\.csv$",
      full.names = TRUE
    )
  }
) %>% unlist()

# ---------------------------------------------------------------------------- #
# Read and Combine Data Headers from the Data Files                            #
# ---------------------------------------------------------------------------- #
# For each file, read the first two rows (ignoring the first two columns),
# transpose them so that the first row becomes the variable code and the second 
# row becomes the description.
read_data_header <- function(file_path) {
  subfolder <- basename(dirname(file_path))
  year      <- as.integer(str_match(basename(file_path), "ACSDP5Y(\\d{4})")[[2]])
  
  data <- read_csv(
    file_path,
    n_max          = 2,
    col_names      = FALSE,
    show_col_types = FALSE
  )
  
  # Drop the first two columns
  data <- data[ , -(1:2)]
  
  # Transpose so that each column becomes a row
  transposed <- as_tibble(t(data), .name_repair = "minimal")
  # Rename the first two columns as "name" and "info"
  names(transposed)[1:2] <- c("name", "info")
  
  # Append subfolder and year information
  transposed <- transposed %>% mutate(
    subfolder = subfolder,
    year      = year
  )
  return(transposed)
}

headers_all <- lapply(file_paths, read_data_header) %>% bind_rows()

# Save the raw combined headers (for reference)
write_csv(
  headers_all,
  file.path(output_folder, "000_full_data_headers.csv")
)

# ---------------------------------------------------------------------------- #
# Build the Master Data Frame for Variable Evolution                           #
# ---------------------------------------------------------------------------- #
# Rename "info" to "description" and select needed columns.
df <- headers_all %>%
  select(subfolder, year, name, info) %>%
  rename(description = info)

# Remove duplicate rows so that each subfolder/description/year appears only 
# once
df_unique <- df %>% distinct(subfolder, description, year, .keep_all = TRUE)

# Pivot wider so that each row is a unique subfolder/description and for each 
# year (2010-2023)
# we have a column "var_YYYY" containing the variable code (name).
years_range <- 2010:2023
wide <- df_unique %>%
  select(subfolder, description, year, name) %>%
  pivot_wider(
    names_from = year,
    values_from = name,
    names_prefix = "var_"
  )

# ---------------------------------------------------------------------------- #
# Compute Match Columns for Each Year                                          #
# ---------------------------------------------------------------------------- #
# For each year, create a match column.
# For each row (i.e. description):
#   - If the description appears for the first time in that row, mark match as 
#     99.
#   - Otherwise, if the previous calendar year's value exists, mark 1 if 
#     unchanged, 0 if changed.
#   - If the previous year is missing, mark as NA.
var_cols <- paste0("var_", years_range)
for (yr in years_range) {
  var_current <- paste0("var_", yr)
  match_col   <- paste0("match_", yr)
  
  wide[[match_col]] <- apply(wide, 1, function(row) {
    current <- row[[var_current]]
    if (is.na(current)) {
      return(NA)
    } else {
      row_vars <- sapply(var_cols, function(col) row[[col]])
      available_years <- years_range[!is.na(row_vars)]
      first_yr <- min(available_years)
      if (yr == first_yr) {
        return(99)
      } else {
        prev_year <- yr - 1
        prev_val  <- row[[paste0("var_", prev_year)]]
        if (is.na(prev_val)) {
          return(NA)
        } else {
          return(ifelse(current == prev_val, 1, 0))
        }
      }
    }
  })
}

# ---------------------------------------------------------------------------- #
# Compute Summary Columns: first_year, last_year, present, absent              #
# ---------------------------------------------------------------------------- #
wide <- wide %>%
  mutate(
    first_year = apply(select(., all_of(var_cols)), 1, function(x) {
      yrs <- years_range[!is.na(x)]
      if (length(yrs) == 0) NA else min(yrs)
    }),
    last_year = apply(select(., all_of(var_cols)), 1, function(x) {
      yrs <- years_range[!is.na(x)]
      if (length(yrs) == 0) NA else max(yrs)
    }),
    present = apply(select(., all_of(var_cols)), 1, function(x) {
      yrs <- years_range[!is.na(x)]
      if (length(yrs) == 0) NA else paste(yrs, collapse = ", ")
    }),
    absent = apply(select(., all_of(var_cols)), 1, function(x) {
      yrs <- years_range[!is.na(x)]
      missing <- setdiff(years_range, yrs)
      if (length(missing) == 0) NA else paste(missing, collapse = ", ")
    })
  )

# ---------------------------------------------------------------------------- #
# Reorder Final Columns                                                        #
# ---------------------------------------------------------------------------- #
# Order the final data frame with: subfolder, description,
# then alternating var_YEAR and match_YEAR for each year, then first_year, 
# last_year, present, absent.
year_cols <- unlist(lapply(years_range, function(yr) c(paste0("var_", yr), 
                                                       paste0("match_", yr))))
final_df <- wide %>%
  filter(!is.na(description)) %>%
  select(subfolder, 
         description, 
         all_of(year_cols), 
         first_year, 
         last_year, 
         present, 
         absent)

# ---------------------------------------------------------------------------- #
# Save Final Evaluation Results to CSV Files with Numeric Prefixes             #
# ---------------------------------------------------------------------------- #
write_csv(
  final_df,
  file.path(output_folder, "001_variable_name_evaluation.csv")
)

################################################################################
######################## CONTINUED IN NEXT SCRIPT... ###########################
################################################################################