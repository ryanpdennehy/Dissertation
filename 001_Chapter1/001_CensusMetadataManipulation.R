################################################################################
################################################################################
################################################################################
### RYAN P. DENNEHY - PhD DISSERTATION - REPLICATION CODE
### CHAPTER 1 - TOWARD AN AMERICAN SOCIOECONOMIC INDEX FOR USE IN POLITICAL...
### LAST UPDATED: MAR. 23, 2025
### 001 - CENSUS METADATA MANIPULATION
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
  stringr
)

# ---------------------------------------------------------------------------- #
# Create Output Directory for Data Headers Evaluation CSV Files                #
# ---------------------------------------------------------------------------- #
output_folder <- file.path(
  "001_Chapter1", "101_MetadataEvaluation"
)
if (!dir.exists(output_folder)) {
  dir.create(
    output_folder,
    recursive = TRUE
  )
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
read_data_header <- function(file_path) {
  subfolder <- basename(dirname(file_path))
  year      <- as.integer(
    str_match(
      basename(file_path),
      "ACSDP5Y(\\d{4})"
    )[[2]]
  )
  
  data <- read_csv(
    file_path,
    n_max          = 2,
    col_names      = FALSE,
    show_col_types = FALSE
  )
  
  data       <- data[ , -(1:2)]
  transposed <- as_tibble(
    t(data),
    .name_repair = "minimal"
  )
  names(transposed)[1:2] <- c("name", "info")
  
  transposed <- transposed %>% mutate(
    subfolder = subfolder,
    year      = year
  )
  return(transposed)
}

headers_all <- lapply(
  file_paths,
  read_data_header
) %>% bind_rows()

write_csv(
  headers_all,
  file.path(output_folder, "000_full_data_headers.csv")
)

# ---------------------------------------------------------------------------- #
# Analyze Consistency of Data Headers (Name/Info Pairings)                     #
# ---------------------------------------------------------------------------- #
headers_summary <- headers_all %>%
  group_by(subfolder, name) %>%
  summarise(
    info        = first(info),
    info_values = paste(unique(info), collapse = " | "),
    years       = paste(sort(unique(year)), collapse = ", "),
    count_years = n_distinct(year),
    .groups     = "drop"
  ) %>%
  mutate(
    consistency = if_else(
      grepl("\\|", info_values),
      "inconsistent",
      "consistent"
    ),
    present = years,
    absent  = sapply(
      strsplit(years, ", "),
      function(x) {
        all_years <- sort(unique(headers_all$year))
        paste(
          setdiff(all_years, as.integer(x)),
          collapse = ", "
        )
      }
    )
  ) %>%
  select(
    subfolder,
    name,
    info,
    present,
    absent,
    info_values,
    count_years,
    consistency
  )

# ---------------------------------------------------------------------------- #
# Identify Variables Present in All Years vs. New/Discontinued                 #
# ---------------------------------------------------------------------------- #
all_years <- sort(unique(headers_all$year))
min_year  <- min(all_years)
max_year  <- max(all_years)

headers_presence <- headers_summary %>%
  mutate(
    first_year = sapply(
      strsplit(present, ", "),
      function(x) min(as.integer(x))
    ),
    last_year = sapply(
      strsplit(present, ", "),
      function(x) max(as.integer(x))
    ),
    new_flag = if_else(
      first_year > min_year,
      "new",
      NA_character_
    ),
    discontinued_flag = if_else(
      last_year < max_year,
      "discontinued",
      NA_character_
    ),
    status = case_when(
      !is.na(new_flag) & !is.na(discontinued_flag) ~ "new and discontinued",
      !is.na(new_flag)                             ~ "new",
      !is.na(discontinued_flag)                    ~ "discontinued",
      TRUE                                         ~ "present in all years"
    )
  ) %>%
  select(
    subfolder,
    name,
    info,
    present,
    absent,
    info_values,
    count_years,
    consistency,
    first_year,
    last_year,
    new_flag,
    discontinued_flag,
    status
  )

# ---------------------------------------------------------------------------- #
# Create Granular Subsets for Final Output                                     #
# ---------------------------------------------------------------------------- #
consistent_headers <- headers_presence %>%
  filter(consistency == "consistent") %>%
  select(-present, -absent)

inconsistent_headers <- headers_presence %>%
  filter(consistency == "inconsistent")

present_all_years_headers <- headers_presence %>%
  filter(status == "present in all years")

new_or_discontinued_headers <- headers_presence %>%
  filter(status != "present in all years")

# ---------------------------------------------------------------------------- #
# Final Cleanup for CSV Outputs                                                #
# ---------------------------------------------------------------------------- #
headers_all_final <- headers_all %>%
  filter(!is.na(name))

headers_summary_final <- headers_summary %>%
  filter(!is.na(name)) %>%
  select(-info_values)

headers_presence_final <- headers_presence %>%
  filter(!is.na(name)) %>%
  select(-info_values)

consistent_headers_final <- consistent_headers %>%
  filter(!is.na(name)) %>%
  select(-info_values)

inconsistent_headers_final <- inconsistent_headers %>%
  filter(!is.na(name)) %>%
  select(-info_values)

present_all_years_headers_final <- present_all_years_headers %>%
  filter(!is.na(name)) %>%
  select(-info_values)

new_or_discontinued_headers_final <- new_or_discontinued_headers %>%
  filter(!is.na(name)) %>%
  select(-info_values)

# ---------------------------------------------------------------------------- #
# Save Final Evaluation Results to CSV Files with Numeric Prefixes             #
# ---------------------------------------------------------------------------- #
write_csv(
  headers_all_final,
  file.path(output_folder, "000_full_data_headers.csv")
)

write_csv(
  headers_summary_final,
  file.path(output_folder, "001_headers_summary.csv")
)

write_csv(
  headers_presence_final,
  file.path(output_folder, "002_headers_evaluation.csv")
)

write_csv(
  consistent_headers_final,
  file.path(output_folder, "003_consistent_headers.csv")
)

write_csv(
  inconsistent_headers_final,
  file.path(output_folder, "004_inconsistent_headers.csv")
)

write_csv(
  present_all_years_headers_final,
  file.path(output_folder, "005_present_all_years_headers.csv")
)

write_csv(
  new_or_discontinued_headers_final,
  file.path(output_folder, "006_new_or_discontinued_headers.csv")
)

################################################################################
######################## CONTINUED IN NEXT SCRIPT... ###########################
################################################################################