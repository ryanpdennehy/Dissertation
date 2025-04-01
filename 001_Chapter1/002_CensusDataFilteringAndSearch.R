################################################################################
################################################################################
################################################################################
### RYAN P. DENNEHY - PhD DISSERTATION - REPLICATION CODE
### CHAPTER 1 - TOWARD AN AMERICAN SOCIOECONOMIC INDEX FOR USE IN POLITICAL...
### LAST UPDATED: APR. 1, 2025
### 002 - CENSUS DATA FILTERING AND SEARCH
### ======================================================================== ###
### Purpose: Load the output CSV from the previous script, subset for 
###          Percent and Estimate variables, compute a presence_score, and 
###          sort so that for each underlying variable (i.e. same base 
###          description) the main row (with no "Margin of Error") appears 
###          immediately before its corresponding margin-of-error row.
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
# Define File Paths                                                            #
# ---------------------------------------------------------------------------- #
input_file  <- file.path("001_Chapter1", "101_MetadataEvaluation", 
                         "001_variable_name_evaluation.csv")

output_file <- file.path("001_Chapter1", "101_MetadataEvaluation", 
                         "002_variable_name_subsets.csv")

# ---------------------------------------------------------------------------- #
# Read Input CSV File                                                          #
# ---------------------------------------------------------------------------- #
df <- read_csv(input_file)

# ---------------------------------------------------------------------------- #
# Subset for Descriptions Starting with Relevant Prefixes                      #
# ---------------------------------------------------------------------------- #
df_subset <- df %>%
  filter(
    str_starts(description, "Percent!!")                  | 
      str_starts(description, "Percent Margin of Error!!")  |
      str_starts(description, "Estimate!!")                 | 
      str_starts(description, "Estimate Margin of Error!!")
  )

# ---------------------------------------------------------------------------- #
# Derive Base Description and Grouping Variables                               #
# ---------------------------------------------------------------------------- #
df_subset <- df_subset %>%
  mutate(
    base_description = case_when(
      str_starts(description, "Percent!!") ~ 
        str_remove(description, "^Percent!!"),
      
      str_starts(description, "Percent Margin of Error!!") ~ 
        str_remove(description, "^Percent Margin of Error!!"),
      
      str_starts(description, "Estimate!!") ~ 
        str_remove(description, "^Estimate!!"),
      
      str_starts(description, "Estimate Margin of Error!!") ~ 
        str_remove(description, "^Estimate Margin of Error!!")
    ),
    group_type = case_when(
      str_starts(description, "Percent")  ~ "Percent",
      str_starts(description, "Estimate") ~ "Estimate"
    ),
    order_priority = case_when(
      str_starts(description, "Percent!!")                  ~ 1,
      str_starts(description, "Estimate!!")                 ~ 1,
      str_starts(description, "Percent Margin of Error!!")  ~ 2,
      str_starts(description, "Estimate Margin of Error!!") ~ 2,
      TRUE                                                  ~ 99
    )
  )

# ---------------------------------------------------------------------------- #
# Identify Year Columns and Compute Presence Score                             #
# ---------------------------------------------------------------------------- #
var_columns <- grep("^var_\\d{4}$", names(df_subset), value = TRUE)

df_subset <- df_subset %>%
  mutate(
    presence_score = rowSums(!is.na(select(., all_of(var_columns))))
  )

# ---------------------------------------------------------------------------- #
# Assign Group-Level Presence Scores                                           #
# ---------------------------------------------------------------------------- #
df_subset <- df_subset %>%
  group_by(group_type, base_description) %>%
  mutate(
    group_presence = if (any(order_priority == 1)) {
      first(presence_score[order_priority == 1])
    } else {
      first(presence_score)
    }
  ) %>%
  ungroup()

# ---------------------------------------------------------------------------- #
# Sort Rows by Group Presence and Ordering Priority                            #
# ---------------------------------------------------------------------------- #
df_sorted <- df_subset %>%
  arrange(
    desc(group_presence), 
    group_type, 
    base_description, 
    order_priority
  )

# ---------------------------------------------------------------------------- #
# Write Output CSV                                                             #
# ---------------------------------------------------------------------------- #
write_csv(
  df_sorted, 
  output_file
)

################################################################################
######################## CONTINUED IN NEXT SCRIPT... ###########################
################################################################################