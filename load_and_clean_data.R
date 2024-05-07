library(tidyverse)
library(dplyr)
library(readr)
library(dplyr)
library(forcats)
library(here)
library(dplyr)
library(tidyr)
library(broom)
library(dplyr)
library(gt)
library(purrr)

# For the first dataset 
data_path <- here("dataset-ignore", "twok22_data.csv")
rent_data <- read_csv(data_path,show_col_types = FALSE)
rent_data_clean <- rent_data %>%
  filter(RENTGRS != 0,
         !is.na(RENTGRS),
         EMPSTAT != "0" & EMPSTAT != "9",
         KITCHEN != "0", # Filter out rows where Kitchen is "0"
         ROOMS != "00", # Filter out rows where Rooms is "00"
         !INCTOT %in% c(1, 9999999, 9999998, -000001, "FTOTINC-000001"), # Filter based on INCTOT criteria
         !FTOTINC %in% c(9999998, 9999999), # Filter based on FTOTINC criteria
         POVERTY != 000
  ) # Filter based on POVERTY criteria
rent_data_clean$REGION_CLASSIFIED <- case_when(
  rent_data_clean$REGION %in% c(11, 12, 13) ~ "NORTHEAST",
  rent_data_clean$REGION %in% c(21, 22, 23) ~ "MIDWEST",
  rent_data_clean$REGION %in% c(31, 32, 33, 34) ~ "SOUTH",
  rent_data_clean$REGION %in% c(41, 42, 43) ~ "WEST",
  TRUE ~ "OTHER"
)

#saveRDS(rent_data_clean, file = here("dataset", "latest.RData"))
#write_csv(rent_data_clean, file = here("dataset", "latest.csv"))


rent_data_clean <- rent_data_clean %>%
  mutate(KITCHEN = case_when(
    KITCHEN == "0" ~ "N/A",
    KITCHEN == "1" ~ "No",
    KITCHEN == "2" ~ "No, or shared use",
    KITCHEN == "3" ~ "Yes, shared use",
    KITCHEN == "4" ~ "Yes (shared or exclusive use)",
    KITCHEN == "5" ~ "Yes, exclusive use",
    TRUE ~ as.character(KITCHEN)
  ))
rent_data_clean$KITCHEN <- as.factor(rent_data_clean$KITCHEN)

#MARST
rent_data_clean <- rent_data_clean %>%
  mutate(MARST = case_when(
    MARST == "1" ~ "Married, spouse present",
    MARST == "2" ~ "Married, spouse absent",
    MARST == "3" ~ "Separated",
    MARST == "4" ~ "Divorced",
    MARST == "5" ~ "Widowed",
    MARST == "6" ~ "Never married/single",
    MARST == "9" ~ "Blank, missing",
    TRUE ~ as.character(MARST)
  ))
rent_data_clean$MARST <- as.factor(rent_data_clean$MARST)

#RACE
rent_data_clean <- rent_data_clean %>%
  mutate(RACE = case_when(
    RACE == "1" ~ "White",
    RACE == "2" ~ "Black/African American",
    RACE == "3" ~ "American Indian or Alaska Native",
    RACE == "4" ~ "Chinese",
    RACE == "5" ~ "Japanese",
    RACE == "6" ~ "Other Asian or Pacific Islander",
    RACE == "7" ~ "Other race, nec",
    RACE == "8" ~ "Two major races",
    RACE == "9" ~ "Three or more major races",
    TRUE ~ as.character(RACE) 
  ))
rent_data_clean$RACE <- as.factor(rent_data_clean$RACE)

#EMPSTAT
rent_data_clean <- rent_data_clean %>%
  mutate(EMPSTAT = case_when(
    EMPSTAT == "1" ~ "Employed",
    EMPSTAT == "2" ~ "Unemployed",
    EMPSTAT == "3" ~ "Not in labour force",
    TRUE ~ as.character(EMPSTAT) # This line should return the existing value of EMPSTAT
  ))
rent_data_clean$EMPSTAT <- as.factor(rent_data_clean$EMPSTAT)



data_2022 = rent_data_clean
# Filter out non-positive values before log transformation
data_2022 <- data_2022[data_2022$RENTGRS > 0 & data_2022$INCTOT > 0, ]

# Apply log transformation
data_2022$log_RENTGRS <- log(data_2022$RENTGRS)
data_2022$log_INCTOT <- log(data_2022$INCTOT)


# Calculating means and standard deviations for the log-transformed columns
mean_log_RENTGRS <- mean(data_2022$log_RENTGRS, na.rm = TRUE)
sd_log_RENTGRS <- sd(data_2022$log_RENTGRS, na.rm = TRUE)
mean_log_INCTOT <- mean(data_2022$log_INCTOT, na.rm = TRUE)
sd_log_INCTOT <- sd(data_2022$log_INCTOT, na.rm = TRUE)


# Defining lower and upper bounds for both variables
lb_log_RENTGRS <- mean_log_RENTGRS - 3 * sd_log_RENTGRS
ub_log_RENTGRS <- mean_log_RENTGRS + 3 * sd_log_RENTGRS
lb_log_INCTOT <- mean_log_INCTOT - 3 * sd_log_INCTOT
ub_log_INCTOT <- mean_log_INCTOT + 3 * sd_log_INCTOT

# Filtering out outliers
data_2022_clean <- data_2022[data_2022$log_RENTGRS >= lb_log_RENTGRS & data_2022$log_RENTGRS <= ub_log_RENTGRS & 
                               data_2022$log_INCTOT >= lb_log_INCTOT & data_2022$log_INCTOT <= ub_log_INCTOT, ]
#saveRDS(rent_data_clean, file = here("dataset", "latest_clean.rds"))
#write_csv(data_2022_clean, file = here("dataset", "latest_clean.csv"))




# Second dataset
data_path <- here("dataset-ignore", "twenty_yr_data.csv")
twentyyr_data <- read_csv(data_path,show_col_types = FALSE)
set.seed(123) # Setting a seed for reproducibility
twentyyr_data_clean <- sample_frac(twentyyr_data, 0.01)
twentyyr_data_clean <- twentyyr_data_clean %>%
  filter(RENTGRS != 0,  # Exclude entries with zero rent
         !is.na(RENTGRS),  # Exclude NA values for rent
         EMPSTAT %in% c("1", "2", "3"),  # Include only valid employment statuses
         KITCHEN != "0",  # Exclude entries without a kitchen
         ROOMS != "00",  # Exclude entries with invalid room counts
         !INCTOT %in% c(1, 9999999, 9999998, -1),  # Exclude specific INCTOT values
         !FTOTINC %in% c(9999998, 9999999),  # Exclude specific FTOTINC values
         POVERTY != 0,  # Exclude invalid poverty codes
         !REGION %in% c(91, 92, 97, 99))  # Exclude specific region/state codes
twentyyr_data_clean$REGION_CLASSIFIED <- case_when(
  twentyyr_data_clean$REGION %in% c(11, 12, 13) ~ "NORTHEAST",
  twentyyr_data_clean$REGION %in% c(21, 22, 23) ~ "MIDWEST",
  twentyyr_data_clean$REGION %in% c(31, 32, 33, 34) ~ "SOUTH",
  twentyyr_data_clean$REGION %in% c(41, 42, 43) ~ "WEST",
  TRUE ~ "OTHER"
)

#KITCHEN
twentyyr_data_clean <- twentyyr_data_clean %>%
  mutate(KITCHEN = case_when(
    KITCHEN == "0" ~ "N/A",
    KITCHEN == "1" ~ "No",
    KITCHEN == "2" ~ "No, or shared use",
    KITCHEN == "3" ~ "Yes, shared use",
    KITCHEN == "4" ~ "Yes (shared or exclusive use)",
    KITCHEN == "5" ~ "Yes, exclusive use",
    TRUE ~ as.character(KITCHEN)
  ))
twentyyr_data_clean$KITCHEN <- as.factor(twentyyr_data_clean$KITCHEN)

#MARST
twentyyr_data_clean <- twentyyr_data_clean %>%
  mutate(MARST = case_when(
    MARST == "1" ~ "Married, spouse present",
    MARST == "2" ~ "Married, spouse absent",
    MARST == "3" ~ "Separated",
    MARST == "4" ~ "Divorced",
    MARST == "5" ~ "Widowed",
    MARST == "6" ~ "Never married/single",
    MARST == "9" ~ "Blank, missing",
    TRUE ~ as.character(MARST)
  ))
twentyyr_data_clean$MARST <- as.factor(twentyyr_data_clean$MARST)

#RACE
twentyyr_data_clean <- twentyyr_data_clean %>%
  mutate(RACE = case_when(
    RACE == "1" ~ "White",
    RACE == "2" ~ "Black/African American",
    RACE == "3" ~ "American Indian or Alaska Native",
    RACE == "4" ~ "Chinese",
    RACE == "5" ~ "Japanese",
    RACE == "6" ~ "Other Asian or Pacific Islander",
    RACE == "7" ~ "Other race, nec",
    RACE == "8" ~ "Two major races",
    RACE == "9" ~ "Three or more major races",
    TRUE ~ as.character(RACE) 
  ))
twentyyr_data_clean$RACE <- as.factor(twentyyr_data_clean$RACE)

#EMPSTAT
twentyyr_data_clean <- twentyyr_data_clean %>%
  mutate(EMPSTAT = case_when(
    EMPSTAT == "1" ~ "Employed",
    EMPSTAT == "2" ~ "Unemployed",
    EMPSTAT == "3" ~ "Not in labour force",
    TRUE ~ as.character(EMPSTAT) 
  ))

twentyyr_data_clean$EMPSTAT <- as.factor(twentyyr_data_clean$EMPSTAT)

#write_csv(twentyyr_data_clean, file = here("dataset", "twentyyear_rent.csv"))
#saveRDS(twentyyr_data_clean, file = here("dataset", "twentyyear_rent.rds"))


data_path <- here("dataset", "twentyyear_rent.csv")
data_twenty <- read_csv(data_path,show_col_types = FALSE) 
data_twenty_positive <- data_twenty[data_twenty$RENTGRS > 0 & data_twenty$INCTOT > 0, ]

# Apply log transformation correctly
data_twenty_positive$log_RENTGRS <- log(data_twenty_positive$RENTGRS)
data_twenty_positive$log_INCTOT <- log(data_twenty_positive$INCTOT)

# Calculating means and standard deviations for the log-transformed columns
mean_log_RENTGRS <- mean(data_twenty_positive$log_RENTGRS, na.rm = TRUE)
sd_log_RENTGRS <- sd(data_twenty_positive$log_RENTGRS, na.rm = TRUE)
mean_log_INCTOT <- mean(data_twenty_positive$log_INCTOT, na.rm = TRUE)
sd_log_INCTOT <- sd(data_twenty_positive$log_INCTOT, na.rm = TRUE)

# Defining lower and upper bounds for both variables
lb_log_RENTGRS <- mean_log_RENTGRS - 3 * sd_log_RENTGRS
ub_log_RENTGRS <- mean_log_RENTGRS + 3 * sd_log_RENTGRS
lb_log_INCTOT <- mean_log_INCTOT - 3 * sd_log_INCTOT
ub_log_INCTOT <- mean_log_INCTOT + 3 * sd_log_INCTOT

# Filter for normal range values
data_clean <- data_twenty_positive %>%
  filter(log_RENTGRS >= lb_log_RENTGRS & log_RENTGRS <= ub_log_RENTGRS,
         log_INCTOT >= lb_log_INCTOT & log_INCTOT <= ub_log_INCTOT) %>%
  filter(complete.cases(KITCHEN, ROOMS, NFAMS, AGE, MARST, RACE, EMPSTAT, REGION_CLASSIFIED)) %>%
  filter_all(all_vars(!is.infinite(.)))
#saveRDS(twentyyr_data_clean, file = here("dataset", "data_twenty_clean.rds"))
#write_csv(data_clean, file = here("dataset", "data_twenty_clean.csv"))








