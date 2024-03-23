library(tidyverse)

rent_data <- read_csv(here::here("dataset", "rent_income.csv"))

## CLEAN the data
rent_data_clean <- rent_data

write_csv(rent_data_clean, file = here::here("dataset", "rent_income.csv"))

save(rent_data_clean, file = here::here("dataset/rent_income.csv.RData"))
