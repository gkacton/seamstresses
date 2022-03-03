# Data Cleaning Script


# Load Packages -----------------------------------------------------------

library(tidyverse)
library(lubridate)
library(rlang)
library(skimr)

# Load Data ---------------------------------------------------------------
seamstresses <- read_csv("data/seamstresses.csv")
genealogy <- read_csv("data/genealogy.csv")


# Worcester City Directory -------------------------------------------------

date_na <- genealogy %>% 
  filter(is.na(ad_date) == TRUE) %>% 
  mutate(ad_date = case_when(ad_id == "Worcester1844" ~ 1844))

date_available <- genealogy %>% 
  filter(is.na(ad_date) == FALSE)

genealogy <- rbind(date_na, date_available)




# Persis Goldthwait -------------------------------------------------------

seamstresses["name_id"][seamstresses["name"] == "Goldthwait P"] <- "Goldthwait P"
seamstresses["name_id"][seamstresses["name"] == "Persis Goldthwaith"] <- "Goldthwait P"

# Joining Data Frames -----------------------------------------------------

seamstresses_tojoin <- seamstresses %>% 
  separate_rows(name_id, sep = "; ") 
joined <- left_join(x = genealogy, y = seamstresses_tojoin, by = "name_id")

# Date Formatting ---------------------------------------------------------

ages <- joined %>% 
  select(name.x, birth_date, ad_date) %>% 
  mutate(birth_date_format = lubridate::parse_date_time(birth_date, 
                                                        orders = c("%d/%B/%Y", 
                                                                   "%B/%Y", 
                                                                   "%Y"))
         ) %>% 
  mutate(ad_date_format = lubridate::parse_date_time(ad_date, 
                                                     orders = c("%d/%B/%Y", 
                                                                "%B/%Y", 
                                                                "%Y"))
         )


# Age Calculation ---------------------------------------------------------

age <- function(birth, event) {
  as.period(interval(birth, event), unit = "year")
}

ages <- ages %>% 
  mutate(age_ad = age(birth_date_format, ad_date_format))

ages <- ages %>% 
  mutate(age_ad_years = time_length(age_ad, unit = "years"))


