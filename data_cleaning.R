# Data Cleaning Script

 
# Load Packages -----------------------------------------------------------

library(tidyverse)
library(lubridate)
library(rlang)
library(skimr)
library(magrittr)
library(leaflet) ## For leaflet interactive maps
library(sf) ## For spatial data
library(RColorBrewer) ## For colour palettes
library(htmltools) ## For html
library(leafsync) ## For placing plots side by side
library(kableExtra) ## Table output
library(ggmap) ## for google geocoding

# Load Data ---------------------------------------------------------------
seamstresses <- read_csv("data/seamstresses.csv")
genealogy <- read_csv("data/genealogy.csv")
worcester_directory <- read_csv("data/worcester.csv")
ad_links <- read_csv("data/ad_links.csv")
api_key <- google_api_key

# Worcester City Directory in Seamstresses Sheet ------------------------------

date_na <- genealogy %>% 
  filter(is.na(ad_date) == TRUE) %>% 
  mutate(ad_date = case_when(ad_id == "Worcester1844" ~ 1844))

date_available <- genealogy %>% 
  filter(is.na(ad_date) == FALSE)

genealogy <- rbind(date_na, date_available)


# Separation by Job Title -------------------------------------------------

seamstresses_separated <- seamstresses %>% 
  separate_rows(job_title, sep = "; ") %>% 
  mutate(job_title = fct_collapse(job_title,
                                  Milliner = c("Milliner", "milliner"),
                                  Dressmaker = c("Dressmaker", "dressmaker"),
                                  Tailor = c("Tailor"),
                                  Tailoress = c("Tailoress", "tailoress"),
                                  Mantuamaker = c("Mantuamaker", "mantuamaker", "Mantua-Maker"),
                                  Apprentices = c("apprentices")
  ))

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
         ) %>% 
  rename("name" = "name.x") %>% 
  distinct()


# Age Calculation ---------------------------------------------------------

age <- function(birth, event) {
  as.period(interval(birth, event), unit = "year")
}

ages <- ages %>% 
  mutate(age_ad = age(birth_date_format, ad_date_format))

ages <- ages %>% 
  mutate(age_ad = time_length(age_ad, unit = "years"))


# Marriages ---------------------------------------------------------------

marriages <- genealogy %>% 
  filter(is.na(spouse) == FALSE) %>% 
  separate_rows(spouse, sep = "; ") %>% 
  separate_rows(marriage_date, sep = "; ") %>% 
  filter(!grepl("2nd", spouse)) %>% 
  filter(name != "Bradford Baylies") %>% 
  filter(marriage_date != "28 August 1851") %>% 
  filter(marriage_date != "15 August 1850") %>% 
  select(name, spouse, marriage_date) %>% 
  left_join(y = ages, by = "name") %>% 
  mutate(marriage_date_format = lubridate::parse_date_time(marriage_date, 
                                              orders = c("%d/%B/%Y", 
                                                         "%B/%Y", 
                                                         "%Y"))) %>% 
  distinct() %>% 
  mutate(age_marriage = age(birth_date_format, marriage_date_format)) %>% 
  mutate(age_marriage = time_length(age_marriage, unit = "years"))

# Marriage vs Ad ----------------------------------------------------------

marriages <- marriages %>% 
  mutate(diff_ad_marriage = age(ad_date_format, marriage_date_format)) %>% 
  mutate(diff_ad_marriage = time_length(diff_ad_marriage, unit = "years"))

# Summary Stats -----------------------------------------------------------

marriages %>% 
  summarize(med_marriage_age = median(age_marriage, na.rm = TRUE),
            mean_marriage_age = mean(age_marriage, na.rm = TRUE))

ages %>% 
  summarize(med_ad_age =  median(age_ad, na.rm = TRUE),
            mean_ad_age = mean(age_ad, na.rm = TRUE))

marriages %>% 
  summarize(med_diff = median(diff_ad_marriage, na.rm = TRUE),
            mean_diff = mean(diff_ad_marriage, na.rm = TRUE))


# Worcester Sheet ---------------------------------------------------------

worcester <- seamstresses %>% 
  filter(city == "Worcester") %>% 
  filter(is.na(address) == FALSE) %>% 
  mutate(street_address = paste(address, city, state, sep = ", ")) %>% 
  select(name, job_title, street_address, year, source_name) 

worcester_directory <- worcester_directory %>% 
  mutate(source_name = "Worcester City Directory") %>% 
  mutate(street_address = paste(street_address, city, state, sep = ", ")) %>% 
  select(name, job_title, street_address, year, source_name)

worcester <- rbind(worcester, worcester_directory)


# Add Ads to Map Data -----------------------------------------------------

genealogy <- genealogy %>% 
  left_join(ad_links, by = "ad_id") %>% 
  rename(ad_image = ad_link.y)


# MAP PREP ----------------------------------------------------------------

# Load data 
ma_towns <- st_read("data/townssurvey_shp/TOWNSSURVEY_POLYM.shp")

# Join shape data to MA set
map_data <- seamstresses %>% 
  filter(state == "MA") %>% 
  filter(is.na(city) == FALSE) %>% 
  select(city) %>% 
  mutate(city = case_when(city == "Williamsburgh" ~ "WILLIAMSBURG",
                          TRUE ~ toupper(city))) %>% 
  group_by(city) %>% 
  summarize(number = n()) %>% 
  right_join(y = ma_towns, by = c("city" = "TOWN")) %>% 
  select(city, number, geometry) %>% 
  st_as_sf() %>% 
  write_csv("data/map_data_cleaned.csv")

# Determine frequencies of job titles by town
job_title_popularity <- seamstresses %>% 
  separate_rows(job_title, sep = "; ") %>% 
  mutate(job_title = fct_collapse(job_title,
                                  Milliner = c("Milliner", "milliner"),
                                  Dressmaker = c("Dressmaker", "dressmaker"),
                                  Tailor = c("Tailor"),
                                  Tailoress = c("Tailoress", "tailoress"),
                                  Mantuamaker = c("Mantuamaker", "mantuamaker", "Mantua-Maker"),
                                  Apprentices = c("apprentices")
  )) %>% 
  filter(state == "MA", is.na(job_title) == FALSE, is.na(city) == FALSE) %>% 
  group_by(city) %>% 
  count(job_title) %>% 
  slice(which.max(n)) %>% 
  mutate(city = case_when(city == "Williamsburgh" ~ "WILLIAMSBURG",
                          TRUE ~ toupper(city))) %>% 
  right_join(y = ma_towns, by = c("city" = "TOWN")) %>% 
  select(city, job_title, geometry) %>% 
  st_as_sf() %>% 
  ms_simplify(keep = 0.02) %>% 
  write_csv("data/job_titles_cleaned.csv")

# Geocode Worcester Sheet

worcester_geo <- worcester %>% 
  mutate_geocode(location = street_address, 
                 output = "latlon", 
                 source = "google") 

worcester_geo <- worcester_geo %>% 
  filter(name != "Lucina Grover",
         name != "Amelia Goodrich",
         name != "Harriet Fisk") %>% 
  mutate(lat = jitter(lat, factor = 0.005)) %>% 
  mutate(lon = jitter(lon, factor = 0.005)) %>% 
  mutate(popup = paste("<b>", name, "</b>" ,
                       "<br>", year, 
                       "<br>", job_title, 
                       "<br>", street_address,
                       "<br>", source_name)
  ) %>% 
  write_csv("data/worcester_geo_cleaned.csv")

# worcester_geo <- worcester_geo %>% 
#   st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
#   st_jitter(factor = 0.002) %>% 
  

# Geocode MA Data
seamstresses_ma <- seamstresses %>% 
  filter(state == "MA") %>% 
  filter(is.na(city) == FALSE) %>% 
  filter(city != "Haverhill") %>% 
  select(name, city, state, location_type, job_title, source_name, year, ad_id) %>% 
  mutate(city = case_when(city == "Williamsburgh" ~ "Williamsburg",
                          city == "Haverhill" ~ "Haverhill",
                          TRUE ~ city)) %>% 
  mutate(city_state = paste(city, state, sep = ", ")) 

ma_geo <- seamstresses_ma %>% 
  mutate_geocode(location = city_state, 
                 output = "latlon", 
                 source = "google") %>% 
  mutate(ma_popup = paste("<b>", name, "</b>" ,
                          "<br>", year, 
                          "<br>", job_title, 
                          "<br>", city_state,
                          "<br>", source_name)
  ) %>% 
  mutate(lat = jitter(lat, factor = 0.005)) %>% 
  mutate(lon = jitter(lon, factor = 0.005)) %>% 
  write_csv("data/ma_geo_cleaned.csv")

# ma_geo <- ma_geo %>% 
#   st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
#   st_jitter(factor = 0.002) %>% 
#   write_csv("data/ma_geo_cleaned.csv")

# Geocode Genealogy Data
genealogy_data <- genealogy %>% 
  filter(is.na(ad_image) == FALSE) %>% 
  filter(is.na(state) == FALSE) %>% 
  select(name, birth_date, father, mother, address, city, state, spouse, marriage_date, ad_date, death_date, ad_image, ad_id) %>% 
  unite(street_address, address, city, state, na.rm = TRUE, sep = ", ") %>% 
  mutate(genealogy_popup = paste("<b>", name, "</b>",
                                 "<br> Birth Date: ", birth_date,
                                 "<br> Parents: ", father, " and ", mother,
                                 "<br> Spouse: ", spouse,
                                 "<br> Marriage Date: ", marriage_date,
                                 "<br> Address: ", street_address,
                                 "<br> Death Date: ", death_date,
                                 "<br><img src=",ad_image," width = '200'>"))

genealogy_geo <- genealogy_data %>% 
  mutate_geocode(location = street_address, 
                 output = "latlon", 
                 source = "google") %>% 
  mutate(lat = jitter(lat, factor = 0.005)) %>% 
  mutate(lon = jitter(lon, factor = 0.005)) %>% 
  # st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  # st_jitter(factor = 0.002) %>% 
  write_csv("data/genealogy_cleaned.csv")


