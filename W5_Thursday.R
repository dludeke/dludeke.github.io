library(tidyverse)
library(censusapi)
library(sf)
library(tigris)
library(mapview)

library(readxl)

temp <- tempfile()
download.file("https://oehha.ca.gov/media/downloads/calenviroscreen/document/calenviroscreen40resultsdatadictionaryf2021.zip",destfile = temp)

# ces4 <- read_excel(
#   unzip(
#     temp,
#     "calenviroscreen40resultsdatadictionary_F_2021.xlsx"
#   ),
#   sheet = "CES4.0FINAL_results"
# )

unlink(temp)

ces4 <- read_excel("calenviroscreen40resultsdatadictionary_F_2021.xlsx")

colnames(ces4)

ces4_clean <- ces4 %>% 
  dplyr::select(!ends_with("Pctl"))

# see if there are any missing entries for a variable
sum(is.na(ces4_clean$Poverty))

# equivalent in pipe format 
ces4_clean %>% 
  pull(Poverty) %>% 
  is.na() %>% 
  sum()


ca_tracts <- tracts("CA")

ces4_map <- ces4_clean %>% 
  left_join(
    ca_tracts %>% 
      transmute(GEOID = as.numeric(GEOID)), # transmute mutates and selects only that thing
      # select(GEOID) %>% 
      # mutate(GEOID = as.numeric(GEOID)), 
    by = c("Census Tract" = "GEOID")
  ) %>% 
  st_as_sf() # needed because ces4_clean is not a spatial object, even though we've added the geometry

mapview(ces4_map, zcol = "Asthma")


