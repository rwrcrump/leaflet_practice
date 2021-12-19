library(tidyverse)
library(vroom)
library(sf)
library(tigris)
library(leaflet)
library(htmlwidgets)
library(lubridate)

download.file(url = "https://github.com/nychealth/coronavirus-data/archive/master.zip",
              destfile = "coronavirus-data-master.zip")

unzip(zipfile = "coronavirus-data-master.zip")

# raw data
percentops <- vroom("coronavirus-data-master/trends/percentpositive-by-modzcta.csv")
caserate <- vroom("coronavirus-data-master/trends/caserate-by-modzcta.csv")
testrate <- vroom("coronavirus-data-master/trends/testrate-by-modzcta.csv")

# shapefiles and zcta conversion table
modzcta <- st_read("coronavirus-data-master/Geography-resources/MODZCTA_2010.shp")
zcta_conv <- vroom("coronavirus-data-master/Geography-resources/ZCTA-to-MODZCTA.csv", delim = ",")

# clean data and reshape
percpositives <- percentops %>% select(-c(2:7))
percpos_long <- percpositives %>% 
  pivot_longer(2:178, names_to = "modzcta",
               names_prefix = "PCTPOS_", values_to = "pctpos")


caserates <- caserate %>% select(-c(2:7))
caserates_long <- caserates %>% 
  pivot_longer(2:178, names_to = "modzcta",
               names_prefix = "CASERATE_", values_to = "caserate")

testrates <- testrate %>% select(-c(2:7))
testrates_long <- testrates %>% 
  pivot_longer(2:178, names_to = "modzcta",
               names_prefix = "TESTRATE_", values_to = "testrate")

# merge data
all <- percpos_long %>% 
  left_join(caserates_long, by = c("week_ending", "modzcta")) %>% 
  left_join(testrates_long, by = c("week_ending", "modzcta"))

# geo data
all_modzcta <- geo_join(modzcta, all,
                        "MODZCTA", "modzcta",
                        how = "inner")

# convert to date, still not sure why the date format is off?
all_modzcta$week_ending <- as.Date(all_modzcta$week_ending, format = "%m/%d/%Y")

# save data for shiny app
saveRDS(all_modzcta, "all_modzcta.RDS")

#### MAKE INTERACTIVE MAP ####

labels <- sprintf(
  "<strong>%s</strong><br/>%g cases per 100,000 people",
  all_modzcta$MODZCTA, all_modzcta$caserate) %>% 
  lapply(htmltools::HTML)

pal <- colorBin(palette = "OrRd", 9, domain = all_modzcta$caserate)

map_interactive <- all_modzcta %>% 
  st_transform(crs = "+init=epsg:4326") %>% 
  leaflet() %>% 
  addProviderTiles(provider = "CartoDB.Positron") %>% 
  addPolygons(label = labels,
              stroke = FALSE,
              smoothFactor = .5,
              opacity = 1,
              fillOpacity = 0.7,
              fillColor = ~pal(caserate),
              highlightOptions = highlightOptions(weight = 5,
                                                  fillOpacity = 1,
                                                  color = "black",
                                                  opacity = 1,
                                                  bringToFront = TRUE)) %>% 
  addLegend("bottomright",
            pal = pal,
            values = ~caserate,
            title = "Cases per 100,000 people",
            opacity = 0.7)

saveWidget(l, "nyc_covid_caserate_map.html")

















