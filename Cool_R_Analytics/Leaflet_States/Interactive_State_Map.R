install.packages('tigris')
#https://www.datascience.com/blog/visualizing-usa-housing-prices
# Note that things might get tricky with this particular package.
# If you are using a Linux OS, run the following before installing 'tigris':
# 1. sudo apt-get update && sudo apt-get install libgdal-dev libproj-dev
# 2. install.packages("rgdal")
# 3. install.packages("rgeos")
# 4. library(rgdal)
# 5. library(rgeos)
install.packages('tidyverse')
install.packages('leaflet')
install.packages('Quandl')
install.packages('lubridate')
install.packages('geojsonio')
install.packages('htmlwidgets')

library(tigris)
library(sp)
library(tidyverse)
library(leaflet)
library(Quandl)
library(lubridate)
library(htmlwidgets)
setwd("/Users/ndelzingaro/git_dir/analytics/Cool_R_Analytics/Leaflet_States/")
# You might want to supply an api key
# Quandl.api_key("[your key here]")

states_hpi <-
  Quandl("FMAC/HPI", order = 'asc') %>%
  select(-53:-54)

states_wrangled <- states_hpi  %>%
  filter(Date == ymd("2016-03-31") | Date == ymd("2017-03-31")) %>%
  gather(state, value, -Date) %>%
  group_by(state) %>%
  mutate(hpa = (value - lag(value))/ lag(value)) %>%
  mutate(hpa = round(hpa, digits = 4) * 100)  %>%
  na.omit() %>%
  select(state, hpa) %>%
  rename(STUSPS = state)

states <- states(cb = TRUE, class = "sf")

# Now we want to merge by a common column name.
states_hpa_leaflet <- merge(states, states_wrangled, by = "STUSPS", all.x = TRUE)

# Build states map
statesPal<- colorNumeric(
  palette = "GnBu",
  domain = states_hpa_leaflet$hpa)

statesPopup <- paste0(
  states_hpa_leaflet$NAME,
  "Annual House Price Percent Change:",
  states_hpa_leaflet$hpa, "%")

leaf_states <- leaflet(states_hpa_leaflet) %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(-95, 40, zoom = 4) %>%
  addPolygons(stroke = TRUE, color = "black", weight = .4, opacity = 1.0,
              smoothFactor = 0.5, fill = TRUE, fillColor = ~statesPal(hpa),
              fillOpacity = .8, layerId = ~STUSPS, popup = statesPopup)

leaf_states
saveWidget(leaf_states, file="leaf_states.html")

#https://rstudio.github.io/leaflet/choropleths.html
#https://stackoverflow.com/questions/43443260/how-to-download-geojson-data-and-read-it-to-r

library(geojson)
library(geojsonio)
url <- "http://leafletjs.com/examples/choropleth/us-states.js"

# read as text file
doc <- readLines(url)

# remove the javascript assignment at the front 
doc2 <- gsub("var statesData = ", "", doc)

# write out as a temp file and read
write(doc2, file = "tempgeo.json")
states <- geojson_read("tempgeo.json", what = "sp")
#states <- geojsonio::geojson_read("json/us-states.geojson", what = "sp")
class(states)
names(states)

m <- leaflet(states) %>%
  setView(-96, 37.8, 4) %>%
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))

m %>% addPolygons()

bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
pal <- colorBin("YlOrRd", domain = states$density, bins = bins)

m %>% addPolygons(
  fillColor = ~pal(density),
  weight = 2,
  opacity = 1,
  color = "white",
  dashArray = "3",
  fillOpacity = 0.7)

m %>% addPolygons(
  fillColor = ~pal(density),
  weight = 2,
  opacity = 1,
  color = "white",
  dashArray = "3",
  fillOpacity = 0.7,
  highlight = highlightOptions(
    weight = 5,
    color = "#666",
    dashArray = "",
    fillOpacity = 0.7,
    bringToFront = TRUE))

labels <- sprintf(
  "<strong>%s</strong><br/>%g people / mi<sup>2</sup>",
  states$name, states$density
) %>% lapply(htmltools::HTML)

m <- m %>% addPolygons(
  fillColor = ~pal(density),
  weight = 2,
  opacity = 1,
  color = "white",
  dashArray = "3",
  fillOpacity = 0.7,
  highlight = highlightOptions(
    weight = 5,
    color = "#666",
    dashArray = "",
    fillOpacity = 0.7,
    bringToFront = TRUE),
  label = labels,
  labelOptions = labelOptions(
    style = list("font-weight" = "normal", padding = "3px 8px"),
    textsize = "15px",
    direction = "auto"))
m
m %>% addLegend(pal = pal, values = ~density, opacity = 0.7, title = NULL,
                position = "bottomright")
