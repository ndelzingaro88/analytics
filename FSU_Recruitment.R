if(!require("leaflet")) install.packages('leaflet')
library(leaflet)
if(!require("mapview")) install.packages('mapview')
library(mapview)
if(!require("geojsonio")) install.packages('geojsonio')
library(geojsonio)
library(dplyr)

counties <- geojsonio::geojson_read("http://catalog.civicdashboards.com/dataset/cda82e8b-7a8b-411e-95ba-1200b921c35d/resource/5c5d19a0-b817-49e6-b76e-ea63a8e2c0f6/download/fd880c1e4d23463ca869f1122109b3eftemp.geojson", what = "sp")
counties = counties[order(counties$name),]

coaches = structure(list(Coach = c("Snyder", "Frey", "Bell", "Snyder", "Pimpleton", "Hampton", "Barnett", "Woodie", "Snyder", "Snyder", "Woodie", "Frey", "Woodie", "Barnett", "Frey", "Bell", "Snyder", "Barnett", "Barnett", "Snyder", "Woodie", "Barnett", "Frey", "Woodie", "Woodie", "Snyder", "Woodie", "Woodie", "Bell", "Pimpleton", "Bell", "Barnett", "Barnett", "Snyder", "Woodie", "Barnett",  "Snyder", "Barnett", "Barnett", "Woodie", "Snyder", "Pimpleton", "Lockette", "Lockette", "Frey", "Bell", "Pimpleton", "Haggins",  "Haggins", "Pimpleton", "Snyder", "Frey", "Haggins", "Snyder", "Bell", "Woodie", "Haggins", "Snyder", "Pimpleton", "Snyder",  "Barnett", "Barnett", "Snyder", "Haggins", "Barnett", "Bell", "Bell"), County = c("Alachua", "Baker", "Bay", "Bradford", "Brevard",  "Broward", "Calhoun", "Charlotte", "Citrus", "Clay", "Collier", "Columbia", "Desoto", "Dixie", "Duval", "Escambia", "Flagler", "Franklin", "Gadsen", "Gilchrist", "Glades", "Gulf", "Hamilton", "Hardee", "Hendry", "Hernando", "Highlands", "Hillsborough", "Holmes", "Indian River", "Jackson", "Jefferson", "Lafayette",   "Lake", "Lee", "Leon", "Levy", "Liberty", "Madison", "Manatee", "Marion", "Martin", "Miami-Dade", "Monroe", "Nassau", "Okeechobee",  "Oklaoosa", "Orange", "Osceola", "Palm Beach", "Pasco", "Pinellas",  "Polk", "Putnam", "Santa Rosa", "Sarasota", "Seminole", "St. Johns",  "St. Lucie", "Sumter", "Suwanee", "Taylor", "Union", "Volusia",  "Wakulla", "Walton", "Washington")), .Names = c("Coach", "County" ), class = "data.frame", row.names = c(NA, -67L))

counties$coach.county = coaches$County
counties$Coaches = coaches$Coach

pal <- colorFactor(
  palette = rainbow(9),
  domain = counties$Coaches)


leaflet(counties) %>%
  addProviderTiles(providers$Stamen.Toner) %>%
  addPolygons(color = ~pal(Coaches), weight = 1, fillOpacity = .7, label = ~coach.county, stroke = T) %>%
  addLegend("bottomleft", pal = pal, values = ~Coaches,
            opacity = .8
  ) %>%
  addLogo(img = "https://cdn.vox-cdn.com/uploads/blog/sbnu_logo/99/large_Tomahawk_Nation_Full.98121.png", 
          src = "remote", position = "topleft", width = 150, height = 120,
          offset.y = 10)
