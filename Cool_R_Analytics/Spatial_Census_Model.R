if(!require("tidycensus")) install.packages('tidycensus')
require("tidycensus", quietly=TRUE)
if(!require("lwgeom")) install.packages('lwgeom')
require("lwgeom", quietly=TRUE)
require("sf", quietly=TRUE)
require("dplyr", quietly=TRUE)
require("ggplot2", quietly=TRUE)

census_api_key("1ea6749d38b38496d630bda0f44703e5fbf7957f")

#retrieve median housing values for Cook County
chicago<-get_acs(geography = "block group", variables = "B25077_001E",  state = "IL",county = "Cook County",year=2016,geometry = T) 

#retrieve median housing values for Suffolk County
boston<-get_acs(geography = "block group", variables = "B25077_001E",  state = "MA",county = "Suffolk County",year=2016,geometry = T)   

#city centers.. two sf objects withcity centers
chicago_cbd<-st_as_sf(x = read.table(text="-87.627800  41.881998"),
                      coords = c(1,2),
                      crs = "+proj=longlat +datum=WGS84")

boston_cbd<-st_as_sf(x = read.table(text="-71.057083  42.361145"),
                     coords = c(1,2),
                     crs = "+proj=longlat +datum=WGS84")
#everything in the same projection
chicago_cbd <-chicago_cbd %>% st_transform(st_crs(chicago) )
boston_cbd <-boston_cbd %>% st_transform(st_crs(boston) )


chicago$dist_CBD<-st_distance(chicago,chicago_cbd) #computes distance to CBD (in meters)
chicago$dist_CBD<-as.numeric(chicago$dist_CBD)*0.000621371 #change units to miles

boston$dist_CBD<-st_distance(boston,boston_cbd) #computes distance to CBD (in meters)
boston$dist_CBD<-as.numeric(boston$dist_CBD)*0.000621371 #change units to miles

#add city column
boston$City<-"Boston"
chicago$City<-"Chicago"

chicago<-chicago %>% filter(dist_CBD<=10)
chicago<-data.frame(chicago)
boston<-data.frame(boston)
#combine dataframes
dta<-rbind(chicago,boston)

ggplot(dta, aes(x=dist_CBD, y=estimate, color=City)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm) +  # Add linear regression line
  xlab("Distance to CBD (miles)") +
  ylab("Median Housing Prices ($)") +
  theme_bw()
  
#https://www.r-bloggers.com/a-simple-spatial-equilibrium-model-with-tidycensus/