#https://austinwehrwein.com/digital-humanities/creating-a-density-map-in-r-with-zipcodes/
if(!require("zipcode")) install.packages('zipcode')
library(zipcode)
library(tidyverse)
library(maps)
library(viridis)
if(!require("ggthemes")) install.packages('ggthemes')
library(ggthemes)
#devtools::install_github("hrbrmstr/albersusa")
#https://github.com/hrbrmstr/albersusa
library(albersusa)#installed via github
#path
setwd("~/git_dir/analytics/Cool_R_Analytics/zipcode_analytics/")
#data
fm<-Export <- read_csv("Export.csv")#the file we just downloaded
data(zipcode)
fm$zip<- clean.zipcodes(fm$zip)
#size by zip
fm.zip<-aggregate(data.frame(count=fm$FMID),list(zip=fm$zip,county=fm$County),length)
fm<- merge(fm.zip, zipcode, by='zip')

us<-map_data('state')

###Count density on the state
ggplot(fm,aes(longitude,latitude)) +
  geom_polygon(data=us,aes(x=long,y=lat,group=group),color='gray',fill=NA,alpha=.35)+
  geom_point(aes(color = count),size=.15,alpha=.25) +
  xlim(-125,-65)+ylim(20,50)

fm.counties<-aggregate(fm$count,by=list(fm$county,fm$state),sum)
names(fm.counties)[1:2]<-c('county','state')
cty_sf <- counties_sf("aeqd")
cty_sf$county<-as.character(cty_sf$name)
cty_sf$state<-as.character(cty_sf$iso_3166_2)
data.fm<-left_join(cty_sf,fm.counties,by=c('state','county'))
data.fm$x<-log(data.fm$x)
data.fm$x[is.na(data.fm$x)]<-0

data.fm %>%
  ggplot(aes(fill = x, color = x)) + 
  geom_sf() + 
  scale_fill_viridis(option = "B",direction=-1) + 
  scale_color_viridis(option = "B",direction=-1) +
  theme_map(base_size=11)
