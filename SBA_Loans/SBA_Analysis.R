rm(list=ls())

library(tidyquant)
library(tidyverse)
library(lubridate)
library(data.table)
library(ggridges)
library(extrafont)
library(dplyr)
library(scales)
library(ggplot2)
library(hrbrthemes)
#if(!require("animation")) install.packages('animation')
library(devtools)
#install_github("yihui/animation")
library(animation)
library(tweenr)
install.packages('openxlsx')
library("openxlsx")
setwd("/Users/ndelzingaro/Box/SBA_Loans/")

# TODO: These should all move to a separate package
export_theme <- theme_ipsum() +
  theme(text=element_text(family="Lato"),
        panel.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(color='#999999'),
        plot.title = element_text(size=15),
        axis.title = element_text(size=14),
        axis.text = element_text(size=12),
        axis.text.x = element_text(angle = 0, vjust = .5),
        axis.line = element_blank(),
        legend.text=element_text(size=12))

palette <- c('#006c91', '#222222', '#848484','#990033', '#ff9900', '#660066', '#8b572a', '#e00e7c', '#4fcbe5', '#8a76d4', '#7ed321', '#d7bf1e', '#5891d4', '#d48758', '#e36e74')

format_si <- function(...) {
  function(x) {
    limits <- c(1e-9,  1e-6,  1e-3,  1e0,   1e3,
                1e6,   1e9,   1e12,  1e15,  1e18,
                1e21,  1e24)
    prefix <- c("n",   "µ",   "m",   " ",   "K",
                "MM",   "B",   "T",   "P",   "E",
                "Z",   "Y")
    
    # Vector with array indices according to position in intervals
    i <- findInterval(abs(x), limits)
    
    # Set prefix to " " for very small values < 1e-24
    i <- ifelse(i==0, which(limits == 1e0), i)
    
    paste(format(round(x/limits[i], 1),
                 trim=TRUE, scientific=FALSE, ...),
          prefix[i], sep = '')
  }
}


format_month <- function(...) {
  function(x) {
    paste(month(x),"'",substr(year(x), 3, 4), sep = '')
  }
}

format_quarter <- function(...) {
  function(x) {
    paste('Q', quarter(x)," ",substr(year(x),3,4), sep = '')
  }
}

get_state_name <- function(x) {
  if(x %in% state.abb) {
    to_return <- tolower(state.name[grep(x, state.abb)])
  } else if (x %in% state.name) {
    to_return <- tolower(x)
  } else if (x == 'DC') {
    to_return <- 'district of columbia'
  } else if (x == 'US') {
    to_return <- 'United States'
  } else {
    to_return <- 'Invalid State Name'
  }
  to_return
}


x <- read.xlsx("FOIA - 7(a)(FY2010-Present).xlsx",sheet = 1,startRow=2,colNames=TRUE)
x2 <- read.xlsx("FOIA - 7(a)(FY2000-FY2009).xlsx",sheet = 1,startRow=2,colNames=TRUE)
x3 <- read.xlsx("FOIA - 7(a)(FY1991-FY1999).xlsx",sheet = 1,startRow=2,colNames=TRUE)

z <- read.xlsx("FOIA - 504 (FY1991-Present).xlsx",sheet = 1,startRow=2,colNames=TRUE)


sba7a_data<-rbind(x,x2,x3)
sba7a_data$NAICS_Code<-substr(trimws(sba7a_data$NaicsCode),1,2)
sba7a_data$ChargeOffDate<-as.Date(sba7a_data$ChargeOffDate, origin = "1899-12-30")
sba7a_data$ChargeoffMonth <- format(sba7a_data$ChargeOffDate,'%Y-%m-01')
sba7a_data$ApprovalDate<-as.Date(sba7a_data$ApprovalDate, origin = "1899-12-30")

NAICS<-read_csv("NAICS.csv",col_names = FALSE)
NAICS$X3<-NULL
colnames(NAICS) <- c("NAICS_Code", "NAICS_MAIN")

sba7a_data<-merge(sba7a_data,NAICS,by="NAICS_Code")

industry<- sba7a_data %>% group_by(NAICS_MAIN)%>%summarise(count=n(),
                                                                 charge_off_rate = sum(GrossChargeOffAmount,na.rm=TRUE)/sum(GrossApproval, na.rm=TRUE))
bus_type<- sba7a_data %>% group_by(BusinessType)%>%summarise(count=n(),
                                                           charge_off_rate = sum(GrossChargeOffAmount,na.rm=TRUE)/sum(GrossApproval, na.rm=TRUE))
chargeoff_time<- sba7a_data %>% group_by(ApprovalFiscalYear)%>%summarise(count=n(),
                                                             charge_off_rate = sum(GrossChargeOffAmount,na.rm=TRUE)/sum(GrossApproval, na.rm=TRUE))

p1 <- ggplot(chargeoff_time) +
  geom_bar(aes(x = ApprovalFiscalYear, y = charge_off_rate), stat = "identity", fill = palette[1]) +
 # scale_x_date(labels = format_month()) +
  scale_y_continuous(labels=percent) +
  labs(x='Origination Year',
       y='Charge-off Rate',
       title='Charge Off Rate by Origination Year') +
  export_theme + theme(axis.text.x = element_text(angle = 90, vjust = 0))  + theme(legend.position="top",legend.title = element_blank(),legend.text = element_text(size=8, face="bold"))

p1

library(zipcode)
library(tidyverse)
library(maps)
library(viridis)
library(ggthemes)
library(albersusa)
data(zipcode) ##zipcode dataset
sba7a_data$zip<- clean.zipcodes(sba7a_data$BorrZip)
fm.zip<-aggregate(data.frame(count=sba7a_data$BorrName),list(zip=sba7a_data$zip),length)
fm<- merge(fm.zip, zipcode, by='zip')
us<-map_data('state')

ggplot(fm,aes(longitude,latitude)) +
  geom_polygon(data=us,aes(x=long,y=lat,group=group),color='gray',fill=NA,alpha=.35)+
  geom_point(aes(color = count),size=.15,alpha=.25) +
  xlim(-125,-65)+ylim(20,50)+guides(fill = guide_colorbar(reverse = T, barheight = 1, barwidth = 10, ticks = F))+
  theme(axis.line = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        panel.background = element_blank(), 
        panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        plot.background = element_blank(),
        legend.position="none", #legend.box = "horizontal",
        strip.background = element_blank(),strip.text = element_text(size=14))

