if(!require("RPostgreSQL")) install.packages('RPostgreSQL')
library(RPostgreSQL)
if(!require("dplyr")) install.packages('dplyr')
library(dplyr)
if(!require("RJDBC")) install.packages('RJDBC')
library(RJDBC)
if(!require("ggplot2")) install.packages('ggplot2')
library(ggplot2)
if(!require("hrbrthemes")) install.packages('hrbrthemes')
library(hrbrthemes)
if(!require("scales")) install.packages('scales')
library(scales)
if(!require("reshape")) install.packages('reshape')
library(reshape)
if(!require("data.table")) install.packages('data.table')
library(data.table)
if(!require("ggrepel")) install.packages('ggrepel')
library(ggrepel)
if(!require("Hmisc")) install.packages('Hmisc')
library(Hmisc)
if(!require("zoo")) install.packages('zoo')
library(zoo)
if(!require("lubridate")) install.packages('lubridate')
library(lubridate)
if(!require("xlsx")) install.packages('xlsx')
library(xlsx)
if(!require("XML")) install.packages('XML')
library(XML)
if(!require("gridExtra")) install.packages('gridExtra')
library(gridExtra)
###Reporting Functions
export_theme <- theme_ipsum() +
  theme(text=element_text(family="Lato"),
        panel.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(color='#999999'),
        plot.title = element_text(size=15),
        axis.title = element_text(size=14),
        axis.text = element_text(size=12),
        axis.text.x = element_text(angle = 90, vjust = .5),
        axis.line = element_blank(),
        legend.text=element_text(size=12))

orchard_palette <- c('#006c91', '#222222', '#848484','#990033', '#ff9900', '#660066', '#8b572a', '#e00e7c', '#4fcbe5', '#8a76d4', '#7ed321', '#d7bf1e', '#5891d4', '#d48758', '#e36e74')

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
