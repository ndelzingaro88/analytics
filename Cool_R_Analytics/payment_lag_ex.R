rm(list=ls())
#setwd("/Users/ndelzingaro/Box/Orchard_Data/")
#source('~/git_dir/risk_reporting/format/format.R')
library(data.table)
example <- read.csv('test.csv')
example <- data.table(example)

data_p<-example[, np_day:=unlist(lapply(rle(status > 0 )$lengths, function(x) seq(1:x) )), by="Load_ID"]
data_p$np_day <- ifelse(data_p$status==0, 0,data_p$np_day)

write.csv(data_p,'finish.csv')
