### Load libraries
library(tidyverse)
library(quantmod)
library(data.table)
library(viridis)  # for the colors
if(!require("htmlTable")) install.packages('htmlTable')
library(htmlTable)  # for the table

#  Here are some variables and a brief description, for more see FRED

my.names <- data.table(var=c("A229RC0","CUSR0000SA0L2","CUUR0000SEHA","HPIPONM226S"),
                       name=c("income","cpi.less.shelter","cpi.rent","hpi"),
                       Description=c("Per capita disposable income",
                                     "CPI-U All items less shelter",
                                     "CPI-U Rent of primary residences",
                                     "FHFA Purchase-only house price index"),
                       Source=c("U.S. Bureau of Economic Analysis",
                                "U.S. Bureau of Labor Statistics",
                                "U.S. Bureau of Labor Statistics",
                                "Federal Housing Finance Agency (FHFA)"))

htmlTable(my.names, caption="Data description",tfoot="Accessed via: St Louis Federal Reserve Economic Database (FRED)")

# Use Quantmod to load data
# helpful reference https://jeffreybreen.wordpress.com/tag/quantmod/

df= getSymbols('CUUR0000SEHA',src='FRED', auto.assign=F) 

df = data.frame(date=time(df), coredata(df) )

df.hpi =getSymbols('HPIPONM226S',src='FRED', auto.assign=F) 
df.hpi = data.frame(date=time(df.hpi), coredata(df.hpi) )

df.inc=getSymbols('A229RC0',src='FRED', auto.assign=F) 
df.inc = data.frame(date=time(df.inc), coredata(df.inc) )

df2= getSymbols('CUSR0000SA0L2',src='FRED', auto.assign=F) 
df2 = data.frame(date=time(df2), coredata(df2) )

# merge the data
df3<-merge(df,df.hpi,by="date")
df3<-merge(df3,df.inc,by="date")
df3<-merge(df3,df2,by="date")
dt<-data.table(df3)

# Gather the data to make it tidy

dt %>% gather(var,value,-date) %>% data.table() ->dt2

# Merge on variable names

dt2<-merge(dt2,my.names,by="var")

# Create and index with January 1991 = 100
# Count by var
dt2=dt2[,id:=1:.N, by=var]  # Index running from 1:N by group (var)
dt2=dt2[,var0:=100*value/sum(ifelse(id==1,value,0)),by=var] #create index

# Create caption
mycaption<- "@lenkiefer Source: House Prices: FHFA purchase-only house price index. Rent: U.S. Bureau of Labor Statistics (BLS) consumer price index rent of primary residence. Other Prices: BLS consumer price index all items less shelter. Income: U.S. Bureau of Economic Analysis per capita disposable personal income (Table 2.6.). All are seasonally adjusted."

# Wrap caption 120 characters:
mycaption <- paste0(strwrap(mycaption, 120), sep="", collapse="\n")


# Create Plot
ggplot(data=dt2,aes(x=date,y=var0,color=name,linetype=name))+
  geom_line(size=1.1)+
  scale_y_log10(breaks=c(100,125,150,175,200,250))+
  theme_minimal()+theme(plot.caption=element_text(hjust=0),
                        legend.position="top")+
  guides(linetype=F)+
  scale_color_viridis(name="Variable",discrete=T,end=0.8)+
  labs(x="",y="Index, January 1991=100 (log scale)",
       title="Comparing house prices to rent, income and other prices",
       caption=mycaption       )