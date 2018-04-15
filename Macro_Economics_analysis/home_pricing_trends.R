#####################################################################################
## Step 1: Load Libraries ##
#####################################################################################
library(tidyverse)
library(tidyquant)
library(geofacet)
library(viridis)
library(scales)
#if(!require("tweenr")) install.packages('tweenr')
#library(tweenr)
#if(!require("farver")) install.packages('farver')
#library(farver)
if (!require(devtools)) {
  install.packages("devtools")
}
devtools::install_github("thomasp85/tweenr")

library(animation)
library(dplyr)

#####################################################################################
## Step 2: go get data ##
## FHFA's ALL-Transactions House Price Index for US and states (NSA) **
#####################################################################################
df <- tq_get(c("USSTHPI",paste0(us_state_grid3$code,      # get state abbreviations
                                "STHPI")),                    # append STHPI
             get="economic.data",             # use FRED
             from="2000-01-01")               # go from 1990 forward

df %>% mutate(state=substr(symbol,1,2)        # create a state variable
) -> df


df %>% group_by(state) %>% 
  mutate(hpi=100*price/price[date=="2000-01-01"]) %>% # rebenchmark index to 100 in Q1 2000
  ungroup() %>% 
  map_if(is.character,as.factor) %>%   # tweenr will try to interpolate characters, but will ignore factors
  as.tibble() -> df
knitr::kable(head(df))


####Chart with just CA highlighted
dplyr::filter(df,state=="CA") %>% 
  ggplot(aes(x=date, y=hpi))+
  geom_line(data=df, aes(group=state),color="lightgray",alpha=0.5)+
  geom_line(size=1.1,color="royalblue") + 
  geom_line(data=dplyr::filter(df,state=="US"),color="black",linetype=2, alpha=0.85)+
  geom_text(data=dplyr::filter(df,state=="US" & date==max(df$date)), aes(label=state), nudge_y=0.01,fontface="bold",color="black",label="US")+
  geom_text(data=df %>% filter(state=="CA" & date==max(df$date)), aes(label=state), nudge_y=0.01,fontface="bold",color="royalblue")+
  # set axis labels
  scale_y_log10(breaks=c(100,150,200,250,300),limits=c(85,300),sec.axis=dup_axis())+
  labs(x="",y="House Price Index (2000 Q1=100, log scale NSA)",
       title="House price trends by state",
       subtitle=paste("Each gray line a state, highlighted state CA"),
       caption="@lenkiefer Source: U.S. Federal Housing Finance Agency, All-Transactions House Price Index for the United States [USSTHPI],\nretrieved from FRED, Federal Reserve Bank of St. Louis; https://fred.stlouisfed.org/series/USSTHPI, March 18, 2018.")+
  theme(plot.subtitle=element_text(face="italic",size=14),
        plot.title=element_text(color="royalblue",face="bold",size=18),
        plot.caption=element_text(hjust=0))




# tween states
# use keep_state to pause the animation for 10/15 frames

data <- dplyr::filter(df, state=="US") %>%
  keep_state(10) %>%
  tween_state(dplyr::filter(df, state=="CA"), 'linear', 10) %>%
  keep_state(15) %>%
  tween_state(dplyr::filter(df, state=="TX"), 'linear', 10) %>%
  keep_state(15) %>%
  tween_state(dplyr::filter(df, state=="FL"), 'linear', 10) %>%
  keep_state(15) %>%
  tween_state(dplyr::filter(df, state=="NY"), 'linear', 10) %>%
  keep_state(15) %>%
  tween_state(dplyr::filter(df, state=="OH"), 'linear', 10) %>%
  keep_state(15) %>%
  tween_state(filter(df, state=="US"), 'linear',10) %>%
  keep_state(10)

oopt = ani.options(interval = 1/10)
saveGIF({for (i in 1:max(data$.frame)) {
  df.plot<-dplyr::filter(data,.frame==i)
  p<-df.plot %>% 
    ggplot(aes(x=date, y=hpi))+
    geom_line(data=df, aes(group=state),color="lightgray",alpha=0.5)+
    geom_line(size=1.1,color="royalblue") + 
    geom_line(data=dplyr::filter(df,state=="US"),color="black",linetype=2, alpha=0.85)+
    geom_text(data=dplyr::filter(df,state=="US" & date==max(df.plot$date)), aes(label=state), nudge_y=0.01,fontface="bold",color="black",label="US")+
    geom_text(data=df.plot %>% filter(date==max(df.plot$date)), aes(label=state), nudge_y=0.01,fontface="bold",color="royalblue")+
    # set axis labels
    scale_y_log10(breaks=c(100,150,200,250,300),limits=c(85,300),sec.axis=dup_axis())+
    labs(x="",y="House Price Index (2000 Q1=100, log scale NSA)",
         title="House price trends by state",
         subtitle=paste("Each gray line a state, highlighted state",head(df.plot,1)$state),
         caption="@lenkiefer Source: U.S. Federal Housing Finance Agency, All-Transactions House Price Index for the United States [USSTHPI],\nretrieved from FRED, Federal Reserve Bank of St. Louis; https://fred.stlouisfed.org/series/USSTHPI, March 18, 2018.")+
    theme(plot.subtitle=element_text(face="italic",size=14),
          plot.title=element_text(color="royalblue",face="bold",size=18),
          plot.caption=element_text(hjust=0))
  print(p)
  
  print(paste0(i, " out of ",max(data$.frame)))
  
  ani.pause()}
},movie.name="YOURDIRECTORY/hpi1.gif",ani.width = 700, ani.height = 540) #replace YOURDIRECTORY with a place where you want to save the gif