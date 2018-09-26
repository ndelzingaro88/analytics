if(!require("data.table")) install.packages('data.table')
library(data.table)
if(!require("dplyr")) install.packages('dplyr')
library(dplyr)
if(!require("formattable")) install.packages('formattable')
library(formattable)
if(!require("tidyr")) install.packages('tidyr')
library(tidyr)


customGreen0 = "#DeF7E9"
customGreen = "#71CA97"
customRed = "#ff7f7f"

#Download the Austin indicator data set

austinData= fread('https://raw.githubusercontent.com/lgellis/MiscTutorial/master/Austin/Imagine_Austin_Indicators.csv', data.table=FALSE, header = TRUE, stringsAsFactors = FALSE)

head(austinData)
attach(austinData)


i1 <- austinData %>%
  filter(`Indicator Name` %in% 
           c('Prevalence of Obesity', 'Prevalence of Tobacco Use', 'Prevalence of Cardiovascular Disease', 'Prevalence of Diabetes')) %>%
          select(c(`Indicator Name`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`)) %>%mutate (Average = round(rowMeans(
            cbind(`2011`, `2012`, `2013`, `2014`, `2015`, `2016`), na.rm=T),2), 
            `Improvement` = round((`2011`-`2016`)/`2011`*100,2))

i1

#0) Throw it in the formattable function
formattable(i1)


#1)  First Data Table
formattable(i1, 
            align =c("l","c","c","c","c", "c", "c", "c", "r"), 
            list(`Indicator Name` = formatter(
              "span", style = ~ style(color = "grey",font.weight = "bold"))))


#2) Add the color mapping for all 2011 to 2016.
formattable(i1, align =c("l","c","c","c","c", "c", "c", "c", "r"), list(
  `Indicator Name` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")), `2011`= color_tile(customGreen, customGreen0),
  `2012`= color_tile(customGreen, customGreen0),
  `2013`= color_tile(customGreen, customGreen0),
  `2014`= color_tile(customGreen, customGreen0),
  `2015`= color_tile(customGreen, customGreen0),
  `2016`= color_tile(customGreen, customGreen0)
))


#3) Add the color bar to the average column
formattable(i1, align =c("l","c","c","c","c", "c", "c", "c", "r"), list(
  `Indicator Name` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")),
  `2011`= color_tile(customGreen, customGreen0),
  `2012`= color_tile(customGreen, customGreen0),
  `2013`= color_tile(customGreen, customGreen0),
  `2014`= color_tile(customGreen, customGreen0),
  `2015`= color_tile(customGreen, customGreen0),
  `2016`= color_tile(customGreen, customGreen0),
  `Average` = color_bar(customRed)
))

#4) Add sign formatter to improvement over time
improvement_formatter <- 
  formatter("span", 
            style = x ~ style(
              font.weight = "bold", 
              color = ifelse(x > 0, customGreen, ifelse(x < 0, customRed, "black"))))

formattable(i1, align =c("l","c","c","c","c", "c", "c", "c", "r"), list(
  `Indicator Name` = 
    formatter("span", style = ~ style(color = "grey",font.weight = "bold")), 
  `2011`= color_tile(customGreen, customGreen0),
  `2012`= color_tile(customGreen, customGreen0),
  `2013`= color_tile(customGreen, customGreen0),
  `2014`= color_tile(customGreen, customGreen0),
  `2015`= color_tile(customGreen, customGreen0),
  `2016`= color_tile(customGreen, customGreen0),
  `Average` = color_bar(customRed),
  `Improvement` = improvement_formatter
))


#5) For improvement formatter add icons
# Up and down arrow with greater than comparison from the vignette
improvement_formatter <- formatter("span", 
                                   style = x ~ style(font.weight = "bold", 
                                                     color = ifelse(x > 0, customGreen, ifelse(x < 0, customRed, "black"))),
                                   x ~ icontext(ifelse(x>0, "arrow-up", "arrow-down"), x))
                                   
formattable(i1, align =c("l","c","c","c","c", "c", "c", "c", "r"), list(
 `Indicator Name` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")),
 `2011`= color_tile(customGreen, customGreen0),
 `2012`= color_tile(customGreen, customGreen0),
 `2013`= color_tile(customGreen, customGreen0),
 `2014`= color_tile(customGreen, customGreen0),
 `2015`= color_tile(customGreen, customGreen0),
 `2016`= color_tile(customGreen, customGreen0),
 `Average` = color_bar(customRed),
 `Improvement` = improvement_formatter
))
 
 

#6) Add a star to the max value.  Use  if/else value = max(value)
improvement_formatter <- formatter("span", 
                                   style = x ~ style(font.weight = "bold",
                                   color = ifelse(x > 0, customGreen, ifelse(x < 0, customRed, "black"))),
                                   x ~ icontext(ifelse(x == max(x), "thumbs-up", ""), x))


## Based on Name
formattable(i1, align =c("l","c","c","c","c", "c", "c", "c", "r"), list(
  `Indicator Name` = formatter("span",
                               style = x ~ style(color = "gray"),
                               x ~ icontext(ifelse(x == "Prevalence of Tobacco Use", "star", ""), x)),
  `2011`= color_tile(customGreen, customGreen0),
  `2012`= color_tile(customGreen, customGreen0),
  `2013`= color_tile(customGreen, customGreen0),
  `2014`= color_tile(customGreen, customGreen0),
  `2015`= color_tile(customGreen, customGreen0),
  `2016`= color_tile(customGreen, customGreen0),
  `Average` = color_bar(customRed),
  `Improvement` = improvement_formatter
))

##7)  Compare column to column
#Drop the rest and show just 2015 and 2016
i2 <- austinData %>%
  filter(`Indicator Name` %in% c('Prevalence of Obesity', 'Prevalence of Tobacco Use', 'Prevalence of Cardiovascular Disease', 'Prevalence of Diabetes')) %>%
  select(c(`Indicator Name`, `2015`, `2016`)) 
head(i2)
formattable(i2, align =c("l","c","c"), list(
  `Indicator Name` = formatter("span",
                               style = ~ style(color = "gray")), 
  `2016`= formatter("span", style = ~ style(color = ifelse(`2016` >`2015`, "red", "green")),
                    ~ icontext(ifelse(`2016` >`2015`,"arrow-up", "arrow-down"), `2016`))))

#https://www.littlemissdata.com/blog/prettytables






 
              