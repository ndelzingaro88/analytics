rm(list=ls())
setwd("~/git_dir/analytics/")
source('~/git_dir/analytics/format/format.R')
#https://medium.freecodecamp.org/an-introduction-to-web-scraping-using-r-40284110c848

if(!require("selectr")) install.packages('selectr')
library(selectr)
if(!require("xml2")) install.packages('xml2')
library(xml2)
##Package for R scraping
if(!require("rvest")) install.packages('rvest')
library(rvest)
if(!require("stringr")) install.packages('stringr')
library(stringr)
if(!require("jsonlite")) install.packages('jsonlite')
library(jsonlite)


#Specifying the url for desired website to be scrapped
url <- "https://www.amazon.in/OnePlus-Mirror-Black-64GB-Memory/dp/B0756Z43QS?tag=googinhydr18418-21&tag=googinkenshoo-21&ascsubtag=aee9a916-6acd-4409-92ca-3bdbeb549f80"


#Reading the html content from Amazon
webpage <- read_html(url)
#In this code, we read the HTML content from the given URL, and assign that HTML into the webpage variable.
#Inspect button in Chrome

title_html <- html_nodes(webpage, "h1#title")
title <- html_text(title_html)                         
head(title)

# remove all space and new lines
str_replace_all(title, "[\r\n]" , "")

# scrape the price of the product
price_html <- html_nodes(webpage, "span#priceblock_ourprice")
price <- html_text(price_html)
# remove spaces and new line
str_replace_all(price, "[\r\n]" , "")
# print price value
head(price)

# scrape product description
desc_html <- html_nodes(webpage, "div#productDescription")
desc <- html_text(desc_html)
# replace new lines and spaces
desc <- str_replace_all(desc, "[\r\n\t]" , "")
desc <- str_trim(desc)
head(desc)


# scrape product rating 
rate_html <- html_nodes(webpage, "span#acrPopover")
rate <- html_text(rate_html)
# remove spaces and newlines and tabs 
rate <- str_replace_all(rate, "[\r\n]" , "")
rate <- str_trim(rate)
# print rating of the product
head(rate)

# Scrape size of the product
size_html <- html_nodes(webpage, "div#variation_size_name")
size_html <- html_nodes(size_html, "span.selection")
size <- html_text(size_html)
# remove tab from text
size <- str_trim(size)
# Print product size
head(size)

# Scrape product color
color_html <- html_nodes(webpage, "div#variation_color_name")
color_html <- html_nodes(color_html, "span.selection")
color <- html_text(color_html)
# remove tabs from text
color <- str_trim(color)
# print product color
head(color)

#Combining all the lists to form a data frame
product_data <- data.frame(Title = title,Description = desc, Rating = rate, Size = size, Color = color)
#Structure of the data frame
str(product_data)                           

#jsonlite package
# convert dataframe into JSON format
json_data <- toJSON(product_data)
# print output
cat(json_data)
