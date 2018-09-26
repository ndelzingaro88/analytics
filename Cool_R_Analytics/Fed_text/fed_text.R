# load libraries ----
#http://lenkiefer.com/2018/07/28/text-mining-fedspeak/
suppressPackageStartupMessages({
  library(extrafont)
  library(ggraph)
  library(ggridges)
  library(pdftools)
  library(tidyverse)
  library(tidytext)
  library(forcats)
  library(reshape2)
  library(tidyr)
  library(igraph)
  library(widyr)
  library(viridis)}
  
)


setwd("~/git_dir/analytics/Cool_R_Analytics/Fed_text/")

fed_import1 <- pdf_text("https://www.federalreserve.gov/monetarypolicy/files/20180713_mprfullreport.pdf")
str(fed_import1)

substr(fed_import1[7],1,500)

fed_text_raw <- 
  data.frame(text=unlist(strsplit(fed_import1,"\r"))) %>% 
  mutate(report="July2018", 
         line=row_number(),
         text=gsub("\n","",text))
head(fed_text_raw)

fed_text <- 
  fed_text_raw %>% 
  as_tibble() %>%
  unnest_tokens(word,text)
fed_text

#count the words
fed_text  %>%
  count(word, sort = TRUE) 

#anti_join to remove stop words
fed_text  %>%
  anti_join(stop_words)%>%
  count(word, sort = TRUE) 
