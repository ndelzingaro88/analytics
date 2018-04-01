#=============
# LOAD PACKGES
#=============

library(tidyverse)
library(rvest)
library(stringr)
library(dplyr)


#=============
# READ WEBPAGE
#=============
html.public_debt_by_country <- read_html('https://en.wikipedia.org/wiki/List_of_countries_by_public_debt')


#==================
# SCRAPE TABLE DATA
#==================
df.public_debt_by_country <- html.public_debt_by_country %>% 
  html_nodes('table') %>% 
  .[[1]] %>% 
  html_table()

#===============
# RENAME COLUMNS
#===============
names(df.public_debt_by_country)

colnames(df.public_debt_by_country) <- c('country'
                                         ,'debt_as_pct_of_gdp'
                                         , 'measure_year'
                                         , 'gross_debt_as_pct_of_gdp_IMF'
                                         , 'net_debt_as_pct_of_gdp_IMF'
                                         , 'measure_year_IMF'
                                         , 'region'
)


#========================================================
# DROP EXTRA COLUMNS
# - The Wikipedia table has a couple of different sources
#   for very similar data.
# - We will drop the IMF data
#========================================================

df.public_debt_by_country %>% 
  select(-gross_debt_as_pct_of_gdp_IMF
         , -net_debt_as_pct_of_gdp_IMF
         , -measure_year_IMF
  ) ->
  df.public_debt_by_country


#=====================================================
# REMOVE RECORD FOR 'World'
# - The original data has a summary record for 'World'
# - we will remove this
#=====================================================
df.public_debt_by_country <- df.public_debt_by_country %>% filter(country != 'World')


#============================
# COERCE data.frame TO tibble
#============================
df.public_debt_by_country <-  df.public_debt_by_country %>% as_tibble()


#=============
# GET MAP DATA
#=============
df.map <- map_data('world')


# INSPECT map data
df.map %>% glimpse()
df.map %>% names()


# INSPECT debt data
df.public_debt_by_country %>% glimpse()


#=============================
# RENAME 'region' to 'country'
#=============================
colnames(df.map)[colnames(df.map)=="region"] <- "country"


df.map %>% glimpse()
 
#===============================
# IDENTIFY JOIN MISMATCHES
# - we will use an anti-join to 
#   identify mis-matches between
#   the country variable on our 
#   two different datasets
#===============================
anti_join(df.public_debt_by_country
          ,df.map
          ,by = 'country'
) 

#============================================
# GET COUNTRY NAMES FROM df.map
# - these will be the new names that we will
#   use when we re-code the names in 
#   df.public_debt_by_country
#============================================
df.map %>% 
  group_by(country) %>% 
  summarise() %>% 
  print(n = Inf)


# RECODE
df.public_debt_by_country %>% 
  mutate(country = recode(country
                          ,`Antigua and Barbuda` = 'Antigua'
                          ,`Burma` = 'Myanmar'
                          ,`People's Republic of China` = 'China'
                          ,`Congo, Democratic Republic of the` = 'Democratic Republic of the Congo'
                          ,`Congo, Republic of the` = 'Republic of Congo'
                          ,`Cote d'Ivoire` = 'Ivory Coast'
                          ,`Gambia, The` = 'Gambia'
                          #,`Gibraltar` = ''
                          #,`Hong Kong` = ''
                          ,`Korea, North` = 'North Korea'
                          ,`Korea, South` = 'South Korea'
                          ,`Saint Kitts and Nevis` = 'Saint Kitts'
                          ,`Saint Vincent and the Grenadines` = 'Saint Vincent'
                          ,`Trinidad and Tobago` = 'Trinidad'
                          #,`Tuvalu` = ''
                          ,`United Kingdom` = 'UK'
                          ,`United States` = 'USA'
  )
  ) ->
  df.public_debt_by_country

#===========================
# RE-INSPECT JOIN MISMATCHES
# note: these last 3 are OK
#===========================
anti_join(df.public_debt_by_country
          ,df.map
          ,by = 'country'
)

#=====
# JOIN
#=====
df.map_public_debt <- left_join(df.map
                                ,df.public_debt_by_country
                                ,by = 'country'
)


# INSPECT
df.map_public_debt %>% glimpse()

#============================
# PLOT
# - this is just a basic plot
#============================
df.map_public_debt %>% 
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = debt_as_pct_of_gdp))


#=============
# CREATE THEME
#=============
theme.map <- theme(
  text = element_text(family = 'Helvetica Neue', color = '#444444')
  ,panel.background = element_rect(fill = '#CCCCCC')
  ,plot.background = element_rect(fill = '#CCCCCC')
  ,legend.background = element_rect(fill = '#CCCCCC')
  ,panel.grid = element_blank()
  ,plot.title = element_text(size = 18, face = 'bold')
  ,plot.subtitle = element_text(size = 12)
  ,legend.key = element_blank()
  ,axis.text = element_blank()
  ,axis.ticks = element_blank()
  ,axis.title = element_blank()
)


#========================
# CREATE WORLD PLOT / MAP
#========================
plot.debt_to_gdp_map <- df.map_public_debt %>% 
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = debt_as_pct_of_gdp)) +
  theme.map +
  labs(title = str_c('Countries in the developed world are'
                     ,'\ncarrying high levels of debt compared to GDP'
  )
  ,fill = str_c('Net public debt','\nas a % of GDP')
  ) +
  scale_fill_gradientn(colors = c('#009933', '#ffff00', 'orange', '#e60000')
                       ,values = scales::rescale(c(30, 50, 70, 100, 200))
  )



#First, we’ll plot the entire map of the world.

#------
# WORLD
#------
plot.debt_to_gdp_map

#-------
# EUROPE
#-------
plot.debt_to_gdp_map + 
  coord_cartesian(xlim = c(-15, 50), ylim = c(30, 75)) +
  labs(title = "European countries have high levels of public debt"
       ,subtitle = str_c('In particular, countries in southern Europe - including Portugal, Italy,'
                         ,'\nGreece, and Spain - have high levels of public debt.'
       )
  )

#-------
# North America
#-------
plot.debt_to_gdp_map + 
  coord_cartesian(xlim = c(-170, -50), ylim = c(10, 80)) +
  labs(title = "North American levels of public debt"
       ,subtitle = str_c('Canada and Jamaica have high levels of public debt.'
       )
  )