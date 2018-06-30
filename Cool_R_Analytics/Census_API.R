#https://cran.r-project.org/web/packages/censusapi/vignettes/getting-started.html

# Add key to .Renviron
Sys.setenv(CENSUS_KEY="1ea6749d38b38496d630bda0f44703e5fbf7957f")
if(!require("censusapi")) install.packages('censusapi')
library(censusapi)

apis <- listCensusApis()
View(apis)

#variables avaiable in the SAHIE API
sahie_vars <- listCensusMetadata(name = "timeseries/healthins/sahie", type = "variables")
head(sahie_vars)

listCensusMetadata(name = "timeseries/healthins/sahie", type = "geography")

#IPRCAT: Income Poverty Ratio Category
#IPR_DESC: Income Poverty Ratio Category Description
#PCTUI_PT: Percent Uninsured in Demographic Group for Selected Income Range, Estimate
#NAME: Name of the geography returned (e.g. state or county name)
getCensus(name = "timeseries/healthins/sahie",
          vars = c("NAME", "IPRCAT", "IPR_DESC", "PCTUI_PT"), 
          region = "us:*", time = 2015)

sahie_states <- getCensus(name = "timeseries/healthins/sahie",
                          vars = c("NAME", "IPRCAT", "IPR_DESC", "PCTUI_PT"), 
                          region = "state:*", time = 2015)

sahie_counties <- getCensus(name = "timeseries/healthins/sahie",
                            vars = c("NAME", "IPRCAT", "IPR_DESC", "PCTUI_PT"), 
                            region = "county:*", regionin = "state:1,2", time = 2015)
head(sahie_counties, n=12L)

#2016 County Business Patterns - Zip Code Business Patterns: Total For Zip Code
zbp_vars <- listCensusMetadata(name = "zbp", vintage = 2016, type = "variables")

sahie_counties <- getCensus(name = "zbp",
                            vars = c("NAME", "IPRCAT", "IPR_DESC", "PCTUI_PT"), 
                            region = "county:*", regionin = "state:1,2", time = 2015)
