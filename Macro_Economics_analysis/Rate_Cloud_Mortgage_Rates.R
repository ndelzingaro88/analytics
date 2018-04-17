#####################################################################################
## Load libraries ----  ##
#####################################################################################
library(tidyverse)
library(tidyquant)
library(ggridges)
library(animation) # for the animated gif

#####################################################################################
## Get mortgage data ---- ##
# Can also get direct here: http://www.freddiemac.com/pmms/docs/historicalweeklydata.xls
#####################################################################################

df<- tq_get("MORTGAGE30US",get="economic.data",from="1971-04-01")
df$year<-year(df$date)
knitr::kable(tail(df))

#####################################################################################
## Make Color Scale ----  ##
#####################################################################################
my_colors <- c(
  "green"      = rgb(103,180,75, maxColorValue = 256),
  "green2"      = rgb(147,198,44, maxColorValue = 256),
  "lightblue"  =  rgb(9, 177,240, maxColorValue = 256),
  "lightblue2" = rgb(173,216,230, maxColorValue = 256),
  'blue'       = "#00aedb",
  'red'        = "#d11141",
  'orange'     = "#f37735",
  'yellow'     = "#ffc425",
  'gold'       = "#FFD700",
  'light grey' = "#cccccc",
  'dark grey'  = "#8c8c8c")


my_cols <- function(...) {
  cols <- c(...)
  if (is.null(cols))
    return (my_colors)
  my_colors[cols]
}


my_palettes <- list(
  `main`  = my_cols("blue", "green", "yellow"),
  `cool`  = my_cols("blue", "green"),
  `hot`   = my_cols("yellow", "orange", "red"),
  `mixed` = my_cols("lightblue", "green", "yellow", "orange", "red"),
  `mixed2` = my_cols("lightblue2","lightblue", "green", "green2","yellow","gold", "orange", "red"),
  `mixed3` = my_cols("lightblue2","lightblue", "green", "yellow","gold", "orange", "red"),
  `grey`  = my_cols("light grey", "dark grey")
)

my_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- my_palettes[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  colorRampPalette(pal, ...)
}


scale_color_mycol <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- my_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("colour", paste0("my_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}



scale_fill_mycol <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- my_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("fill", paste0("my_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}

#####################################################################################
## Static plot ----  ##
#####################################################################################
# filter the data and make some labels
dfp<- filter(df, year >2013) %>% 
  select(date,price) %>% 
  # create labels and a factor variables
  mutate(rate_label=as.factor(as.character(round(price,2))), 
         yearf=factor(year(date)),
         year=year(date))

#Create a list of dates
dlist<-unique(dfp$date)
# max length

N<-length(dlist)
# @Clauswilke on fixing axis for animation: https://twitter.com/ClausWilke/status/969774047729999872 

make_plot_appear <- function(i=25){
  plot_data <- filter(dfp, date<=dlist[i]) 
  
  p<- plot_data %>% 
    
    ggplot(aes(y=forcats::fct_reorder(yearf,-year),x=price,color=price,fill=..x..))+
    geom_density_ridges(data=dfp,color=NA, fill=NA)+
    scale_fill_mycol(palette="grey",name="30-year fixed mortgage rate (%)",discrete=F)+
    geom_density_ridges_gradient(rel_min_height=0.01,alpha=0.75,point_color="royalblue",scale=0.9,
                                 jittered_points = TRUE, position = "raincloud")  +
    guides(color=F,fill=F)+
    theme_ridges()+
    theme(legend.position="top",
          plot.caption=element_text(hjust=0),
          legend.key.width=unit(1.25,"cm"))+
    labs(x="30-year fixed mortgage rate",y="Year",
         title="Distribution of U.S. average weekly mortgage rates",
         subtitle=paste("through ",as.character(as.Date(max(plot_data$date)), format="%B %d, %Y")),
         caption="@lenkiefer Source: Freddie Mac Primary Mortgage Market Survey")
    #scale_y_discrete(expand = expand_scale(mult = 0, add = c(0.5, 0.25)))  # drop this if you don't have ggplot 2.2.1.9000 (Development as of 4/12/2018)
  
  return(p)
}    

make_plot_appear(N)

oopt<-ani.options(interval=1/10)
suppressMessages(  # if you want to supress ggridges messages during aniamtion loop
  saveGIF({for (i in 54:N){
    #  saveGIF({for (i in 53:75){
    g<-make_plot_appear(i)
    print(g)
    print(paste(i,"out of",N)) # a counter to know when animation loop may end
    ani.pause()
  }
    for (ii in 1:20){
      g<- make_plot_appear(N)
      print(g)
      ani.pause()
    }
  }, movie.name = "/Users/nicholasdelzingaro/git_dir/analytics/rate_cloud.gif",  ani.width=840, ani.height=1000)  # replace YOURDIRECTORY with a path to where you want to save gif
)