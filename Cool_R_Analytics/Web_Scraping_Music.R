library(tidyverse)
library(rvest)
#https://towardsdatascience.com/learn-to-create-your-own-datasets-web-scraping-in-r-f934a31748a5

#Identify the url from where you want to extract data
base_url <- "https://www.billboard.com/charts/greatest-of-all-time-pop-songs-artists"
webpage <- read_html(base_url)

# Get the artist name
artist <- html_nodes(webpage, ".chart-row__artist")
artist <- as.character(html_text(artist))

# Get the artist rank
rank <- html_nodes(webpage, ".chart-row__rank")
rank <- as.numeric(html_text(rank))

# Save it to a tibble
top_artists <- tibble('Artist' = gsub("\n", "", artist),   #remove the \n character in the artist's name
                      'Rank' = rank) %>%
  filter(rank <= 10)

#Format the link to navigate to the artists genius webpage
genius_urls <- paste0("https://genius.com/artists/",top_artists$Artist)

#Initialize a tibble to store the results
artist_lyrics <- tibble()

# Outer loop to get the song links for each artist 
for (i in 1:10) {
  genius_page <- read_html(genius_urls[i])
  song_links <- html_nodes(genius_page, ".mini_card_grid-song a") %>%
    html_attr("href") 
  
  #Inner loop to get the Song Name and Lyrics from the Song Link    
  for (j in 1:10) {
    
    # Get lyrics
    lyrics_scraped <- read_html(song_links[j]) %>%
      html_nodes("div.lyrics p") %>%
      html_text()
    
    # Get song name
    song_name <- read_html(song_links[j]) %>%
      html_nodes("h1.header_with_cover_art-primary_info-title") %>%
      html_text()
    
    # Save the details to a tibble
    artist_lyrics <- rbind(artist_lyrics, tibble(Rank = top_artists$Rank[i],
                                                 Artist = top_artists$Artist[i],
                                                 Song = song_name,
                                                 Lyrics = lyrics_scraped ))
    
    # Insert a time lag - to prevent me from getting booted from the site :)
    Sys.sleep(10)
  }
} 

#Inspect the results
artist_lyrics