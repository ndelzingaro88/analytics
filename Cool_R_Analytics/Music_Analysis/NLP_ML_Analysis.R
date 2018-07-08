#https://www.datacamp.com/community/tutorials/ML-NLP-lyric-analysis

library(tidytext) #text mining, unnesting
if(!require("topicmodels")) install.packages('topicmodels')
library(topicmodels) #the LDA algorithm
library(tidyr) #gather()
library(dplyr) #awesome tools
library(ggplot2) #visualization
library(kableExtra) #create attractive tables
library(knitr) #simple table generator
library(ggrepel) #text and label geoms for ggplot2
library(gridExtra)
library(formattable) #color tile and color bar in `kables`
library(tm) #text mining
library(circlize) #already loaded, but just being comprehensive
if(!require("plotly")) install.packages('plotly')
library(plotly) #interactive ggplot graphs

#define some colors to use throughout
my_colors <- c("#E69F00", "#56B4E9", "#009E73", "#CC79A7", "#D55E00", "#D65E00")

#customize ggplot2's default theme settings
#this tutorial doesn't actually pass any parameters, but you may use it again in future tutorials so it's nice to have the options
theme_lyrics <- function(aticks = element_blank(),
                         pgminor = element_blank(),
                         lt = element_blank(),
                         lp = "none")
{
  theme(plot.title = element_text(hjust = 0.5), #center the title
        axis.ticks = aticks, #set axis ticks to on or off
        panel.grid.minor = pgminor, #turn on or off the minor grid lines
        legend.title = lt, #turn on or off the legend title
        legend.position = lp) #turn on or off the legend
}

#customize the text tables for consistency using HTML formatting
my_kable_styling <- function(dat, caption) {
  kable(dat, "html", escape = FALSE, caption = caption) %>%
    kable_styling(bootstrap_options = c("striped", "condensed", "bordered"),
                  full_width = FALSE)
}

word_chart <- function(data, input, title) {
  data %>%
    #set y = 1 to just plot one variable and use word as the label
    ggplot(aes(as.factor(row), 1, label = input, fill = factor(topic) )) +
    #you want the words, not the points
    geom_point(color = "transparent") +
    #make sure the labels don't overlap
    geom_label_repel(nudge_x = .2,  
                     direction = "y",
                     box.padding = 0.1,
                     segment.color = "transparent",
                     size = 3) +
    facet_grid(~topic) +
    theme_lyrics() +
    theme(axis.text.y = element_blank(), axis.text.x = element_blank(),
          #axis.title.x = element_text(size = 9),
          panel.grid = element_blank(), panel.background = element_blank(),
          panel.border = element_rect("lightgray", fill = NA),
          strip.text.x = element_text(size = 9)) +
    labs(x = NULL, y = NULL, title = title) +
    #xlab(NULL) + ylab(NULL) +
    #ggtitle(title) +
    coord_flip()
}

#Get Tidy Prince Dataset and Balanced Tidy Dataset of All Sources and 3 Sources
#Had to Download
three_sources_tidy_balanced <- read.csv("three_sources_tidy_balanced.csv",
                                        stringsAsFactors = FALSE)

all_sources_tidy_balanced <- read.csv("all_sources_tidy_balanced.csv",
                                      stringsAsFactors = FALSE)

prince_tidy <- read.csv("prince_tidy.csv",
                        stringsAsFactors = FALSE)

#group the dataset by writer (source) and count the words
three_sources_tidy_balanced %>%
  group_by(source) %>%
  mutate(word_count = n()) %>%
  select(source, genre, word_count) %>% #only need these fields
  distinct() %>%
  ungroup() %>%
  #assign color bar for word_count that varies according to size
  #create static color for source and genre
  mutate(word_count = color_bar("lightpink")(word_count),  
         source = color_tile("lightblue","lightblue")(source),
         genre = color_tile("lightgreen","lightgreen")(genre)) %>%
  my_kable_styling("Three Sources Stats")


#Create Document-Term Matrix.. Amount of Docs and Terms 
three_sources_dtm_balanced <- three_sources_tidy_balanced %>%
  #get word count per document to pass to cast_dtm
  count(document, word, sort = TRUE) %>%
  ungroup() %>%
  #create a DTM with docs as rows and words as columns
  cast_dtm(document, word, n)
#examine the structure of the DTM
three_sources_dtm_balanced

###inspecting matrix
#look at 4 documents and 8 words of the DTM
inspect(three_sources_dtm_balanced[1:4,1:8])

##Set variables
#assign the source dataset to generic var names
#so we can use a generic function per model
source_dtm <- three_sources_dtm_balanced
source_tidy <- three_sources_tidy_balanced


###Fit Model
#the default is VEM, but in my experience, it 
#does not perform as well as the alternative GIBBS sampling method.
# GIBBS sampling is that it performs a random walk that starts at some point which 
#you can initialize (you'll just use the default of 0) and at each step moves plus or 
#minus one with equal probability.

k <- 3 #number of topics
seed = 1234 #necessary for reproducibility
#fit the model passing the parameters discussed above
#you could have more control parameters but will just use seed here
#four parameters: the input, the number of topics, the sampling method and the seed.
lda <- LDA(source_dtm, k = k, method = "GIBBS", control = list(seed = seed))
#examine the class of the LDA object
class(lda)

#convert the LDA object into a tidy format
#passing "beta" shows the word probabilities
#filter on the word iceberg as an example
#results show probability of iceberg for each topic
tidy(lda, matrix = "beta") %>% filter(term == "iceberg")

###identify top themes for words
num_words <- 10 #number of words to visualize

#create function that accepts the lda model and num word to display
top_terms_per_topic <- function(lda_model, num_words) {
  
  #tidy LDA object to get word, topic, and probability (beta)
  topics_tidy <- tidy(lda_model, matrix = "beta")
  

  top_terms <- topics_tidy %>%
    group_by(topic) %>%
    arrange(topic, desc(beta)) %>%
    #get the top num_words PER topic
    slice(seq_len(num_words)) %>%
    arrange(topic, beta) %>%
    #row is required for the word_chart() function
    mutate(row = row_number()) %>%
    ungroup() %>%
    #add the word Topic to the topic labels
    mutate(topic = paste("Topic", topic, sep = " "))
  #create a title to pass to word_chart
  title <- paste("LDA Top Terms for", k, "Topics")
  #call the word_chart function you built in prep work
  word_chart(top_terms, top_terms$term, title)
}
#call the function you just built!
top_terms_per_topic(lda, num_words)
#these topics clearly reflect the three sources of Icebergs, Machine Learning, and Prince!

#Classify Documents
#matrix = "gamma" and you'll get the per-document-per-topic probabilities.
#this time use gamma to look at the prob a doc is in a topic
#just look at the Prince song 1999 as an example
tidy(lda, matrix = "gamma") %>% filter(document == "1999")


#####Chord Diagram
#plot shows what percentage of docs from each source belongs to each topic
#using tidy with gamma gets document probabilities into topic
#but you only have document, topic and gamma
source_topic_relationship <- tidy(lda, matrix = "gamma") %>%
  #join to orig tidy data by doc to get the source field
  inner_join(three_sources_tidy_balanced, by = "document") %>%
  select(source, topic, gamma) %>%
  group_by(source, topic) %>%
  #get the avg doc gamma value per source/topic
  mutate(mean = mean(gamma)) %>%
  #remove the gamma value as you only need the mean
  select(-gamma) %>%
  #removing gamma created duplicates so remove them
  distinct()

#relabel topics to include the word Topic
source_topic_relationship$topic = paste("Topic", source_topic_relationship$topic, sep = " ")

circos.clear() #very important! Reset the circular layout parameters
#assign colors to the outside bars around the circle
grid.col = c("prince" = my_colors[1],
             "icebergs" = my_colors[2],
             "machine_learning" = my_colors[3],
             "Topic 1" = "grey", "Topic 2" = "grey", "Topic 3" = "grey")

# set the global parameters for the circular layout. Specifically the gap size (15)
#this also determines that topic goes on top half and source on bottom half
circos.par(gap.after = c(rep(5, length(unique(source_topic_relationship[[1]])) - 1), 15,
                         rep(5, length(unique(source_topic_relationship[[2]])) - 1), 15))
#main function that draws the diagram. transparancy goes from 0-1
chordDiagram(source_topic_relationship, grid.col = grid.col, transparency = .2)
title("Relationship Between Topic and Source")
#classified documents into Topics based on the mean of gamma for a topic/source

#Top Documents Per Topic
number_of_documents = 5 #number of top docs to view
title <- paste("LDA Top Documents for", k, "Topics")

#create tidy form showing topic, document and its gamma value
topics_tidy <- tidy(lda, matrix = "gamma")

#same process as used with the top words
top_documents <- topics_tidy %>%
  group_by(topic) %>%
  arrange(topic, desc(gamma)) %>%
  slice(seq_len(number_of_documents)) %>%
  arrange(topic, gamma) %>%
  mutate(row = row_number()) %>%
  ungroup() %>%
  #re-label topics
  mutate(topic = paste("Topic", topic, sep = " "))

title <- paste("LDA Top Documents for", k, "Topics")
word_chart(top_documents, top_documents$document, title)

#####Identify Artists/Authors#########
title <- paste("Sources for Top Documents for", k, "Topics")

topics_tidy <- tidy(lda, matrix = "gamma")

top_sources <- top_documents %>%
  #join back to the tidy form to get the source field
  inner_join(source_tidy) %>%
  select(document, source, topic) %>%
  distinct() %>%
  group_by(topic) %>%
  #needed by word_chart (not relevant here)
  mutate(row = row_number()) %>%
  ungroup()

word_chart(top_sources, top_sources$source, title)
#The top documents for each topic as shown in the previous chart are 
#replaced by their source. The code has done well at classifying these documents because they fell 
#into the same category


#####Set Variables and Fit The Model####
#use the same three sources you started with
source_dtm <- three_sources_dtm_balanced
source_tidy <- three_sources_tidy_balanced

#Set a seed for replicable results
set.seed(1234)
k <- 3
kmeansResult <- kmeans(source_dtm, k)
str(kmeansResult)

#see the contents of each variable for the song 1999 and the word party which appears in that song.
head(kmeansResult$cluster["1999"])
###Word
head(kmeansResult$centers[,"party"])


####Identify K-Means Themes with Top Words
# top words for each cluster and compare your results to the LDA output
num_words <- 8 #number of words to display
#get the top words from the kmeans centers
kmeans_topics <- lapply(1:k, function(i) {
  s <- sort(kmeansResult$centers[i, ], decreasing = T)
  names(s)[1:num_words]
})

#make sure it's a data frame
kmeans_topics_df <- as.data.frame(kmeans_topics)
#label the topics with the word Topic
names(kmeans_topics_df) <- paste("Topic", seq(1:k), sep = " ")
#create a sequential row id to use with gather()
kmeans_topics_df <- cbind(id = rownames(kmeans_topics_df),
                          kmeans_topics_df)
#transpose it into the format required for word_chart()
kmeans_top_terms <- kmeans_topics_df %>% gather(id, 1:k)
colnames(kmeans_top_terms) = c("topic", "term")

kmeans_top_terms <- kmeans_top_terms %>%
  group_by(topic) %>%
  mutate(row = row_number()) %>% #needed by word_chart()
  ungroup()

title <- paste("K-Means Top Terms for", k, "Topics")
word_chart(kmeans_top_terms, kmeans_top_terms$term, title)
#k-means algorithm is smart enough to separate topics between books and music. 
#In other words, Topics 1 and 3 are clearly Prince themes
#At a quick glance, with default parameters, k-means does not seem to perform as well as LDA, and in fact, 
#hard clustering is typically not used as much for topic modeling


####Model Three: LDA for 12 Writers#####
###Artists with writers###
all_sources_tidy_balanced %>%
  group_by(source) %>%
  #get the word count and doc count per source
  mutate(word_count = n(),
         source_document_count = n_distinct(document)) %>%
  select(source, genre, word_count, source_document_count) %>%
  distinct() %>%
  ungroup() %>%
  #bars change size according to number
  #tiles are static sizes
  mutate(word_count = color_bar("lightpink")(word_count),
         source_document_count = color_bar("lightpink")(source_document_count),
         source = color_tile("lightblue","lightblue")(source),
         genre = color_tile("lightgreen","lightgreen")(genre)) %>%
  my_kable_styling("All Sources Stats")

###Abreviating/correcting sources
all_sources_tidy_balanced <- all_sources_tidy_balanced %>%
  mutate(source = ifelse(source == "machine_learning", "m_learn",
                         ifelse(source == "machine_learning_r", "m_learn_r",
                                ifelse(source == "michael_jackson", "mi_jackson",
                                       ifelse(source == "sports_nutrition", "nutrition", source))))) %>%
  mutate(genre = ifelse(genre == "machine_learning", "m_learn",
                        ifelse(genre == "sports_nutrition", "nutrition", genre)))

#Create the DTM and set the variables
#this time use the dataset with 12 sources
all_sources_dtm_balanced <- all_sources_tidy_balanced %>%
  count(document, word, sort = TRUE) %>%
  ungroup() %>%
  cast_dtm(document, word, n)

source_dtm <- all_sources_dtm_balanced
source_tidy <- all_sources_tidy_balanced

k <- 8 #number of topics chosen to match the number of genres
num_words <- 10 #number of words we want to see in each topic
seed = 1234 #make it repeatable
#same as before
lda <- LDA(source_dtm, k = k, method = "GIBBS", control = list(seed = seed))

###top terms per topic
top_terms_per_topic(lda, num_words)

#This plot shows the average gamma value of documents from each source for each topic
source_topic_relationship <- tidy(lda, matrix = "gamma") %>%
  #join to the tidy form to get the genre field
  inner_join(source_tidy, by = "document") %>%
  select(genre, topic, gamma) %>%
  group_by(genre, topic) %>%
  #avg gamma (document) probability per genre/topic
  mutate(mean = mean(gamma)) %>%
  select(genre, topic, mean) %>%
  ungroup() %>%
  #re-label topics
  mutate(topic = paste("Topic", topic, sep = " ")) %>%
  distinct()

circos.clear() #very important! Reset the circular layout parameters
#this is the long form of grid.col just to show you what I'm doing
#you can also assign the genre names individual colors as well
grid.col = c("Topic 1" = "grey", "Topic 2" = "grey", "Topic 3" = "grey",
             "Topic 4" = "grey", "Topic 5" = "grey", "Topic 6" = "grey",
             "Topic 7" = "grey", "Topic 8" = "grey")

#set the gap size between top and bottom halves set gap size to 15
circos.par(gap.after = c(rep(5, length(unique(source_topic_relationship[[1]])) - 1), 15,
                         rep(5, length(unique(source_topic_relationship[[2]])) - 1), 15))
chordDiagram(source_topic_relationship,  grid.col = grid.col, annotationTrack = "grid",
             preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(source_topic_relationship))))))
#go back to the first track and customize sector labels
#use niceFacing to pivot the label names to be perpendicular
circos.track(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index,
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
}, bg.border = NA) # here set bg.border to NA is important
title("Relationship Between Topic and Genre")

#Recommend Similar Writers
# select the writer with the highest topic_sum for each topic using top_n(1)
#this function can be used to show genre and source via passing the "type"
top_items_per_topic <- function(lda_model, source_tidy, type) {
  #get the tidy version by passing gamma for the per document per topic probs
  document_lda_gamma <- tidy(lda_model, matrix = "gamma") %>%
    #join to the tidy form to get source and genre
    inner_join(source_tidy) %>%
    select(document, gamma, source, genre, topic) %>%
    distinct() %>% #remove duplicates
    #group so that you can get sum per topic/source
    group_by(source, topic) %>%
    #sort by decending gamma value
    arrange(desc(gamma)) %>%
    #create the sum of all document gamma vals per topic/source. Important!
    mutate(topic_sum = sum(gamma)) %>%
    select(topic, topic_sum, source, genre) %>%
    distinct() %>%
    ungroup() %>%
    #type will be either source or genre
    group_by(source, genre ) %>%
    #get the highest topic_sum per type
    top_n(1, topic_sum) %>%
    mutate(row = row_number()) %>%
    mutate(label = ifelse(type == "source", source, genre),
           title = ifelse(type == "source", "Recommended Writers Per Topic",
                          "Genres Per Topic")) %>%
    ungroup() %>%
    #re-label topics
    mutate(topic = paste("Topic", topic, sep = " ")) %>%
    select(label, topic, title)
  
  #slightly different format from word_chart input, so use this version
  document_lda_gamma %>%
    #use 1, 1, and label to use words without numeric values
    ggplot(aes(1, 1, label = label, fill = factor(topic) )) +
    #you want the words, not the points
    geom_point(color = "transparent") +
    #make sure the labels don't overlap
    geom_label_repel(nudge_x = .2,
                     direction = "y",
                     box.padding = 0.1,
                     segment.color = "transparent",
                     size = 3) +
    facet_grid(~topic) +
    theme_lyrics() +
    theme(axis.text.y = element_blank(), axis.text.x = element_blank(),
          axis.title.y = element_text(size = 4),
          panel.grid = element_blank(), panel.background = element_blank(),
          panel.border = element_rect("lightgray", fill = NA),
          strip.text.x = element_text(size = 9)) +
    xlab(NULL) + ylab(NULL) +
    ggtitle(document_lda_gamma$title) +
    coord_flip()
}

#by source
top_items_per_topic(lda, source_tidy, "source")
#Similar Documents by Genre
top_items_per_topic(lda, source_tidy, "genre")

#Model Four: LDA for Prince Songs
#Use NLP To Improve Model
#annotated form of the data resulting from a powerful NLP package called cleanNLP

#read in the provided annotated dataset
prince_annotated <- read.csv("prince_data_annotated.csv")
#look at the fields provided in the dataset
names(prince_annotated)

#lemma: the lemmatized word form (i.e. the reduced version of an inflected word to its base form)
#upos: the universal part of speech of each word

table(prince_annotated$upos)

prince_annotated %>%
  #most lemmas are the same as the raw word so ignore those
  filter((as.character(word) != as.character(lemma))
         & (id %in% c("broken", "1999"))) %>% #filter on 2 songs
  anti_join(stop_words) %>%
  select(song = id, word, lemma, upos) %>%
  distinct() %>%
  my_kable_styling("Annotated Subset")

#Set Variables Using NLP
source_tidy <- prince_annotated %>%
  select(document = id, word, lemma, upos) %>%
  filter(upos == "NOUN") %>% #choose only the nouns
  inner_join(prince_tidy, by = c("word", "document")) %>%
  select(document, word, lemma, upos, source, genre, year) %>%
  distinct()

source_dtm <- source_tidy %>%
  #filter out some words that exist across themes just for our purposes
  filter(!word %in% c("love", "time", "day", "night", "girl")) %>%
  count(document, word, sort = TRUE) %>%
  ungroup() %>%
  cast_dtm(document, word, n)

#Fit the Model and Identify Themes
#Changing these parameters or the source data will cause different results!!
k <- 7
num_words <- 6
seed = 4321
lda <- LDA(source_dtm, k = k, method = "GIBBS",
           
           control = list(seed = seed))

top_terms_per_topic(lda, num_words)

#Themes Over Time
p1 <-  prince_tidy %>%
  filter(!year == "NA") %>% #remove songs without years
  filter(word %in% c("music", "party", "dance")) %>%
  group_by(year) %>%
  mutate(topic_7_count = n()) %>%
  select(year, topic_7_count) %>%
  distinct() %>%
  ggplot(aes(year, topic_7_count)) + geom_smooth(se = FALSE, col = "red")

p2 <-  prince_tidy %>%
  filter(!year == "NA") %>% #remove songs without years
  filter(word %in% c("heaven","hand","soul")) %>%
  group_by(year) %>%
  mutate(topic_4_count = n()) %>%
  select(year, topic_4_count) %>%
  distinct() %>%
  ggplot(aes(year, topic_4_count)) + geom_smooth(se = FALSE)

grid.arrange(p1, p2, ncol = 2)
