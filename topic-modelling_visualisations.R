
# Visualising the topic modelling results from topic modelling.py script in R #

# required libraries 

library(tidytext)
library(tm)
require(wordcloud2)
library(plotrix)
library(ggthemes)
library(RWeka)
library(mapview)
require(htmlwidgets)

# importing the Py DF
combi_courses_cleaned <- read.csv("combi_courses_topic_modelling_results.csv",header = T)

# Additional text cleaning to ensure topic consistency so they can be better grouped for wordcloud later on 
combi_courses_cleaned <- combi_courses_cleaned %>% 
    mutate(theories_cleaned = ifelse(is.na(theories_cleaned), "", theories_cleaned),
         focus_lec_cleaned = str_replace_all(focus_lec_cleaned, "\\bethic\\w*", "ethics"),  
         focus_lec_cleaned = str_replace_all(focus_lec_cleaned, "\\bcommunit\\w*", "community"),
         theories_cleaned = str_replace_all(theories_cleaned, "\\bethic\\w*", "ethics"),
         theories_cleaned = str_replace_all(theories_cleaned, "\\bcommunit\\w*", "community"),
         concepts_cleaned = str_replace_all(concepts_cleaned, "\\bethic\\w*", "ethics"),
         concepts_cleaned = str_replace_all(concepts_cleaned, "\\bcommunit\\w*", "community"),
         focus_lec_cleaned = str_replace_all(focus_lec_cleaned,"(?i)\\b(man|mens|womens|woman|child)\\b", # bulk replacement
                                             function(x) {switch(x,
                                                                 "man" = "men",
                                                                 "mens" = "men",
                                                                 "womens" = "women",
                                                                 "woman" = "women",
                                                                 "child" = "children")
                                             }),
         theories_cleaned =  str_replace_all(theories_cleaned,"\\b(man|mens|womens|woman|child)\\b", # bulk replacement
                                             function(x) {switch(x,
                                                                 "man" = "men",
                                                                 "mens" = "men",
                                                                 "womens" = "women",
                                                                 "woman" = "women",
                                                                 "child" = "children")
                                             }),
         concepts_cleaned =  str_replace_all(concepts_cleaned ,"\\b(man|mens|womens|woman|child)\\b", # bulk replacement
                                             function(x) {switch(x,
                                                                 "man" = "men",
                                                                 "mens" = "men",
                                                                 "womens" = "women",
                                                                 "woman" = "women",
                                                                 "child" = "children")
                                             }),
         focus_lec_cleaned = gsub("\\b(\\S+)\\s+(?=.*\\b\\1\\b)", "", focus_lec_cleaned, perl=TRUE), # checks whether the word appears later in the string to ensure duplicates are removed
         theories_cleaned = gsub("\\b(\\S+)\\s+(?=.*\\b\\1\\b)", "", theories_cleaned, perl=TRUE), # ,
         concepts_cleaned = gsub("\\b(\\S+)\\s+(?=.*\\b\\1\\b)", "", concepts_cleaned, perl=TRUE)) %>% 
  select(course_id,lec_num,focus_lec_cleaned,theories_cleaned,concepts_cleaned) 



### Treemap of concept per clustering group for Lecture Focus ###

# selecting only relevant cols 
tpm_lec_focus <- combi_courses_cleaned  %>%
    select(cluster_lec_focus, cluster_label_lec_focus, NMF_topic_lec_focus, NMF_topic_label_lec_focus)

# creating the counts so we can create our treemap of topics across clusters
df_cluster_count <- tpm_lec_focus %>%
  count(cluster_lec_focus, concept=cluster_label_lec_focus) %>%
  rename(freq = n)

# Define the saving before creating the fig so it gets exported
jpeg("General themes across all modules.jpeg", width = 1800, height = 1600)
treemap(df_cluster_count,
        index = "concept",   
        vSize = "freq",  
        vColor = "cluster_lec_focus", 
        draw = TRUE,
        palette = "Set3",
        title = "General themes across all modules",
        fontsize.title = 30,
        fontsize.labels = 28
)
dev.off()


# Using the NMF topic modelling results due to improved qualitative performance #

# separating the top 10 topics for the focus_lec
df_nmf <- tpm_lec_focus %>%
  separate_rows(NMF_topic_label_lec_focus, sep = ", ") %>%
  rename(concept = NMF_topic_label_lec_focus) %>%
  mutate(topic_type = "NMF")

tpm_lec_focus_count <- df_nmf %>%
  count(NMF_topic_lec_focus, concept, topic_type) %>%
  rename(freq = n) %>% 
  mutate(NMF_topic_lec_focus = as.factor(NMF_topic_lec_focus))

jpeg("Topics for each of the lecture clusters.jpeg", width = 1800, height = 1600)
treemap(tpm_lec_focus_count,
        index = c("NMF_topic_lec_focus", "concept"),  
        vSize = "freq",  
        vColor = "NMF_topic",  
        palette = "Set1",
        draw = TRUE,
        title = "Topics for each of the lecture clusters",
        fontsize.title = 30,
        fontsize.labels = c(0, 28)
)
dev.off() 


## Assuming that you've created the corresponding NMF for the theories column
tpm_theories <- combi_courses_cleaned  %>%
    select(cluster_theories, cluster_label_theories, NMF_topic_theories, NMF_topic_label_theories)

df_nmf <- tpm_theories %>%
  separate_rows(NMF_topic_label_theories, sep = ", ") %>%
  rename(concept = NMF_topic_label_theories) %>%
  mutate(topic_type = "NMF")

tpm_lec_focus_count <- df_nmf %>%
  count(NMF_topic_theories, concept, topic_type) %>%
  rename(freq = n) %>% 
  mutate(NMF_topic_theories = as.factor(NMF_topic_theories))


jpeg("Topics for each of the theories clusters.jpeg", width = 1800, height = 1600)
treemap(tpm_lec_focus_count,
        index = c("NMF_topic_theories", "concept"),  
        vSize = "freq",  
        vColor = "NMF_topic_theories",  
        palette = "Set1",
        draw = TRUE,
        title = "Topics for each of the theories clusters",
        fontsize.title = 30,
        fontsize.labels = c(0, 28)
)
dev.off() 

## Assuming that you've created the corresponding NMF for the concepts column
tpm_concepts <- combi_courses_cleaned  %>%
    select(cluster_concepts, cluster_label_concepts, NMF_topic_concepts, NMF_topic_label_concepts)

df_nmf <- tpm_concepts %>%
  separate_rows(NMF_topic_label_concepts, sep = ", ") %>%
  rename(concept = NMF_topic_label_concepts) %>%
  mutate(topic_type = "NMF")

tpm_lec_focus_count <- df_nmf %>%
  count(NMF_topic_concepts, concept, topic_type) %>%
  rename(freq = n) %>% 
  mutate(NMF_topic_concepts = as.factor(NMF_topic_concepts))


jpeg("Topics for each of the concepts clusters.jpeg", width = 1800, height = 1600)
treemap(tpm_lec_focus_count,
        index = c("NMF_topic_concepts", "concept"),  
        vSize = "freq",  
        vColor = "NMF_topic",  
        palette = "Set1",
        draw = TRUE,
        title = "Topics for each of the concepts clusters",
        fontsize.title = 30,
        fontsize.labels = c(0, 28)
)
dev.off() 


#### WorldClouds ####

word_cloud_maker <- function(data,variable,plot_name){
  
  # convert into corpus for processing
  courses_corpus <- Corpus(VectorSource(data[[variable]]))

  # commented out since it's already been cleaned in py but just in case you don't use such script
  #courses_corpus <- tm_map(courses_corpus, removeWords, stopwords("english"))
  #courses_corpus <- tm_map(courses_corpus, removePunctuation)
  
  tdm <- TermDocumentMatrix(courses_corpus) 
  tdm <- removeSparseTerms(tdm, 0.999)
  m <- as.matrix(tdm)
  
  word_freqs <- sort(rowSums(m), decreasing=TRUE) # count occurrences and sort in desc order
  
  word_data <- data.frame(word = names(word_freqs), freq = word_freqs)
  
  one_words_cloud <- wordcloud2(word_data, size = 1, color = 'random-dark')

  # export as an animated HTML file
  saveWidget(one_words_cloud, paste0("1_word_wordcloud_", plot_name, ".html"),selfcontained =T)
  
  # exports a png of that html
  webshot::webshot(paste0("1_word_wordcloud_", plot_name, ".html"),paste0("1_word_wordcloud_", plot_name, ".png"),
                          zoom=5, vwidth = 1200, vheight = 900, delay =20)

  # traditional barplot of frequencies
  p <- word_data %>% 
    mutate(word = as.character(word)) %>%
    slice(1:20) %>%
    ggplot(aes(reorder(word,+freq),y=freq,fill=freq)) + # the + insde aes() ensures the decreasing ordering
    geom_bar(stat = "identity")+
    scale_fill_distiller(palette = "BuPu", direction = 1)+
    coord_flip() +
    xlab("") +
    ylab("")+
    labs(fill= "Proportion", title = paste0("Single word ",plot_name, " frequency across courses"))+
    theme_bw(base_size=14) 
  
  print(p)
  ggsave(filename = paste0("Two-word", plot_name, " frequency across courses.jpeg"), 
         plot = p, width = 8, height = 3)
  
  ## 2-word freq 
  tokenizer  <- function(x) {
    NGramTokenizer(x, Weka_control(min = 2, max = 2))
  }
  
  # if directly on text, it doesn't recognise tokenizer for some reason
  produceBigrams <- function(text){
    
    corpus <- VCorpus(VectorSource(text))
    # if duplicated word, remove!
    
    tdm <- TermDocumentMatrix(corpus, control=list(tokenize=tokenizer))
    tdm <- removeSparseTerms(tdm, 0.999)
    m <- as.matrix(tdm)
    word_freqs <- sort(rowSums(m), decreasing=TRUE)
    dm <- data.frame(word=names(word_freqs), freq=word_freqs)
    return(dm)
  }
  
  two_words <- produceBigrams(data[[variable]])
  
  two_words_cloud <-wordcloud2(two_words, size = 0.25, color = 'random-dark') # 0.5 when concepts so they all fit in
  
  saveWidget(two_words_cloud,paste0("2_word_wordcloud_", plot_name, ".html"), selfcontained =T)
  
  webshot::webshot(paste0("2_word_wordcloud_", plot_name, ".html"), paste0("2_word_wordcloud_", plot_name, ".jpeg"),
                   zoom =5,vwidth = 1350, vheight = 1000, delay =20)
  
  
  p <- two_words %>% 
    mutate(word = as.character(word)) %>%
    slice(1:30) %>%
    ggplot(aes(reorder(word,+freq),y=freq,fill=freq)) + # the + inside aes() ensures the decreasing ordering
    geom_bar(stat = "identity")+
    scale_fill_distiller(palette = "BuPu", direction = 1)+
    coord_flip() +
    xlab("") +
    ylab("")+
    labs(fill= "Frequenct", title = paste0("Two-word ", plot_name, " frequency across courses"))+
    theme_bw(base_size=10) +
    theme(axis.text.y = element_text(size=11,hjust = 1,angle=5)) # 6 for original
  
  print(p)
  ggsave(filename = paste0("Two-word ",plot_name," frequency across courses.jpeg"), 
         plot = p, width = 8, height = 4)
   
}
  
concepts_extraction <- word_cloud_maker(combi_courses_cleaned,"lec_focus_cleaned","lecture focus")
theories_extraction <- word_cloud_maker(combi_courses_cleaned,"theories_cleaned","theories")
focus_lect_extraction <- word_cloud_maker(combi_courses_cleaned,"concepts_cleaned","concepts")

