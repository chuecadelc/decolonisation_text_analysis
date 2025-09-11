
# Required libraries

require(tidyverse)
require(purrr)
require(tidytext)
require(viridis)
require(tidyr)
library(udpipe)
library(ggrepel)

# importing the data
setwd("your dir")

SOCI01 <- read.csv("~/module1.csv", skip = 3)
SOCI02 <- read.csv("~/module2.csv", skip = 3)
SOCI03 <- read.csv("~/module3.csv", skip = 3)
SOCI04 <- read.csv("~/module4.csv", skip = 3)
SOCI05 <- read.csv("~/module5.csv", skip = 3)
SOCI06 <- read.csv("~/module6.csv", skip = 3)
SOCI07 <- read.csv("~/module7.csv", skip = 3)
SOCI08 <- read.csv("~/module8.csv", skip = 3)
SOCI09 <- read.csv("~/module9.csv", skip = 3)
SOCI10 <- read.csv("~/module10.csv", skip = 3)
SOCI11 <- read.csv("~/module11.csv", skip = 3)


 ## cleaning up the dataset before processing it for text analysis (see other script)
cleaning_data <- function(data, course_id_name) {
  
 
  data_cleaned <- data %>%
    select(-Comments) %>%
    select(
      lec_num = Lecture.No.,
      focus_lec = Primary.focus.of.the.lecture,
      theories = Theory.s..mentioned,
      concepts = Concept.s..mentioned,
      ref_test = Explicit.reference.to.empire..imperialism..colonialism..race...racism..if.yes..please.specify.,
      global_focus = International.focus...Yes.No.,
      countries = Countries.mentioned
    ) %>%
    # need to separate entries of ref_test into multiple binary cols
    mutate(
      across(where(is.character),  ~ ifelse(. == "" | is.na(.), "No", .)),
      focus_lec = str_to_lower(focus_lec),
      concepts = str_to_lower(concepts),
      concepts = str_split(concepts, ";"),
      theories = str_to_lower(theories),
      theories = str_split(theories, ";"),
      across(
        where(is.character),
        ~ str_replace_all(., "[^[:alnum:]\\s]", "")
      ),
      global_focus = ifelse(str_detect(global_focus, "Yes"), 1, 0),
      imperialism_mention = ifelse(str_detect(ref_test, "\\bimperi\\w*"), 1, 0),
      colonialism_mention = ifelse(str_detect(ref_test, "\\bcolon\\w*"), 1, 0),
      racism_mention = ifelse(str_detect(ref_test, "\\b(raci|race)\\w*"), 1, 0),
      course_id = course_id_name
    ) %>% 
    filter(lec_num!=20) %>% 
    na.omit() %>% 
    select(-c(ref_test))
  
  
  return(data_cleaned)
  
} 

SOCI01_cleaned <- cleaning_data(SOCI01, "SOCI01")
SOCI02_cleaned <- cleaning_data(SOCI02, "SOCI02")
SOCI03_cleaned <- cleaning_data(SOCI03, "SOCI03")
SOCI04_cleaned <- cleaning_data(SOCI04, "SOCI04")
SOCI05_cleaned <- cleaning_data(SOCI05, "SOCI05")
SOCI06_cleaned <- cleaning_data(SOCI06, "SOCI06")
SOCI07_cleaned <- cleaning_data(SOCI07, "SOCI07")
SOCI08_cleaned <- cleaning_data(SOCI08, "SOCI08")
SOCI09_cleaned <- cleaning_data(SOCI09, "SOCI09")
SOCI10_cleaned <- cleaning_data(SOCI10, "SOCI10")
SOCI11_cleaned <- cleaning_data(SOCI11, "SOCI11")


# merge them all
combi_courses <- rbind(SOCI01_cleaned,SOCI02_cleaned,SOCI03_cleaned,SOCI04_cleaned,
                       SOCI05_cleaned,SOCI06_cleaned,SOCI07_cleaned,SOCI08_cleaned,
                       SOCI09_cleaned,SOCI10_cleaned,SOCI11_cleaned)



## Prop of mentions for the binary variables (global focus of the lecture, racism, colonialism and imperialism)
data_counts <- function(data) {
  
  
  ## cleaning up the dataset
  data_cleaned <- data %>%
    # removing unnecessary rows for calculations
    filter(focus_lec !=("no") & focus_lec!=("assessment overview") & focus_lec != ("groupwork preparation for presentations")
          & focus_lec!=("guidance on how to write a research proposal")) %>% ## exclude non-relevant lectures
  group_by(course_id) %>% 
  summarise(prop_global_focus = round(sum(global_focus)/n()*100,2),
         prop_imperialism_mention =  round(sum(imperialism_mention)/n()*100,2),
         prop_colonialism_mention =  round(sum(colonialism_mention)/n()*100,2),
         prop_racism_mention = round(sum(racism_mention)/n()*100,2),
         n_lectures = n())
  
  return(data_cleaned)

}

combi_courses_counts <- data_counts(combi_courses)

## Create the barplots of frequency of mentions of colinialism, racism, imperialism and global focus

p <- combi_courses_counts %>% 
  mutate(course_id = fct_reorder(course_id, prop_global_focus)) %>%
  ggplot(aes(x=course_id, y=prop_global_focus, fill=prop_global_focus)) + 
  scale_fill_distiller(palette = "BuPu", direction = 1)+
  geom_bar(stat = "identity")+
  scale_y_continuous(breaks = round(seq(min(0), max(80), by = 10),10))+
  coord_flip() +
  xlab("") +
  ylab("")+
  labs(fill= "Proportion", title = "Proportion of lectures that had a global focus")+
  theme_bw()
p
ggsave(filename = "Global focus barplot.jpeg", plot = p, width = 12, height = 8) 


p <- combi_courses_counts %>% 
  mutate(course_id = fct_reorder(course_id, prop_imperialism_mention)) %>%
  ggplot(aes(x=course_id, y=prop_imperialism_mention, fill=prop_imperialism_mention)) + 
  scale_fill_distiller(palette = "BuPu", direction = 1)+
  geom_bar(stat = "identity")+
  scale_y_continuous(breaks = round(seq(min(0), max(30), by = 5),5))+
  coord_flip() +
  xlab("") +
  ylab("")+
  labs(fill= "Proportion", title = "Proportion of lectures that mentioned imperialism")+
  theme_bw()

p
ggsave(filename = "Imperialism mention barplot.jpeg", plot = p, width = 12, height = 8) 

p <- combi_courses_counts %>% 
  mutate(course_id = fct_reorder(course_id, prop_racism_mention)) %>% #desc(
  ggplot(aes(x=course_id, y=prop_racism_mention, fill=prop_racism_mention)) + 
  scale_fill_distiller(palette = "BuPu", direction = 1)+
  geom_bar(stat = "identity")+
  scale_y_continuous(breaks = round(seq(min(0), max(60), by = 10),10))+
  coord_flip() +
  xlab("") +
  ylab("")+
  labs(fill= "Proportion", title = "Proportion of lectures that mentioned racism")+
  theme_bw()

p
ggsave(filename = "Racism mention barplot.jpeg", plot = p, width = 12, height = 8) 

