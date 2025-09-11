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


list_countries <- c("UK", "US", "EU", "Global North",  "Global South", "Middle East", "Great Britain",
                    "Scotland", "England", "NI", "Northern Ireland", "Wales", "Welsh", "Scottish", "English",
                    "Commonwealth", "European", "British", "Indian", "Europe", 
                    "Afghanistan", "Albania", "Algeria", "Andorra", "Angola", "Antigua and Barbuda", 
                    "Argentina", "Armenia", "Australia", "Austria", "Azerbaijan", "Bahamas", 
                    "Bahrain", "Bangladesh", "Barbados", "Belarus", "Belgium", "Belize", "Benin", 
                    "Bhutan", "Bolivia", "Bosnia and Herzegovina", "Botswana", "Brazil", "Brunei", 
                    "Bulgaria", "Burkina Faso", "Burundi", "Cabo Verde", "Cambodia", "Cameroon", 
                    "Canada", "Central African Republic", "Chad", "Chile", "China", "Colombia", 
                    "Comoros", "Congo", "Costa Rica", "Croatia", "Cuba", "Cyprus", "Czech Republic", 
                    "Denmark", "Djibouti", "Dominica", "Dominican Republic", "Ecuador", "Egypt", 
                    "El Salvador", "Equatorial Guinea", "Eritrea", "Estonia", "Eswatini", "Ethiopia", "Fiji", "Finland", "France", 
                    "Gabon", "Gambia", "Georgia", "Germany", "Ghana", "Greece", "Grenada", 
                    "Guatemala", "Guinea", "Guinea-Bissau", "Guyana", "Haiti", "Honduras", "Hungary", 
                    "Iceland", "India", "Indonesia", "Iran", "Iraq", "Ireland", "Israel", 
                    "Italy", "Jamaica", "Japan", "Jordan", "Kazakhstan", "Kenya", "Kiribati", 
                    "Korea North", "Korea South", "Kuwait", "Kyrgyzstan", "Laos", "Latvia", 
                    "Lebanon", "Lesotho", "Liberia", "Libya", "Liechtenstein", "Lithuania", "Luxembourg", 
                    "Madagascar", "Malawi", "Malaysia", "Maldives", "Mali", "Malta", "Marshall Islands", 
                    "Mauritania", "Mauritius", "Mexico", "Micronesia", "Moldova", "Monaco", 
                    "Mongolia", "Montenegro", "Morocco", "Mozambique", "Myanmar", "Namibia", 
                    "Nauru", "Nepal", "Netherlands", "New Zealand", "Nicaragua", "Niger", 
                    "Nigeria", "North Macedonia", "Norway", "Oman", "Pakistan", "Palau", 
                    "Panama", "Papua New Guinea", "Paraguay", "Peru", "Philippines", "Poland", 
                    "Portugal", "Qatar", "Romania", "Russia", "Rwanda", "Saint Kitts and Nevis", 
                    "Saint Lucia", "Saint Vincent and the Grenadines", "Samoa", "San Marino", 
                    "Sao Tome and Principe", "Saudi Arabia", "Senegal", "Serbia", "Seychelles", 
                    "Sierra Leone", "Singapore", "Slovakia", "Slovenia", "Solomon Islands", 
                    "Somalia", "South Africa", "South Sudan", "Spain", "Sri Lanka", "Sudan", 
                    "Suriname", "Sweden", "Switzerland", "Syria", "Taiwan", "Tajikistan", 
                    "Tanzania", "Thailand", "Timor-Leste", "Togo", "Tonga", "Trinidad and Tobago", 
                    "Tunisia", "Turkey", "Turkmenistan", "Tuvalu", "Uganda", "Ukraine", "United Arab Emirates", 
                    "United Kingdom", "United States", "Uruguay", "Uzbekistan", "Vanuatu", 
                    "Vatican City", "Venezuela", "Vietnam", "Yemen", "Zambia", "Zimbabwe")


# Note this function can also be found on the Frequency of mentions.R script since I wanted to make sure both were independent 
cleaning_data <- function(data, course_id_name) {
  
  ## cleaning up the dataset
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
      across(where(is.character),  ~ ifelse(. == "" | is.na(.), "No", .)), # this prevents full rows of being removed w/ na.omit() as some columns have no values
      across(
        where(is.character),
        ~ str_replace_all(., "[^[:alnum:]\\s]", "") # removing numeric and other unnecessary characters
      ),
      global_focus = ifelse(str_detect(global_focus, "Yes"), 1, 0),
      imperialism_mention = ifelse(str_detect(ref_test, "\\bimperi\\w*"), 1, 0),
      colonialism_mention = ifelse(str_detect(ref_test, "\\bcolon\\w*"), 1, 0),
      racism_mention = ifelse(str_detect(ref_test, "\\b(raci|race)\\w*"), 1, 0),
      countries_mention = map_chr(
        str_extract_all(countries, paste(list_countries, collapse = "|")),
        ~ ifelse(length(.) > 0, str_c(., collapse = ";"), "No country")
      ),
      countries_mention = str_split(countries_mention, ";") ,
      course_id = course_id_name
    ) %>% 
    filter(lec_num!=20) %>% # no need since it's about assessment and no countries are mentioned
    na.omit() %>% 
    select(-c(ref_test,countries))
  
  
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

# Preparing the map data #

# ensuring the country mention can match our map data country labels. 
#Note that we're aggregating mentions of all the UK nations for the purpose of this map.
countries_extraction <- combi_courses %>% 
    unnest(countries_mention) %>%  # Unnest countries first
    mutate(#course_id = factor(as.numeric(course_id), ordered = TRUE),
      countries_mention = case_when(
        countries_mention == "UK" ~ "United Kingdom",
        countries_mention == "Great Britain" ~ "United Kingdom",
        countries_mention == "England" ~ "United Kingdom",
        countries_mention == "British" ~ "United Kingdom",
        countries_mention == "Sctoland" ~ "United Kingdom",
        countries_mention == "NI" ~ "United Kingdom",
        countries_mention == "EU" ~ "Europe",
        countries_mention == "European" ~ "Europe",
        countries_mention == "US" ~ "United States of America",
        country == "Spain" ~ "Spain",
        TRUE ~ countries_mention
      )
    ) %>%
   # unnest(countries_mention) %>%
    count(course_id, countries_mention, sort = TRUE) %>%  # lec_num, unless we want to see how many times per lecture a given country was mentioned
    pivot_wider(names_from = countries_mention, values_from = n, values_fill = list(n = 0)) 

## this used to create a heatmap of courses and countries mentioned
countries_summary_long <- 
  countries_extraction %>%
  pivot_longer(cols = -course_id, names_to = "country", values_to = "count") 
  
# nice but hard to read cause labels overlap - additional adjustment needed

ggplot(countries_summary_long, aes(x = country, y = course_id, fill = count)) +
  geom_tile() +
  coord_flip()+
  scale_fill_viridis(option = "rocket", direction =-1) +  # Change color scale as needed
  theme_minimal() +
  labs(title = "Countries mentioned across modules",
       x = "Countries",
       y = "Cours") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 


## mapping time w/ the above ## 

require(sf)
library(rnaturalearth) ## latest package to get geospatial data
library(rnaturalearthdata)
library(leaflet)
library(webshot)
library(mapview)
require(htmlwidgets)

# get the world map
world_map <- ne_countries(scale = "medium", returnclass = "sf")

# exclude antartica to make maps more readable
world_map <- world_map %>%
  filter(name != "Antarctica")


# aggregate across all modules (since it was pivot wider before)
countries_filter <- countries_extraction %>% 
  select(-course_id) %>%  # Exclude the course_id column
  summarise(across(everything(), sum, na.rm = TRUE)) %>%
  pivot_longer(cols = everything(), names_to = "country", values_to = "count")  %>% 
  unique()

# Merge the world shapefile with the country mentions
world_map_merged <- world_map %>%
  left_join(countries_filter, by = c("name" = "country")) %>% 
  mutate(present = if_else(is.na(count), 0, if_else(count > 0, 1, 0)),
         present = factor(present, levels = c(0, 1), labels=c("Not mentioned", "Mentioned"))) 

# set colors to grey and purple (since that's DU colors)
pal <- colorFactor(palette = c("lightgrey","#8856a7"), domain = world_map_merged$present, na.color="lightgrey")


# basic map - not that exciting
map_plot <- ggplot(world_map_merged) +
  geom_sf(aes(fill = count), color = "black", size = 0.1) +  #factor(present)
  #scale_fill_manual(values = c("lightgrey","#8856a7"),labels =c("Not mentioned", "Mentioned"))+
  scale_fill_distiller(palette = "BuPu", direction = 1, na.value = "gray90") +
  #scale_fill_viridis(option = "viridis", direction= -1, na.value = "gray90") +  # Adjust colors for missing data
  theme_minimal() +
  labs(title = "Countries mentioned across modules",
       fill = "Mention Count", ) +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom")+
  coord_sf(xlim = c(-180, 180), ylim = c(-90, 90))

map_plot

# Save jpeg map
ggsave("countries_mentioned_across_modules_map.jpeg", plot = map_plot, width = 8, height = 10, units = "in")



# Convert to leaflet-friendly format to make it interactive #

leaflet_map <- leaflet(world_map_merged) %>%
  addTiles() %>%
  addPolygons(
    fillColor = ~pal(present),
    weight = 1.25,
    opacity = 1,
    color = "black",
    fillOpacity = 1.7,
    popup = ~paste(name, ": ", count, " mentions")
  ) %>% 
  addLegend(
    position = "bottomleft",  # so we can see Aus
    pal = pal,
    values = ~present,
    title = "Countries mentioned", 
   labels = levels(world_map_merged$present),
    opacity = 1
  ) %>% 
  setView(lng = 0, lat = 20, zoom = 2)

# Render the interactive map
leaflet_map

# export it as HTML & JPEG
saveWidget(leaflet_map, "total_countries_mentioned_across_modules_leaflet_map.html",)
mapshot(leaflet_map, file ="total_countries_mentioned_across_modules_leaflet_map.jpeg")


## Show the frequency of mentions of a given country #

leaflet_map <- leaflet(world_map_merged) %>%
  addTiles() %>%
  addPolygons(
    fillColor = ~colorNumeric(palette = "viridis", reverse = TRUE, domain = world_map_merged$present, na.color="lightgrey")(count),
    weight = 1.25,
    opacity = 1,
    color = "black",
    fillOpacity = 1.7,
    popup = ~paste(name, ": ", count, " mentions")
  ) %>% 
  addLegend(
    position = "bottomleft",  # so we can see Aus
    pal = colorNumeric(palette = "viridis",reverse = TRUE, domain = world_map_merged$count),  # Reversed palette
    values = ~count,
    title = "Mentions Count",
    opacity = 1
  ) %>% 
  setView(lng = 0, lat = 20, zoom = 2)

# Render the interactive map
leaflet_map

# export it as HTML & JPEG
saveWidget(leaflet_map, "freq_countries_mentioned_across_modules_leaflet_map.html",)
mapshot(leaflet_map, file ="freq_countries_mentioned_across_modules_leaflet_map.jpeg")

