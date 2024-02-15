###Plot the publishing place and languages for selected authors
##as part of the poster presentation for the Spatial Humanities Conference 2022
##Author: Lisa Teichmann, LaDiRec and McGill University
## Date: 31 August 2022
##Complete repo: https://doi.org/10.5683/SP3/VNUEP0
##See Shiny app: https://lt-ladirec.shinyapps.io/GeomappingTranslationsPrototype/

library(ggplot2)
library(dplyr)
library(gridExtra)
library(tidyr)
library(stringr)
library(leaflet)
library(tidygeocoder)

#Extract titles in translations for a list of authors in Deutsche Nationalbibliothek Datenshop and export as csv
#Query: (spo=ger and (atr="Bachmann, Ingeborg")) or (spo=ger and (atr="Aichinger, Ilse")) or (spo=ger and (atr="Müller, Herta")) or (spo=ger and (atr="Tawada, Yoko")) or (spo=ger and (atr="Stefan, Verena")) or (spo=ger and (atr="Özdamar, Emine Sevgi"))
#dnb_bib <- read.csv("~/DNB-German-Fiction-Translations-Catalogue-Data/Data/dnb_transdata_220523/alldnb_2023_220523.csv", sep=",")
dnb_all_geo <- read.csv("20220614_alldnb_ch3_author_pubplace_v3.csv")
author_data_female <- read.csv("140224_author_data_gnd_gender_femaleonly.csv",sep=",")

#subset geo by female authors only
dnb_fem_geo <- dnb_all_geo[dnb_all_geo$author %in% author_data_female$Author, ]

#map
pal <- colorFactor(
  palette = 'Dark2',
  domain = dnb_fem_geo$language
)

leaflet(dnb_fem_geo) %>%
  addTiles() %>%
  addCircles(lng = ~longitude, lat = ~latitude, weight = 5,
             popup= ~paste(
               "<strong> Author: </strong>", author, "<br>",
               "<strong> Language: </strong>", language, "<br>",
               "<strong> Publisher: </strong>", publisher, "<br>",
               "<strong> Original Title: </strong>", uniform.title, "<br>",
               "<strong> Translated Title: </strong>", title, "<br>"), 
             color = ~pal(language)) %>% 
  addLegend(pal = pal, values = ~language, group = "circles", position = "topright")


##Create language per author contingency table
author_lang_freq <- dnb_fem_geo %>% 
  count(language, author) %>% 
  pivot_wider(names_from = language, values_from = n, values_fill = list(n = 0))

##which author has most languages?



#Distribution of country codes
countries <- as.data.frame(table(dnb_bib$country))
#Country codes can be found here: https://d-nb.info/standards/vocab/gnd/geographic-area-code.html#XA-AAAT
languages <- as.data.frame(table(dnb_bib$language))

###Split publisher and publication place
dnb_bib_pubplace <- separate(dnb_bib, publisher ,into = c("place", "publisher"), sep= ":")
#remove special characters in the new column
dnb_bib_pubplace$place<-gsub("[[:punct:]]"," ",as.character(dnb_bib_pubplace$place))
dnb_bib_pubplace$place <- trimws(dnb_bib_pubplace$place, which = c("both"))

###Split author column
dnb_bib_pubplace <- separate(dnb_bib_pubplace, creator ,into = c("author", "translator"), sep= ";")
#delete everything after the author's name
dnb_bib_pubplace$author <- gsub("\\[.*","",dnb_bib_pubplace$author)
dnb_bib_pubplace$author <- gsub("\\,,.*","",dnb_bib_pubplace$author)
dnb_bib_pubplace$author <- trimws(dnb_bib_pubplace$author, which = c("both"))
dnb_bib_pubplace$author <- gsub('[[:digit:]]+', '',dnb_bib_pubplace$author)
dnb_bib_pubplace$author <- gsub("\\, -.*","",dnb_bib_pubplace$author)
dnb_bib_pubplace$author <- gsub("\\(.*","",dnb_bib_pubplace$author)
dnb_bib_pubplace$author <- trimws(dnb_bib_pubplace$author, which = c("both"))


##Create place per author contingency table
author_place_freq <- dnb_bib_pubplace %>% 
  count(place, author) %>% 
  pivot_wider(names_from = place, values_from = n, values_fill = list(n = 0))

##Geocode publishing places with the Nominatim DOI
dnb_bib_geo <- dnb_bib_pubplace %>%
  geocode(place, method = 'osm', lat = latitude , long = longitude)

#write.csv(dnb_bib_geo, "dnb-datashop_2022-2-1T18_39_59_geocoded.csv")

##Alternative: read from file
#dnb_bib_geo <- read.csv("dnb-datashop_2022-2-1T18_39_59_geocoded.csv")

##Map all authors
pal <- colorFactor(
  palette = 'Dark2',
  domain = dnb_bib_geo$author
)

leaflet(dnb_bib_geo) %>%
  addTiles() %>%
  addCircles(lng = ~longitude, lat = ~latitude, weight = 5,
             popup= ~paste(
               "<strong> Author: </strong>", author, "<br>",
               "<strong> Language: </strong>", language, "<br>",
               "<strong> Publisher: </strong>", publisher, "<br>",
               "<strong> Original Title: </strong>", uniform.title, "<br>",
               "<strong> Translated Title: </strong>", title, "<br>"), 
             color = ~pal(author)) %>% 
  addLegend(pal = pal, values = ~author, group = "circles", position = "topright")


##Subset for each author and map
bachmann <- dnb_bib_geo %>%
  filter(str_detect(author,"Bachmann"))

pal <- colorFactor(
  palette = 'Dark2',
  domain = bachmann_geo$language
)

leaflet(bachmann) %>%
  addTiles() %>%
  addCircles(lng = ~longitude, lat = ~latitude, weight = 5,
             popup= ~paste(
               "<strong> Language: </strong>", language, "<br>",
               "<strong> Publisher: </strong>", publisher, "<br>",
               "<strong> Title: </strong>", uniform.title, "<br>"), 
             color = ~pal(language))
#%>% addLegend(pal = pal, values = ~language, group = "circles", position = "bottomright")

aichinger <- dnb_bib_geo %>%
  filter(str_detect(author,"Aichinger"))

pal <- colorFactor(
  palette = 'Dark2',
  domain = aichinger_geo$language
)

leaflet(aichinger) %>%
  addTiles() %>%
  addCircles(lng = ~longitude, lat = ~latitude, weight = 5,
             popup= ~paste(
               "<strong> Language: </strong>", language, "<br>",
               "<strong> Publisher: </strong>", publisher, "<br>",
               "<strong> Title: </strong>", uniform.title, "<br>"), 
             color = ~pal(language))
#%>% addLegend(pal = pal, values = ~language, group = "circles", position = "bottomright")

muller <- dnb_bib_geo %>%
  filter(str_detect(author,"Müller"))

pal <- colorFactor(
  palette = 'Dark2',
  domain = muller$language
)

leaflet(muller) %>%
  addTiles() %>%
  addCircles(lng = ~longitude, lat = ~latitude, weight = 5,
             popup= ~paste(
               "<strong> Language: </strong>", language, "<br>",
               "<strong> Publisher: </strong>", publisher, "<br>",
               "<strong> Title: </strong>", uniform.title, "<br>"), 
             color = ~pal(language))
#%>% addLegend(pal = pal, values = ~language, group = "circles", position = "bottomright")

##Create language per author contingency table
author_lang_freq <- dnb_bib_pubplace %>% 
  count(language, author) %>% 
  pivot_wider(names_from = language, values_from = n, values_fill = list(n = 0))

##Count languages per author
#See which author has most languages
#Count the number of languages they publish in and divide by total number of languages

table(ozdamar$language)
length(table(ozdamar$language))
#15 languages
table(aichinger$language)
length(table(aichinger$language))
#16 languages
table(muller$language)
length(table(muller$language))
#41 languages
table(stefan$language)
length(table(stefan$language))
#3 languages
table(tawada$language)
length(table(tawada$language))
#11 languages

##Barplot of language versus title count per author

##Frequency table for languages per author
language_count <- dnb_bib_pubplace %>%
  group_by(author) %>%
  summarise(count=n_distinct(language))

names(language_count)[2] <- "languages"

##Frequency table for titles per author
title_count <- as.data.frame(table(dnb_bib_pubplace$author))
names(title_count)[1] <- "author"
names(title_count)[2] <- "titles"

lang_title_count <- merge(language_count, title_count, by="author")

##Get title/language ratios
#lang_title_count$ratio <- lang_title_count$titles / lang_title_count$languages

##Plot
library(reshape2)
lang_title_count_long<-melt(lang_title_count)
ggplot(lang_title_count_long,aes(author,value,fill=variable))+
  geom_bar(stat="identity",position="dodge")+ xlab("Authors") + ylab("Frequency") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + ggtitle("Title versus language count") +coord_flip()

##Barplot for country and place count

##Frequency table for countries per author
country_count <- dnb_bib_pubplace %>%
  group_by(author) %>%
  summarise(count=n_distinct(country))

names(country_count)[2] <- "countries"

##Frequency table for places per author
place_count <- dnb_bib_geo %>%
  group_by(author) %>%
  summarise(count=n_distinct(place))

names(place_count)[2] <- "places"

##Merge the two dataframes
country_place_count <- merge(country_count, place_count, by="author")

##Plot
country_place_count_long<-melt(country_place_count)
ggplot(country_place_count_long,aes(author,value,fill=variable))+
  geom_bar(stat="identity",position="dodge")+ xlab("Authors") + ylab("Title Frequency") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + ggtitle("Titles per publishing place and country") +coord_flip()


