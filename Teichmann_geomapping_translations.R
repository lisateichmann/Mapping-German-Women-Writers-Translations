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
library(ggrepel)

#Extract titles in translations for a list of authors in Deutsche Nationalbibliothek Datenshop and export as csv
#Query: (spo=ger and (atr="Bachmann, Ingeborg")) or (spo=ger and (atr="Aichinger, Ilse")) or (spo=ger and (atr="Müller, Herta")) or (spo=ger and (atr="Tawada, Yoko")) or (spo=ger and (atr="Stefan, Verena")) or (spo=ger and (atr="Özdamar, Emine Sevgi"))
#dnb_bib <- read.csv("~/DNB-German-Fiction-Translations-Catalogue-Data/Data/dnb_transdata_220523/alldnb_2023_220523.csv", sep=",")
dnb_all_geo <- read.csv("data/20220614_alldnb_ch3_author_pubplace_v3.csv")
author_data_female <- read.csv("data/140224_author_data_gnd_gender_femaleonly.csv",sep=",")

#subset geo by female authors only
dnb_fem_geo <- dnb_all_geo[dnb_all_geo$author %in% author_data_female$Author, ]

##data quality check with gender package e.g. robert musil appears twice (1+2)
library(gender)
gender("Robert", method="genderize")
##input just takes first name. add first name to df
author_names <- separate(dnb_fem_geo, author ,into = c("lastname", "firstname"), sep= ",")
author_names$author <- dnb_fem_geo$author
author_names$firstname <- trimws(author_names$firstname, which = c("both"))
author_firstname <- unique(author_names$firstname)

#predict gender
author_names_genderpred <- gender(author_firstname, method="kantrowitz")

##8 male (2 falsely: Gerrit, Gabriele)
author_names_male <- c("Robert", "Franz", "Benjamin","Keith","Karl","Joe")

##remove them
dnb_fem_geo_nomales <- author_names[ ! author_names$firstname %in% author_names_male, ]

###Statistics

##gender bias
#1 (male)    4305
#2 (female)    1742
# %
1742/6047
#28% of all authors are female!

#% of titles of female authors
nrow(dnb_fem_geo_nomales)/nrow(dnb_all_geo)
#20.5% of 34618 titles female

##which author is the most translated for titles?
author_fem_titlefreq <- as.data.frame(table(dnb_fem_geo_nomales$author))

##which author is the most translated for languages
author_lang_group <- dnb_fem_geo_nomales %>%  
  group_by(author) %>%
  summarise(lang_freq = n_distinct(language))

##Create language per author contingency table
# author_lang_freq <- dnb_fem_geo %>% 
#   count(language, author) %>% 
#   pivot_wider(names_from = language, values_from = n, values_fill = list(n = 0))

##which author is the most distributed across publishing places?
author_place_group <- dnb_fem_geo_nomales %>%  
  group_by(author) %>%
  summarise(place_freq = n_distinct(place))

##combine title_freq, lang_freq, place_freq in one table

author_freqs <- author_fem_titlefreq
colnames(author_freqs)[1] <- "author"
colnames(author_freqs)[2] <- "title_freq"
author_freqs$lang_freq <- author_lang_group$lang_freq
author_freqs$place_freq <- author_place_group$place_freq

##export

write.csv(dnb_fem_geo_nomales, file="results/150224_author_data_gnd_gender_femaleonly_geo.csv")
write.csv(author_freqs, file="results/150224_author_data_gnd_gender_femaleonly_geo_freqs.csv")

##Visualize "the most translated" 20 writers

author_freqs %>% 
  arrange(desc(title_freq)) %>%
  slice(1:20) %>%
  pivot_longer(!author, names_to = "type", values_to = "freqs") %>% 
  ggplot(aes(x = reorder(author, -freqs), y = freqs, fill=type)) + geom_bar(stat='identity') + theme(axis.text.x=element_text(angle=45, hjust=1))

ggsave("figures/200224_author_data_gnd_female_freqs_barchart.png", width = 6, height = 4, dpi=300)

##boxplot

# author_freqs %>% 
#   arrange(desc(title_freq)) %>%
#   slice(1:20) %>%
#   pivot_longer(!author, names_to = "type", values_to = "freqs") %>% 
# ggplot(aes(x = reorder(author, -freqs), y=freqs)) + 
#   geom_boxplot()

##correlation
cor(author_freqs[, c('title_freq','lang_freq','place_freq')])
#title and lang or place lower correlation. It is not significantly more likely that an author with increased titles also has increase lang and places

##LM model to see which authors are unexpected (lowest correlation between lang_freq and title_freq)
#fit model
author_freq_model <- lm(title_freq ~ place_freq, data=author_freqs)

#view model summary
summary(author_freq_model) 

#calculate the standardized residuals
standard_res <- rstandard(author_freq_model)

#view the standardized residuals
standard_res

#column bind standardized residuals back to original data frame
author_freq_res <- cbind(author_freqs, standard_res)

#filter unusual authors that have residuals of less than -2 and more than 2
author_freq_res_outliers <- author_freq_res  %>% filter(standard_res < -2  | standard_res > 2)

#plot predictor variable vs. standardized residuals
author_freq_res_outliers %>% ggplot(aes(x = reorder(author, -standard_res), y = standard_res)) + theme(axis.text.x=element_text(angle=45, hjust=1)) + geom_point()

ggsave("figures/200224_author_data_gnd_female_freqs_outliers.png", width = 6, height = 4, dpi=300)
write.csv(author_freq_res_outliers, file="results/150224_author_data_gnd_gender_outliers.csv")

##Plot title and language frequencies for these authors to see which ones stand out
ggplot(author_freq_res_outliers, aes(place_freq, title_freq)) + geom_point()+ ggtitle("Title and publishing place frequencies of authors with residuals of >2 and <-2") +
  labs(x = "Publishing places", y = "Titles") + geom_smooth(method="lm") + theme_bw() + geom_text_repel(aes(label=author), max.overlaps=20)
  
ggsave("figures/200224_author_data_gnd_female_freqs_outliers.png", width = 10, height = 4, dpi=300)

##Investigate outliers
author_freq_res_outliers_geo %>% 
     filter(str_detect(author,"Courths-Mahler"))  %>% 
     group_by(language) %>% 
    tally()

author_freq_res_outliers_geo %>% 
  filter(str_detect(author,"Courths-Mahler"))  %>% 
  group_by(place) %>% 
  tally()

View(author_freq_res_outliers_geo %>% 
  filter(str_detect(author,"Jelinek"))  %>% 
  group_by(language) %>% 
  tally())

View(author_freq_res_outliers_geo %>% 
  filter(str_detect(author,"Jelinek"))  %>% 
  group_by(place) %>% 
  tally())

View(author_freq_res_outliers_geo %>% 
       filter(str_detect(author,"Link"))  %>% 
       group_by(language) %>% 
       tally())

View(author_freq_res_outliers_geo %>% 
       filter(str_detect(author,"Link"))  %>% 
       group_by(place) %>% 
       tally())

View(author_freq_res_outliers_geo %>% 
       filter(str_detect(author,"Link")))

View(author_freq_res_outliers_geo %>% 
       filter(str_detect(author,"Neuhaus"))  %>% 
       group_by(language) %>% 
       tally())

View(author_freq_res_outliers_geo %>% 
       filter(str_detect(author,"Neuhaus"))  %>% 
       group_by(place) %>% 
       tally())

View(author_freq_res_outliers_geo %>% 
       filter(str_detect(author,"Neuhaus")))

##Map outliers

#append coordinates to author_freq_res_outliers
author_freq_res_outliers_geo <- merge(author_freq_res_outliers,dnb_fem_geo_nomales, by  = "author") 
author_freq_res_outliers_geo <- author_freq_res_outliers_geo[order(author_freq_res_outliers_geo$place_freq, author_freq_res_outliers_geo$place_freq),]

#map: colors for authors in groups depending on tresholds: >100 titles, 50-100, <50 and radius ==titles
pal <- colorFactor(
  palette = 'Paired',
  domain = author_freq_res_outliers_geo$author,
  ordered=FALSE
)

leaflet(author_freq_res_outliers_geo) %>%
  addTiles() %>%
  addCircles(lng = ~longitude, lat = ~latitude, weight = 5,
             popup= ~paste(
               "<strong> Author: </strong>", author, "<br>",
               "<strong> Language: </strong>", language, "<br>",
               "<strong> Publisher: </strong>", publisher, "<br>",
               "<strong> Original Title: </strong>", uniform.title, "<br>",
               "<strong> Translated Title: </strong>", title, "<br>"), 
             color = ~pal(author),
             radius = author_freq_res_outliers_geo$place_freq/100) %>% 
  addLegend(pal = pal, values = ~author, group = "circles", position = "topright")

#only keep authors with >100 titles
author_cases_100plus <- author_freq_res_outliers_geo[author_freq_res_outliers_geo$title_freq>100, ]

pal <- colorFactor(
  palette = 'Dark2',
  domain = author_cases_100plus$author,
  ordered=FALSE
)

leaflet(author_cases_100plus) %>%
  addTiles() %>%
  addCircles(lng = ~longitude, lat = ~latitude, weight = 5,
             popup= ~paste(
               "<strong> Author: </strong>", author, "<br>",
               "<strong> Language: </strong>", language, "<br>",
               "<strong> Publisher: </strong>", publisher, "<br>",
               "<strong> Original Title: </strong>", uniform.title, "<br>",
               "<strong> Translated Title: </strong>", title, "<br>"), 
             color = ~pal(author),
             radius = author_cases_100plus$place_freq/100) %>% 
  addLegend(pal = pal, values = ~author, group = "circles", position = "topright")

###Mapping all authors

##Eurocentrism
##inside vs. outside europe
dnb_fem_geo_european <- dnb_fem_geo_nomales %>% mutate(newcol = ifelse(str_detect(country, "^XA"), "European", "Non-European"))
#remove rows with missing place
dnb_fem_geo_european <- dnb_fem_geo_european %>% filter(!dnb_fem_geo_european$place=="")
##titles for european vs non-european
table(dnb_fem_geo_european$newcol)

##publishing centres
leaflet() %>%
  addTiles() %>% 
  addMarkers(data = dnb_fem_geo_nomales, 
             popup=~place,
             clusterOptions = markerClusterOptions())

pub_places_freq <- as.data.frame(table(dnb_fem_geo_nomales$place))

write.csv(pub_places_freq, "results/110324_author_data_gnd_gender_femaleonly_geo_pubplace_freqs.csv")

##which author has the widest geographic reach outside of europe?
dnb_fem_geo_noneuropean <- dnb_fem_geo_european %>% filter(dnb_fem_geo_european$newcol=="Non-European")
View(table(dnb_fem_geo_noneuropean$author))

write.csv((table(dnb_fem_geo_noneuropean$author)), "results/110324_author_data_gnd_gender_femaleonly_geo_noneuropean.csv")

##The least translated authors
summary(author_freqs)

##% of authors with <5 titles, mean==5.87
nrow(author_freqs[author_freqs$title_freq<5, ])/nrow(author_freqs)
#79% of authors have less than 5 titles, meaning that 20% of authors are frequently translated

##% of authors with <5 languages, mean==2.73 (even fewer!)
nrow(author_freqs[author_freqs$lang_freq<5, ])/nrow(author_freqs)
#85% of authors have less than 5 titles, meaning that 15% of authors are translated into many languages, only 18 of which >20 languages

##% of authors with <5 places, mean==3.15
nrow(author_freqs[author_freqs$place_freq<5, ])/nrow(author_freqs)
#85% of authors have less than 5 titles, meaning that 15% of authors are frequently translated

##long tail
nrow(author_freqs[author_freqs$lang_freq==1, ])/nrow(author_freqs)
##64% only one translated title!!

##Publishing places of least translated
least_translated <- author_freqs[author_freqs$place_freq==1, ]
#append geo
least_translated_geo <- dnb_fem_geo_nomales[dnb_fem_geo_nomales$author %in% least_translated$author, ]

##bubble map of literary centres
leaflet() %>%
  addTiles() %>% 
  addMarkers(data = least_translated_geo, 
                   popup=~place,
             clusterOptions = markerClusterOptions())

######archive#######

###Geomapping

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


