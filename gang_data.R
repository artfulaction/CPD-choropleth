library(readxl)
library(tidyverse)
library(sf)
library(leaflet)
library(viridis)
library(dplyr)
library(stringr)

#setwd("~/code/CPD Gang database")
df_gang_318 <- read_excel("CPD Gang Data/CPD gang database 3-18.xlsx", sheet=1)
df_gang_1117 <- read_excel("CPD Gang Data/CPD gang database 11-17.xlsx", sheet=1)

# I downloaded the Chicago Police Beats Shapefile from the Chicago data portal
map_filepath <- "Boundaries_Police Beats/geo_export_CPD_BEATS.shp"
cpd_beats <- st_read(map_filepath)

#cleaning the data -- take out any rows with NA for a police beat for both datasets 

df_gangs_318_ONLYBEATS <- filter(df_gang_318, !is.na(O_BEAT))
df_gangs_1117_ONLYBEATS <- filter(df_gang_1117, !is.na(BEAT_FIRST_ARREST))


#this is just a quick map to map the police beats. Can click on each area to show which beat it is.

cpd_beats %>% 
  leaflet() %>% 
  addTiles() %>% 
  addPolygons(popup=~beat_num)


# it looks like both dfs need a padded 0 in front of some of the numbers to make them 4 digits instead of 3. So let's clean that up before we join it with cpd_beats (which is all 4 digits)

df_gangs_318_ONLYBEATS$O_BEAT <- str_pad(df_gangs_318_ONLYBEATS$O_BEAT, 4, pad = "0")
df_gangs_1117_ONLYBEATS$BEAT_FIRST_ARREST <- str_pad(df_gangs_1117_ONLYBEATS$BEAT_FIRST_ARREST, 4, pad = "0")

#inner join the two data frames. Used an inner join to drop any rows where the two police beat numbers didn't match up

beats_plus_gangs_318<- inner_join(cpd_beats, df_gangs_318_ONLYBEATS, by=c('beat_num'='O_BEAT'))
beats_plus_gangs_1117<- inner_join(cpd_beats, df_gangs_1117_ONLYBEATS, by=c('beat_num'='BEAT_FIRST_ARREST'))

#get the number of arrests per police beat and create a new data frame with just that info. using the 11-17 database since that's the one that specifically said "arrests." the 3-18 one just uses "classifications" so not using it for now.

#probably need to rename CPD_gang_map2 IS ONLY DATA FROM 11-17 DATABASE!!!
#keeping this as CPD_gang_map2 here just because I'm going to have to change this variable everywhere else now ughhhh
CPD_gang_map2 <- beats_plus_gangs_1117 %>%
  group_by(beat_num) %>%
  summarize(num_arrests=n()) 

#gang_map3 is from the 3-18 database, but didn't end up using this one yet. Is there a way to overlay???
CPD_gang_map3 <- beats_plus_gangs_318 %>%
  group_by(beat_num) %>%
  summarize(num_classed=n()) 


#set the color pallette as Blues. 

col_pal <- colorNumeric("Blues", domain=CPD_gang_map2$num_arrests)

popup_sb <- paste0("Police Beat: ", as.character(CPD_gang_map2$beat_num), "<br>Number of arrests: ", as.character(CPD_gang_map2$num_arrests))


# yaassss the following leaflet worked! 

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(-87.628598, 41.855372, zoom = 10) %>% 
  addPolygons(data = CPD_gang_map2, 
              fillColor = ~col_pal(CPD_gang_map2$num_arrests), 
              fillOpacity = 0.7, 
              weight = 0.2, 
              smoothFactor = 0.2, 
              popup = ~popup_sb) %>%
  addLegend(pal = col_pal, 
            values = CPD_gang_map2$num_arrests, 
            position = "bottomright", 
            title = "Number of Arrests")

