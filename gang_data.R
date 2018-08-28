library(readxl)
library(tidyverse)
library(sf)
library(leaflet)
library(viridis)
library(dplyr)
library(stringr)

df_gang_318 <- read_excel("CPD Gang Data/CPD gang database 3-18.xlsx", sheet=1)

# I downloaded the Chicago Police Beats Shapefile from the Chicago data portal
map_filepath <- "Boundaries_Police Beats/geo_export_CPD_BEATS.shp"
cpd_beats <- st_read(map_filepath)

#cleaning the data -- take out any rows with NA for a police beat. 

df_gangs <- filter(df_gang_318, !is.na(O_BEAT))
cpd_beats <- filter(cpd_beats, !is.na(beat_num))


#this is just a quick map to map the police beats. Can click on each area to show which beat it is.

cpd_beats %>% 
  leaflet() %>% 
  addTiles() %>% 
  addPolygons(popup=~beat_num)


# it looks like df_gangs needs a padded 0 in front of some of the numbers to make them 4 digits instead of 3. So let's clean that up before we join it with cpd_beats (which is all 4 digits)

df_gangs$O_BEAT <- str_pad(df_gangs$O_BEAT, 4, pad = "0")

#inner join the two data frames. Used an inner join to drop any rows where the two police beat numbers didn't match up

cpd_beats_gangs<- inner_join(cpd_beats, df_gangs, by=c('beat_num'='O_BEAT'))


#get the number of arrests per police beat and create a new data frame with just that info

CPD_gang_map2 <- cpd_beats_gangs %>%
  group_by(beat_num) %>%
  summarize(num_arrests=n()) 

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

