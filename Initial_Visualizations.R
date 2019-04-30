## Initial Visualizations

#### Packages  --------------------------------------------------------------------------------------------------------
library(tidyverse)
library(shiny)
library(proj4)
library(ggmap)
library(maps)
library(leaflet)
library(leaflet.extras)
library(magrittr)
library(scales)
library(plotly)

#### Data  ------------------------------------------------------------------------------------------------------------

laparking <- read_csv("~/Downloads/parking-citations.csv")

laparking_18 <- laparking %>%
  filter(`Issue Date` >= as.Date("2018-01-01"), `Issue Date` <= as.Date("2018-12-31"))

laparking_ytd <- laparking %>%
  filter(`Issue Date` >= as.Date("2019-01-01"), `Issue Date` <= as.Date("2019-04-15")) %>%
  select(-`Meter Id`, -`Marked Time`, -VIN, -Route) %>%
  mutate(Latitude = ifelse(Latitude == 99999, NA, Latitude), 
         Longitude = ifelse(Longitude == 99999, NA, Longitude)) %>%
  mutate(`Issue time` = ifelse(nchar(`Issue time`)==3, paste("0", `Issue time`, sep = ""), `Issue time`)) %>%
  mutate(`Issue time` = as.POSIXct(`Issue time`, format = "%H%M")) %>%
  mutate(`Issue time` = format(`Issue time`,"%H%M"))

#### Data Cleaning ----------------------------------------------------------------------------------------------------
str(laparking)
str(laparking$`Issue Date`)

table(laparking_ytd$`RP State Plate`)
table(laparking$`RP State Plate`)

## Latitude/Longitude Cleaning ----------------------------------------------------------------------------------------
proj4string <- "+proj=lcc +lat_1=34.03333333333333 +lat_2=35.46666666666667 +lat_0=33.5 +lon_0=-118 +x_0=2000000 +y_0=500000.0000000002 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 no_defs"

pj <- project(laparking_ytd[, c("Latitude", "Longitude")], proj4string, inverse=TRUE)
latlong <- data.frame(New.Latitude=pj$y, New.Longitude=pj$x)
laparking_ytd <- cbind(laparking_ytd, latlong)

# write_csv(laparking_ytd, "laparking_ytd.csv")

#### Initial Visualizations ------------------------------------------------------------------------------------------
# Register a Google API key

#Test
geocode("Los Angeles", output = "all")

## General maps
levels(factor(laparking_ytd$`Violation Description`))
table(laparking_ytd$`Violation Description`)

laparking_ytd %>%
  group_by(`Violation Description`) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

la.map1 <- ggmap(get_map(location = 'Los Angeles', zoom = 13))
la.map1

la.map_bw <- ggmap(get_map(location = 'Los Angeles', zoom = 13, color = "bw"))
la.map_bw

la.map1 + 
  stat_density2d(data = filter(laparking_ytd), aes(x = New.Longitude, y = New.Latitude, fill = ..level.., alpha = ..level..),
                 geom = "polygon", size = 0.01) +
  scale_fill_gradient(low = "red", high = "green")

# Street Cleaning
la.map1 + 
  stat_density2d(data = filter(laparking_ytd, `Violation Description` == "NO PARK/STREET CLEAN"), aes(x = New.Longitude, y = New.Latitude, fill = ..level.., alpha = ..level..),
                                                          geom = "polygon", size = 0.01) +
  scale_fill_gradient(low = "yellow", high = "red") + scale_alpha(guide = 'none') +
  labs(title = "Tickets Issued for No Parking (Street Cleaning)", subtitle = "Tickets issued from 1/1/2019 to 4/15/2019",
       caption = "Source: City of Los Angeles") +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())

ggplotly(
  la.map1 + 
    stat_density2d(data = filter(laparking_ytd, `Violation Description` == "NO PARK/STREET CLEAN"), aes(x = New.Longitude, y = New.Latitude, fill = ..level.., alpha = ..level..),
                   geom = "polygon", size = 0.01) +
    scale_fill_gradient(low = "yellow", high = "red") + scale_alpha(guide = 'none') + 
    theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
          axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())
) %>% toWebGL()

la.map_bw + 
  stat_density2d(data = filter(laparking_ytd, `Violation Description` == "NO PARK/STREET CLEAN"), aes(x = New.Longitude, y = New.Latitude, fill = ..level.., alpha = ..level..),
                 geom = "polygon", size = 0.01) +
  scale_fill_gradient(low = "yellow", high = "red") + scale_alpha(guide = 'none') +
  labs(title = "Tickets Issued for No Parking (Street Cleaning)", subtitle = "Tickets issued from 1/1/2019 to 4/15/2019",
       caption = "Source: City of Los Angeles") +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())

la.map_bw + 
  stat_density2d(data = filter(laparking_ytd, `Violation Description` == "NO PARK/STREET CLEAN", `RP State Plate` != "CA"), aes(x = New.Longitude, y = New.Latitude, fill = ..level.., alpha = ..level..),
                 geom = "polygon", size = 0.01) +
  scale_fill_gradient(low = "yellow", high = "red") + scale_alpha(guide = 'none') +
  labs(title = "Tickets Issued for No Parking (Street Cleaning)", subtitle = "Tickets issued from 1/1/2019 to 4/15/2019",
       caption = "Source: City of Los Angeles") +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())

# Red Zone
la.map_bw + 
  stat_density2d(data = filter(laparking_ytd, `Violation Description` == "RED ZONE"), aes(x = New.Longitude, y = New.Latitude, fill = ..level.., alpha = ..level..),
                 geom = "polygon", size = 0.01) +
  scale_fill_gradient(low = "yellow", high = "red") + scale_alpha(guide = 'none') +
  labs(title = "Tickets Issued for Red Zone Parking", subtitle = "Tickets issued from 1/1/2019 to 4/15/2019",
       caption = "Source: City of Los Angeles") +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())

la.map_bw + 
  stat_density2d(data = filter(laparking_ytd, `Violation Description` == "RED ZONE", `RP State Plate` != "CA"), aes(x = New.Longitude, y = New.Latitude, fill = ..level.., alpha = ..level..),
                 geom = "polygon", size = 0.01) +
  scale_fill_gradient(low = "yellow", high = "red") + scale_alpha(guide = 'none') +
  labs(title = "Tickets Issued for Red Zone Parking", subtitle = "Tickets issued from 1/1/2019 to 4/15/2019",
       caption = "Source: City of Los Angeles") +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())

# Over time limit
la.map_bw + 
  stat_density2d(data = filter(laparking_ytd, `Violation Description` == "PARKED OVER TIME LIMIT"), aes(x = New.Longitude, y = New.Latitude, fill = ..level.., alpha = ..level..),
                 geom = "polygon", size = 0.01) +
  scale_fill_gradient(low = "yellow", high = "red") + scale_alpha(guide = 'none') +
  labs(title = "Tickets Issued for Parking Over Time Limit", subtitle = "Tickets issued from 1/1/2019 to 4/15/2019",
       caption = "Source: City of Los Angeles") +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())

la.map_bw + 
  stat_density2d(data = filter(laparking_ytd, `Violation Description` == "PARKED OVER TIME LIMIT", `RP State Plate` != "CA"), aes(x = New.Longitude, y = New.Latitude, fill = ..level.., alpha = ..level..),
                 geom = "polygon", size = 0.01) +
  scale_fill_gradient(low = "yellow", high = "red") + scale_alpha(guide = 'none') +
  labs(title = "Tickets Issued for Parking Over Time Limit", subtitle = "Tickets issued from 1/1/2019 to 4/15/2019",
       caption = "Source: City of Los Angeles") +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())

# Preferential Parking
la.map_bw + 
  stat_density2d(data = filter(laparking_ytd, `Violation Description` == "PREFERENTIAL PARKING"), aes(x = New.Longitude, y = New.Latitude, fill = ..level.., alpha = ..level..),
                 geom = "polygon", size = 0.01) +
  scale_fill_gradient(low = "yellow", high = "red") + scale_alpha(guide = 'none') +
  labs(title = "Tickets Issued for Preferential Parking Violation", subtitle = "Tickets issued from 1/1/2019 to 4/15/2019",
       caption = "Source: City of Los Angeles") +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())

# Display of Plates
la.map_bw + 
  stat_density2d(data = filter(laparking_ytd, `Violation Description` == "DISPLAY OF PLATES"), aes(x = New.Longitude, y = New.Latitude, fill = ..level.., alpha = ..level..),
                 geom = "polygon", size = 0.01) +
  scale_fill_gradient(low = "yellow", high = "red") + scale_alpha(guide = 'none') +
  labs(title = "Tickets Issued for Improper Display of License Plates", subtitle = "Tickets issued from 1/1/2019 to 4/15/2019",
       caption = "Source: City of Los Angeles") +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())

# Dot Plot
la.map_bw + geom_point(data = laparking_ytd, aes(x = New.Longitude, y = New.Latitude), size = 0.01, alpha = 0.05, color = "blue") +
  labs(title = "Tickets Issued in the Greater Los Angeles Area", subtitle = "Tickets issued from 1/1/2019 to 4/15/2019",
       caption = "Source: City of Los Angeles") +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())

# Density with times
laparking_ytd %>%
  mutate(Time2 = as.POSIXct(paste("01-01-2019", laparking_ytd$`Issue time`, sep = " "), format="%m-%d-%Y %H%M")) %>%
  ggplot() + geom_density(aes(x = Time2)) + 
  scale_x_datetime(labels = date_format("%I:%M %p", tz = "EST"), date_breaks = "4 hours",
                   limits = c(as.POSIXct("2019-01-01 00:01:00 EST"), as.POSIXct("2019-01-01 23:59:00 EST"))) +
  theme_light() +
  labs(title = "Time Distribution of Parking Tickets in Los Angeles", subtitle = "Tickets issued from 1/1/2019 to 4/15/2019",
       caption = "Source: City of Los Angeles", x = "Time", y = "Density")

ggplotly(laparking_ytd %>%
  select(`Violation Description`, `Issue time`) %>%
  mutate(Time2 = as.POSIXct(paste("01-01-2019", laparking_ytd$`Issue time`, sep = " "), format="%m-%d-%Y %H%M")) %>%
  mutate(Violation2 = ifelse(`Violation Description` == "NO PARK/STREET CLEAN", "No Parking - Street Cleaning",
                             ifelse(`Violation Description` == "METER EXP.", "Meter Expired",
                                    ifelse(`Violation Description` == "PARKED OVER TIME LIMIT", "Over Time Limit",
                                           ifelse(`Violation Description` == "PREFERENTIAL PARKING", "Preferential Parking",
                                                  ifelse(`Violation Description` == "DISPLAY OF TABS", "Display of Tabs", NA)))))) %>%
  drop_na() %>%
  ggplot() + geom_density(aes(x = Time2, color = Violation2, fill = Violation2), alpha = 0.05, size = .7) + 
  scale_x_datetime(labels = date_format("%I:%M %p", tz = "EST"), date_breaks = "4 hours",
                   limits = c(as.POSIXct("2019-01-01 00:01:00 EST"), as.POSIXct("2019-01-01 23:59:00 EST"))) +
  theme_light() + 
  labs(title = "Time Distribution of Parking Tickets in Los Angeles by Violation", subtitle = "Tickets issued from 1/1/2019 to 4/15/2019",
       caption = "Source: City of Los Angeles", x = "Time", y = "Density", fill="Violation Type", color = "Violation Type"))

# Average Fine
laparking_ytd %>%
  select(`Violation Description`, `Issue time`, `Fine amount`) %>%
  mutate(Time2 = as.POSIXct(paste("01-01-2019", laparking_ytd$`Issue time`, sep = " "), format="%m-%d-%Y %H%M")) %>%
  mutate(Violation2 = ifelse(`Violation Description` == "NO PARK/STREET CLEAN", "No Parking - Street Cleaning",
                             ifelse(`Violation Description` == "METER EXP.", "Meter Expired",
                                    ifelse(`Violation Description` == "PARKED OVER TIME LIMIT", "Over Time Limit",
                                           ifelse(`Violation Description` == "PREFERENTIAL PARKING", "Preferential Parking",
                                                  ifelse(`Violation Description` == "DISPLAY OF TABS", "Display of Tabs", NA)))))) %>%
  group_by(Time2) %>%
  summarize(Fine = mean(`Fine amount`)) %>%
  drop_na() %>%
  ggplot() + geom_line(aes(x = Time2, y = Fine), color = "red", size = .5) + 
  scale_x_datetime(labels = date_format("%I:%M %p", tz = "EST"), date_breaks = "4 hours",
                   limits = c(as.POSIXct("2019-01-01 00:01:00 EST"), as.POSIXct("2019-01-01 23:59:00 EST"))) +
  theme_light() + 
  labs(title = "Average Dollar Value of Fine by Time", subtitle = "Tickets issued from 1/1/2019 to 4/15/2019",
       caption = "Source: City of Los Angeles", x = "Time", y = "Average Fine")


## Leaflet testing
leaflet_test <- laparking_ytd %>%
  filter(`Violation Description` == "DISPLAY OF PLATES") %>%
  select(New.Latitude, New.Longitude) %>%
  drop_na()
  
  
leaflet(leaflet_test) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addHeatmap(lng=~New.Longitude, lat=~New.Latitude, blur = 20, max = 0.05, radius = 20)

