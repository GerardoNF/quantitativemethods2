#===============================
#Assignment 7: Spatial Data
#AQMS 2 - Francisco Villamil
#March 19th 2026
#================================
#List of packages
library(sf)
library(spData)
library(dplyr)
library(tidyr)
library(ggplot2)
setwd("/Users/gerardonaranjo/Desktop/quantitativemethods2/assignment7")
events = read.csv("conflict_events.csv")

#===============
#2.1 Convert tabular data to sf
#===============

#a) 
events_sf <- st_as_sf(
  events,
  coords = c("longitude", "latitude"),
  crs = 4326
)

class(events_sf)
st_crs(events_sf)
#The coords argument tells R which columns from the original dataset contain the
#geographic coordinates information. CRS = 4326 tells R which kind of projection
#we are using. This number refers to a very common type of projection which is
#based on latitude and longitude.

#b)
nrow(events_sf)
table(events_sf$event_type)

#There are 68354 total events captured in the data. The most common type of event 
#is the state-based type, with 33487 observations. 

#c) 
data(world)

ggplot() +
  geom_sf(data = world, fill = "grey90", color = "white") +
  geom_sf(data = events_sf, aes(color = event_type)) +
  theme_void () +
  labs (title = "Conflict Events Worldwide")
ggsave("events_world_map.png")

#It seems as if all of the events are concentrated in Africa 

#=====================================
#2.2 Spatial Join: Events to countries
#=====================================

#a) Matching countries to the polygon they match to
st_crs(world)
st_crs(events_sf)
#They are both on the EPSG 4326
events_joined <- st_join(events_sf,world)
nrow(events_joined)
#The number of rows across data sets does match, meaning that the join happened
#with no problem

#b) Checking for events with no match
sum(is.na(events_joined$name_long))
mean(is.na(events_joined$name_long))
#The total number of events with no country match is 1576.

#c) Events and total fatalities per country
events_clean<-events_joined %>%
  filter(!is.na(name_long))

country_summary<- events_clean %>%
  group_by(name_long) %>%
  summarise(
    n_events = n(),
    total_fatalities = sum(fatalities, na.rm = TRUE)
  ) %>%
  arrange(desc(n_events))

st_drop_geometry(country_summary) %>%
  head(10)

#Some aspects of the results are consistent with my --limited-- knowledge of 
#of conflict. While cases of Rwanda, Congo, or Burundi ring a bell in terms of 
#conflict consistency and total fatalities, I was not expecting a country like
#South Africa in the top 10 of this indicator. Similarly, I consider this data set
#limited to Africa, but if it truly was "worldwide" as suggested in the title then
#I would expect the presence of other countries in SE Asia (and maybe Latam) to be
#present here. 

#======================================
#2.3 Choropleth of conflict intensity
#======================================

#a) Joining event counts to world polygon
country_summary_df<- st_drop_geometry(country_summary)

world_conflict<- world %>%
  left_join(country_summary_df, by = c("name_long" = "name_long"))

world_conflict$n_events <- replace_na(world_conflict$n_events, 0)

nrow(world_conflict)
nrow(world)
#The number of observations for both matches

#b) Making new map
ggplot(world_conflict) +
  geom_sf(aes(fill = n_events)) +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  theme_void() +
  labs (title = "Conflict Events by Country")

ggsave("conflict_raw_map.pdf", width = 10, height = 5)

#While the geographic pattern is still aligned with the original map, this provides
#a much clearer picture of which countries are most heaviyl affected by violent
#conflict. 

#c) New map using log-counts
ggplot(world_conflict) +
  geom_sf(aes(fill = log1p(n_events))) +
  scale_fill_distiller(palette = "Yl0Rd", direction = 1,
                       name = "Log(events + 1)") +
  theme_void() +
  labs (title = "Log-Transformed Conflict Events")

ggsave("conflict_log_map.pdf", width = 10, height = 5)

#Log transformation may be useful in that it reduces skewness brought forward
#by raw counting events. When portraying this in a map, it makes slight distinctions
#between countries clearer, allowing for a slightly more detailed visualization
#of how the conflicts are distributed along Africa.

#================================
#2.4 Optional: Capital City Analysis
#================================

#a and b) Subset of events in Nigeria
events_nigeria<-events_joined %>%
  filter(name_long == "Nigeria")
nrow(events_nigeria)

#There are 7166 events recorded in Nigeria

#c) Dataframe for Abuya
abuya_df<- data.frame(
  name = "Abuya",
  longitude = 7.49508,
  latitude = 9.05785
)

abuya_sf<-st_as_sf(
  abuya_df,
  coords = c("longitude", "latitude"),
  crs = 4326
)
#d) Both spatial objects into UTM projection
nigeria_utm<-st_transform(events_nigeria, 32632)
abuya_utm<-st_transform(abuya_sf, 32632)

#e) Calculating distances
dist_matrix<-st_distance(nigeria_utm, abuya_utm)

nigeria_utm$distance_m<-as.numeric(dist_matrix)
#Into km
nigeria_utm$distance_km<-nigeria_utm$distance_m / 1000
nigeria_utm$log_km<-log1p(nigeria_utm$distance_km)
nigeria_utm$log_fatalities<-log1p(nigeria_utm$fatalities)

#f) Running the regression
m_1<-lm(log_fatalities ~ log_km, data=nigeria_utm)
summary(m_1)
#It seems that the farther away a conflict is from the capital, the less fatalities
#observed. However this is not statistically significant.

m_2<-lm(log_fatalities ~ log_km + event_type, data = nigeria_utm)
summary(m_2)
#When controlling for event type, the association with distance becomes positive,
#now suggesting that a 1% increase in distance yields 0.08% more deaths. 
#Also, the effect becomes statistically significant. The type of event holds more
#explanatory power and it also seems to have a stronger association. 

m_3<-lm(log_fatalities ~ log_km*event_type, data=nigeria_utm)
summary(m_3)
#The coefficients obtained suggest that the relationship between distance from the
#capital and fatalities vary significantly by event type. Whereas non-state conflicts
#are expected to be less deadly the farther away they get from the capital, one=sided
#and state-based events display an increase of 0.18 and 0.33% respectively for every
#1% increase in distance from the capital. 
#This is aligned with the theoretical expectation posed forward in the assignment 
#regarding reduced state capacity in more distant areas from the capital.

#==============================
#2.5 Discussion
#=============================

#a)
#Regarding the concern for potential slight imprecisions in calculations which may
#complicate coding of events inside their respective polygon, I wonder whether 
#an error component can be incorporated into the code and account for these slight
#variations. As for events that falle exactly on the border between two countries, 
#I think manual decisions need to be made regarding which country it should be coded
#into

#b)
#From my understanding, st_join is used to handle spatial data while left_join is
#more appropriate for textual information. Therefore, st_join is prefered when 
#merging two sets of spatial information into one standard, visualizable model. 
#Conversely, left_join is used to match observations from two data frames in which
#coincidences are found textually (using the 1st data frame as reference, as opposed
#to using the 2nd one as would be the case in right_join)




