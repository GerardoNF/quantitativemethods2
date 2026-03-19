
#===============================
#Assignment 7: Spatial Data
#AQMS 2 - Francisco Villamil
#March 19th 2026
#================================

#List of packages
install.packages("sf")
library(sf)
install.packages("spData")
library(spData)
library(dplyr)
library(tidyr)
library(ggplot2)

#==============================
#Part 1 In-Class (Exploring Spatial Data with sf)
#==============================

data(world)

#==================================
#1. Inspecting an sf object
#----------------------------------

#a)
class(world)
names(world)
nrow(world)

#class(world) returns both "sf" and "data.frame": an sf object is a regular data 
#frame augmented with an extra geometry column (of class sfc) that stores the 
#spatial shapes (polygons, points, lines). The geometry column is “sticky” — standard 
#dplyr operations (filter, mutate, select) retain it automatically, so spatial 
#attributes travel with the data without any extra effort.

#b)
st_crs(world)
#The dataset uses EPSG:4326 (WGS84 — World Geodetic System 1984). WGS84 is the 
#global standard coordinate system used by GPS and most web mapping tools. 
#Coordinates are expressed in decimal degrees of longitude (east-west) and latitude 
#(north-south), making it suitable for global datasets where a common datum is needed
#across all regions.

#c)
unique(st_geometry_type(world))
#The geometry type is MULTIPOLYGON. A MULTIPOLYGON is a collection of one or more 
#polygons treated as a single geographic feature. Countries require multiple polygons 
#when their territory is not a single contiguous land mass —for example, the 
#United States includes Alaska and Hawaii as separate polygons, and France 
#includes overseas territories such as Martinique and Guadeloupe in the Caribbean.

#d)
pdf("world_gdp_base.pdf")
plot(world["gdpPercap"])
dev.off()
# Display inline as well
plot(world["gdpPercap"], main = "GDP per capita by country")

#The map reveals a sharp global inequality pattern. Western and Northern Europe, 
#North America, and Australia/New Zealand appear as the wealthiest regions (dark 
#end of the scale). Sub-Saharan Africa and parts of South and Southeast Asia occupy 
#the lowest end. East Asia shows intermediate-to-high values, reflecting rapid economic
#growth in countries such as South Korea and Japan.

#================================
#2. Attribute operations
#================================

#a)
africa = filter(world, continent == "Africa")
nrow(africa)
plot(africa["gdpPercap"], main = "GDP per capita -- Africa")
#The dataset contains 51 African countries. The UN recognizes 54 sovereign African 
#states, so this count is slightly below expectations and likely reflects missing 
#data or the exclusion of very small territories from the spData world polygon dataset

#b)
world = world %>%
  mutate(pop_millions = pop / 1e6)
gdp_by_continent = world %>%
  group_by(continent) %>%
  summarise(mean_gdpPercap = mean(gdpPercap, na.rm = TRUE))
print(st_drop_geometry(gdp_by_continent))

#When summarise() is called on a grouped sf object, it unions the geometries within 
#each group and retains the resulting geometry column. To obtain a plain data 
#frame without spatial information, use st_drop_geometry() before or after the 
#summary step. This avoids carrying unneeded geometry through purely tabular analyses.

#c)
africa_sorted = africa %>%
  arrange(desc(gdpPercap)) %>%
  select(name_long, gdpPercap)
print(head(st_drop_geometry(africa_sorted), 5))

#The five African countries with the highest GDP per capita in this dataset are 
#shown above. Equatorial Guinea ranks high due to its oil revenues relative to a 
#small population; Gabon and Libya are also oil-dependent economies; Botswana 
#benefits from diamond exports and relatively strong institutions; the fifth 
#position is typically taken by a North African economy (Mauritius or Algeria 
#depending on the dataset vintage).

#========================
#3. Simple visualization with ggplot2
#========================

#a)
ggplot(world) +
  geom_sf(aes(fill = gdpPercap)) +
  scale_fill_viridis_c(option = "plasma", na.value = "grey80",
                       name = "GDP per capita") +
  theme_void() +
  labs(title = "GDP per capita by country")
ggsave("world_gdp.pdf", width = 10, height = 5)

#The geographic pattern mirrors what the base-R map showed. Western Europe, 
#North America, and Oceania stand out as the wealthiest cluster. East Asia shows 
#a gradient from high (Japan, South Korea) to middle (China).Sub-Saharan Africa 
#and South Asia concentrate the lowest values, with a few exceptions 
#(e.g., Equatorial Guinea’s oil wealth).

#b) 
ggplot(africa) +
  geom_sf(aes(fill = gdpPercap)) +
  scale_fill_viridis_c(option = "magma", na.value = "grey80",
                       name = "GDP per capita") +
  theme_void() +
  labs(title = "GDP per capita -- Africa")
ggsave("africa_gdp.pdf", width = 7, height = 6)
#Within Africa, there is substantial variation. A cluster of relatively wealthier 
#countries appears in North Africa (Egypt, Libya, Tunisia) and in Southern Africa 
#(Botswana, South Africa, Namibia). Central and West Africa (with the exception of 
#oil-rich Equatorial Guinea and Gabon) display the lowest values, reflecting 
#low diversification anpersistent structural poverty.

#c)
ggplot(africa) +
  geom_sf(aes(fill = gdpPercap), color = "white", linewidth = 0.3) +
  scale_fill_viridis_c(option = "magma", na.value = "grey80",
                       name = "GDP per capita") +
  theme_void() +
  labs(title = "GDP per capita -- Africa (with borders)")
ggsave("africa_gdp_borders.pdf", width = 7, height = 6)

#Adding white country borders significantly improves readability, especially for 
#smaller countries where adjacent fill colours alone make it hard to distinguish 
#units. The thin white lines demarcate each country without competing visually with 
#the fill scale, making it easier to identify specific countries of interest and 
#to compare neighbours.
