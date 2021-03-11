### Making Maps with R using maps and ggplot ###
library(tidyverse)
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)

#### Load in data to work with ####
df <- read_csv("StateAndCountyData.csv")
head(df)

# Data Transformation (we need to make counties lower case to left join and the state variable is of no value after join)
df <- df %>% 
  mutate(County = tolower(County)) 
df
colnames(df)[3] <- "subregion"
colnames(df)[5] <- "population"

##### County lines in every state #####

# Get State Lines
states <- map_data("state")
head(states)

# Create Base Map
states_base <- states %>% 
  ggplot(aes(x = long, y = lat, fill = region, group = group),) +
  geom_polygon(color = "black", fill = "gray") + 
  coord_quickmap() +
  guides(fill = FALSE)
states_base

# Get county lines
counties <- map_data("county") 
head(counties)

# Draw County Lines
usa_base <- states_base + theme_void() + 
  geom_polygon(data = counties, fill = NA, color = "white") +
  geom_polygon(color = "black", fill = NA)  # get the state border back on top
usa_base

# filter data of choice, in this case low access to food for the entire country in 2010
low_access_2010 <- df %>% 
  filter(Variable_Code == "LACCESS_POP10")
head(low_access_2010)

# Left Join the counties data for usa and the low access to food data 
head(counties)
head(low_access_2010)
low_access_2010 <- left_join(counties, low_access_2010, by = "subregion")
head(low_access_2010)

# Plot the joined data on the map, the usa_base map above is important
low_access_2010_map <- usa_base + 
  geom_polygon(data = low_access_2010, aes(fill = population, group = group), color = "white") +
  geom_polygon(color = "black", fill = NA) +
  theme_void()
low_access_2010_map

# Make colors on map better
# use the viridis library to change color scheme 
library(viridis)
low_access_2010_map <- low_access_2010_map + 
  scale_fill_viridis(breaks = c(2, 4, 10, 100, 1000, 10000),
                     trans = "log10")
low_access_2010_map



##### County lines in a particular state #####

# California
ca <- states %>% 
  filter(region == "california")
head(ca)

# Create a base map for California
ca_base <- ggplot(data = ca, aes(x = long, y = lat, group = group)) + 
  coord_quickmap() +
  geom_polygon( color = "black", fill = "gray") 
ca_base + theme_void()


# filter out california counties
ca_county <- counties %>%
  filter(region == "california")
head(ca_county)

# Plot the counties
ca_base + 
  theme_void() + 
  geom_polygon(data = ca_county, fill = NA, color = "white") +
  geom_polygon(color = "black", fill = NA)  # get the state border back on top

# filter data of choice, in this case low access to food for the entire country in 2010
ca_low_access_2010 <- df %>% 
  filter(Variable_Code == "LACCESS_POP10") %>% 
  filter(State == "CA")
head(low_access_2010)

# Connect the data with the map with left join
ca_low_access_2010 <- left_join(ca_county, ca_low_access_2010, by = "subregion")
head(ca_low_access_2010)

# Plot the data on the map, the ca_base map above is important
ca_low_access_2010_map <- ca_base + 
  geom_polygon(data = ca_low_access_2010, aes(fill = population, group = group), color = "white") +
  geom_polygon(color = "black", fill = NA) +
  theme_void()
ca_low_access_2010_map

# use the viridis library to change color scheme 
library(viridis)
ca_low_access_2010_map <- ca_low_access_2010_map + 
  scale_fill_viridis(breaks = c(2, 4, 10, 100, 1000, 10000),
                     trans = "log10")
ca_low_access_2010_map

