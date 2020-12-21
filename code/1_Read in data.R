# MNO Chain 
## 1 Read in of raw census data

library(tidyverse)
library(data.table)
library(sf)
library(raster)
library(furrr)
library(stars)
library(osc)

# Raw census 100 m^2 data read in
census.raw <- fread("C:/Users/Marco/OneDrive - Universiteit Utrecht/MNO/Data/Census data Germany/csv_Bevoelkerung_100m_Gitter/Zensus_Bevoelkerung_100m-Gitter.csv")

# Dataframe with bounding box, tile id, and two versions of the population variable
census.de.100m <- census.raw %>% 
  dplyr::select(x = x_mp_100m, y = y_mp_100m, pop.raw = Einwohner) %>% 
  filter(between(y, 2700000, 2900000), # 285
         between(x, 4400000, 4500000)) %>%
  mutate(internal.id = row_number()) %>%
  mutate(pop.true = case_when(pop.raw == "-1" | is.na(pop.raw) ~ sample(0:1, n(), replace = T),
                              pop.raw %in% c(2:3) ~ sample(2:3, n(), replace = T),
                              TRUE ~ as.integer(pop.raw))) %>% 
  mutate(pop = case_when(pop.true <= 12 ~ sample(0:4, n(), prob = c(3/4, 3/16, 3/64, 3/256, 1/256), replace = T),
                         pop.true > 12 ~ as.integer(round(pop.true / 3, 0)))) %>%  # reducing population by a third standing for one MNO provider population
  mutate(pop.raster = case_when(pop < 15 ~ 0,
                                pop >= 15 ~ 1)) %>%  # defining the pop threshold per tile
  dplyr::select(-pop.raw)

saveRDS(census.de.100m, "C:/Users/Marco/OneDrive - Universiteit Utrecht/MNO/working objects/example data frame.rds")
# census.de.100m <- readRDS("C:/Users/Marco/OneDrive - Universiteit Utrecht/MNO/working objects/example data frame.rds")

# Raster brick object of the complete bounding box region
census.tile <- raster::rasterFromXYZ(census.de.100m, crs = st_crs(3035)$proj4string)

# Raster layer object with specified dichotomized variable for cca
census.tile.pop.raster <- raster::raster(census.tile, layer = 4) 

# CCA workflow, clustering 1 values (pop > 15) and define s
cities <- cca(census.tile.pop.raster, cell.class = 1, s = 11100000, unit = "m") 

# Adding classification results to census sf object and splitting it up for further parallelized workflow
census.classified.sf <- cities[["cluster"]] %>% 
  right_join(census.de.100m, by = c("long" = "x", "lat" = "y")) %>% 
  mutate(parts = ntile(internal.id, 50)) %>%
  group_by(parts) %>%
  group_split()

# Calculate the number of cores
no_cores <- availableCores() - 1
plan(multisession, workers = no_cores)

# Rasterizing the sf object with cluster results and transforming back to final sf object
census.classified.sf.transform <- census.classified.sf %>% 
  future_map(~raster::rasterFromXYZ(., crs = st_crs(3035)$proj4string), .progress = T) %>% 
  future_map(~st_as_stars(.), .progress = T) %>% 
  future_map_dfr(~st_as_sf(., coords = c("long", "lat")), .progress = T) %>%
  st_transform(crs = 3035) %>% 
  group_by(cluster_id) %>% 
  mutate(cluster.tile.n = n()) %>% 
  ungroup() %>% 
  dplyr::select(-parts) %>% 
  mutate(pop.area.kind.helper = case_when(pop.raster != 0 & cluster.tile.n > 10 ~ 1, # specify clusters that have at least fifty tiles
                                          TRUE ~ 0))  

# summarise clusters geometries to build buffers
result.interim <- census.classified.sf.transform %>% 
  filter(pop.area.kind.helper == 1 & !is.na(cluster_id)) %>% 
  group_by(cluster_id) %>% 
  summarise(geometry = st_union(geometry))

# build buffers for suburban and urban area respectively
urban.buffer <- st_buffer(result.interim, 800)
suburban.buffer <- st_buffer(result.interim, 3000)

# classifiy tiles that are within the respective buffer with either suburban or urban, rest is rural
census.classified.final.sf <- census.classified.sf.transform %>% 
  mutate(urban.dummy = lengths(st_within(census.classified.sf.transform, urban.buffer))) %>% 
  mutate(suburban.dummy = lengths(st_within(census.classified.sf.transform, suburban.buffer))) %>% 
  mutate(pop.area.kind = case_when(urban.dummy > 0 ~ "Urban",
                                   suburban.dummy > 0 & urban.dummy == 0 ~ "Suburban",
                                   TRUE ~ "Rural"))


# check if correctly classified
census.classified.final.sf %>% 
  filter(!pop.area.kind == "Rural") %>% 
  ggplot() +
  geom_sf(aes(color = pop.area.kind))


saveRDS(census.classified.final.sf, "C:/Users/Marco/OneDrive - Universiteit Utrecht/MNO/working objects/census.tile.final.rds")
census.classified.final.sf <- readRDS("Vysoká škola ekonomická v Praze/Tony Wei Tse Hung - YAY/working objects/census.tile.final.rds")


cluster.plot <- census.classified.final.sf %>%
  filter(pop.area.kind %in% c("Suburban", "Urban")) %>% 
  ggplot() +  
  geom_sf(aes(color = factor(pop.area.kind), fill = factor(pop.area.kind)),  show.legend = F) + 
  ggtitle("", subtitle = "Clustering Results") + 
  theme(plot.margin = unit(c(0, 0, 0, 0), "mm")) +  
  scale_color_manual(breaks = c("Suburban", "Urban"), values = c("#DDCC77", "#CC6677")) +
  labs(subtitle = "Fig. 2b: Clustering results") +
  theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 9, hjust = 0.5))

saveRDS(cluster.plot, "Vysoká škola ekonomická v Praze/Tony Wei Tse Hung - YAY/working objects/cluster.plot.rds")


######################################################

### Excurse: Checking the results of the cca (adjustment of parameters)
result <- census.tile.pop.raster * 0
result[cellFromXY(result, cities$cluster[, 1:2])] <- cities$cluster[, 3]
result.cca <- brick(census.tile.pop.raster2, result)

# Number of clusters and how many tiles does each cluster entail
result.summary <- cities[["cluster"]] %>% 
  group_by(cluster_id) %>% 
  summarise(n())

# Classifying tiles (pop.raster == 1) and transforming to sf object, will be used to join below
result.sf <- st_as_stars(result) %>% 
  st_as_sf() %>%
  st_transform(crs = 3035) %>% 
  group_by(pop.raster) %>% 
  mutate(pop.raster.n = n()) %>% 
  ungroup() %>% 
  mutate(area.kind = case_when(pop.raster =! 0 & pop.raster.n > 100 ~ "Urban", # cluster with at least 100 tiles is urban
                               pop.raster =! 0 & pop.raster.n > 50 & pop.raster.n <= 100 ~ "Suburban", # cluster with at least fifty and below 100 tiles is suburban
                               TRUE ~ NA_character_))  # Remaining tiles are considered as rural
