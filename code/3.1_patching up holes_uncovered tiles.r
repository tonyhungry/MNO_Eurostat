### Patching up holes (finding the locations of tiles that are not covered sufficiently and generating tower positions that will be added manually in 2)
# Information from 3_device to cell is necessary, therefore it is not integrated in 2_Radio cell generation directly

library(sf)
library(tidyverse)
library(igraph)

census.de.100m.tile <- readRDS("C:/Users/Marco/OneDrive - Universiteit Utrecht/MNO/working objects/census.tile.final.rds") %>% 
  dplyr::select(-c(cluster_id, pop.raster, cluster.tile.n))

dev.to.cell.classified <- readRDS("C:/Users/Marco/OneDrive - Universiteit Utrecht/MNO/working objects/dev.to.cell.classified.rds")

coverage.areas <- readRDS("C:/Users/Marco/OneDrive - Universiteit Utrecht/MNO/working objects/coverage.areas.rds")


# aggregating and specifing the tiles that are uncovered 
tiles.cat.old <- dev.to.cell.classified.old %>% 
  filter(!weight.pij == 0) %>% 
  dplyr::select(internal.id, coverage.kind) %>% 
  st_drop_geometry() %>% 
  group_by(internal.id) %>% 
  summarise(count = n())

tiles.cat <- dev.to.cell.classified %>% 
  filter(!weight.pij == 0) %>% 
  dplyr::select(internal.id, coverage.kind) %>% 
  st_drop_geometry() %>% 
  group_by(internal.id) %>% 
  summarise(count = n())

# the uncovered tiles resemble three clusters
missings.old <- anti_join(census.de.100m.tile, tiles.cat.old, by = "internal.id") 
missings <- anti_join(census.de.100m.tile, tiles.cat, by = "internal.id") 

missings.buffer <- st_buffer(missings, 100)
missings.int <- st_intersects(missings.buffer, missings)
missings.graph <- graph.adjlist(missings.int)
missings.clust <- components(missings.graph)
table(missings.clust$membership)

%>% 
  mutate(group.tower = case_when(str_detect(internal.id, "^5|^6") ~ "1",
                                 str_detect(internal.id, "^12") ~ "2",
                                 str_detect(internal.id, "^17") ~ "3"))


# detect missing clusters
missings %>% 
  ggplot() +
  geom_sf(data = missings.buffer, fill = "green") +
  geom_sf()

aes(col = group.tower))

# spatially aggregate the clusters and develop the adjusted tower position
coordinates <- missings %>% 
  mutate(group.tower = missings.clust$membership) %>% 
  group_by(group.tower) %>% 
  summarize(geometry = st_union(geometry)) %>% 
  mutate(tower.position = st_centroid(geometry) - 1800) %>% # adjusting the deviance of the tower centroid to get the tiles close to the antennas centroid in ordr to pass the threshold
  st_drop_geometry() %>% 
  st_sf(sf_column_name = "tower.position", crs = 3035) 

# Visual analysis if tower positions apply correct
coverage.areas %>% 
  filter(area.kind == "Suburban") %>% # for good visibility
  ggplot() +
  geom_sf(aes(col = area.kind), fill = NA) +
  geom_sf(data = missings) +
  geom_sf(data = coordinates)

# Save the adjusted tower position coordinates to bind_rows in 2_Radio cell generation
coordinates.final <- coordinates %>% 
  st_coordinates() %>% 
  as_tibble()


saveRDS(coordinates.final, "C:/Users/Marco/OneDrive - Universiteit Utrecht/MNO/working objects/manually placed towers.rds")  
  
  
  