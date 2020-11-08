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
tiles.cat <- dev.to.cell.classified %>% 
  filter(!weight.pij == 0) %>% 
  dplyr::select(internal.id, coverage.kind) %>% 
  st_drop_geometry() %>% 
  group_by(internal.id) %>% 
  summarise(count = n())

# how many tiles are not sufficiently covered
missings <- anti_join(census.de.100m.tile, tiles.cat, by = "internal.id") 

# workflow: spanning a buffer of 700m over each tile and finding groups that intersect with each other --> clusters
missings.buffer <- st_buffer(missings, 700)
missings.int <- st_intersects(missings.buffer, missings)
missings.graph <- graph.adjlist(missings.int)
missings.clust <- components(missings.graph)
table(missings.clust$membership)

# summariszing their geometries and their tile size of each cluster
missings.clustered <- missings %>% 
  mutate(group.tower = missings.clust$membership) %>% 
  group_by(group.tower) %>% 
  summarize(geometry = st_union(geometry), count.tiles = n())

# manually placing towers at the centroid of each cluster and then moving them partially down to place coverage areas over them missing tiles
coordinates <- missings.clustered %>% 
  mutate(tower.kind = case_when(count.tiles >= 500 ~ "Rural",
                                TRUE ~ "Suburban")) %>% # speccifiyng which kind of tower needs to be placed
  mutate(tower.position = st_centroid(geometry) - c(1000, 1800)) %>% # adjusting the deviance of the tower centroid to get the tiles close to the antennas centroid in ordr to pass the threshold
  st_drop_geometry() %>% 
  st_sf(sf_column_name = "tower.position", crs = 3035) 

# visual analysis: detect missing clusters
missings %>% 
  ggplot() +
  geom_sf(data = missings.buffer, fill = "green") +
  geom_sf()

# Visual analysis if tower positions apply correct
coverage.areas %>% 
  filter(area.kind == "Suburban") %>% # for good visibility
  ggplot() +
  geom_sf(aes(col = area.kind), fill = NA) +
  geom_sf(data = missings) +
  geom_sf(data = coordinates)

# Save the adjusted tower position coordinates to bind_rows in 2_Radio cell generation
coordinates.rural.final <- coordinates %>% 
  # filter(tower.kind == "Rural") %>% 
  st_coordinates() %>% 
  as_tibble()

# coordinates.suburban.final <- coordinates %>% 
#   filter(tower.kind == "Suburban") %>% 
#   st_coordinates() %>% 
#   as_tibble()

# coordinates.suburban.final.second <- coordinates %>% 
#   filter(tower.kind == "Suburban") %>% 
#   st_coordinates() %>% 
#   as_tibble()



saveRDS(coordinates.rural.final, "C:/Users/Marco/OneDrive - Universiteit Utrecht/MNO/working objects/manually placed towers.rural.rds")  
# saveRDS(coordinates.suburban.final, "C:/Users/ramljak/Desktop/marco/manually placed towers.suburban.rds")  
# saveRDS(coordinates.suburban.final.second, "C:/Users/ramljak/Desktop/marco/manually placed towers.suburban.second.rds")  
