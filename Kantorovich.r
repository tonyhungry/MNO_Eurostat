installr::install.packages.zip("https://github.com/eurostat/Spatial-KWD/releases/download/v0.2.0-alpha/SpatialKWD_0.2.0.zip")


library(SpatialKWD)
library(tidyverse)
library(sf)
library(data.table)


setwd("C:/Users/Marco/")

# Loading In
voro <- readRDS("Vysoká škola ekonomická v Praze/Tony Wei Tse Hung - YAY/Estimates/Voronoi both/Voronoi.estimates.rds")
mle <- readRDS("Vysoká škola ekonomická v Praze/Tony Wei Tse Hung - YAY/Estimates/P.equal/u.est.non.inf.P.equal.part801_1000_new.rds")
mle.equal.1000 <- mle %>% 
  select(internal.id = j, mle.equal = u200)

census.de.100m.tile <- readRDS("Vysoká škola ekonomická v Praze/Tony Wei Tse Hung - YAY/working objects/census.tile.final.rds") 
census.new <- census.de.100m.tile %>% 
  dplyr::select(internal.id, pop) %>% 
  # filter(internal.id <= 1000) %>% 
  st_make_grid(cellsize = 400) # the size in square meters per cell --> 400 is ca. 4x4

new <- census.de.100m.tile %>% 
  select(internal.id, true.pop = pop) %>% 
  left_join(voro, by = "internal.id") %>% 
  left_join(mle.equal.1000, by = "internal.id") %>% 
  dplyr::select(true.pop, voronoi.est.antenna, voronoi.est.tower, mle.equal) %>% 
  aggregate(by = census.new, FUN = sum, join = st_contains) %>% 
  mutate(lon = map_dbl(geometry, ~st_centroid(.x)[[1]]),
         lat = map_dbl(geometry, ~st_centroid(.x)[[2]])) %>% 
  st_drop_geometry()

coordinates.new <- new %>% 
  select(lon, lat) %>% 
  as.matrix()

weights.new <- new %>% 
  select(true.pop, voronoi.est.tower, voronoi.est.antenna, mle.equal) %>% 
  as.matrix()



# d <- compareOneToOne(coordinates.new, Weights = weights.new, L = 3, recode=TRUE)
# cat("runtime:", d$runtime, " distance:", d$distance, " nodes:", d$nodes, " arcs:", d$arcs, "\n")


d.all <- compareOneToMany(coordinates.new, Weights = weights.new, L = 2, recode = TRUE)
cat("runtime:", d.all$runtime, " distance:", d.all$distance, " nodes:", d.all$nodes, " arcs:", d.all$arcs, "\n")


