# device to cell

library(tidyverse)
library(data.table)
library(sf)
library(furrr)
library(Matrix)

set.seed(2)

census.de.100m.tile <- readRDS("C:/Users/Marco/OneDrive - Universiteit Utrecht/MNO/working objects/census.tile.final.rds") %>% 
  dplyr::select(-c(cluster_id, pop.raster, cluster.tile.n))

coverage.areas <- readRDS("C:/Users/Marco/OneDrive - Universiteit Utrecht/MNO/working objects/coverage.areas.rds")


signal_strength <- Vectorize(function(distance, radius, max.equal = 0.7, min.threshold = 0.01) {
  sij.calc <- 1 - distance / radius
  
  if (str_detect(antenna.ID, "RT") & sij.calc > 0.7) {
    sij.calc <- sij.calc - 0.3 # sanction parameter for rural antennas 
  } else {
    sij.calc <- sij.calc
  }
  

  if (sij.calc < min.threshold) { # min.threshold (nu)
    sij <- as.numeric(0)
  } else if (sij.calc > max.equal) { # maximum sij where it doesnt make a difference anymore if closer
    sij <- as.numeric(1)
  } else {
    sij <- sij.calc
  }

  return(sij)
})


dev.to.cell <- census.de.100m.tile %>% 
  mutate(tile.centroid = st_centroid(geometry)) %>%
  st_join(coverage.areas, left = F) %>%
  st_transform(crs = 3035) %>% 
  st_sf(sf_column_name = "antenna.centroid", crs = 3035) %>% 
  mutate(dist.sij = as.numeric(st_distance(tile.centroid, antenna.centroid, by_element = T))) %>% 
  mutate(signal.sij = signal_strength(dist.sij, coverage.radius)) %>% # implement relevant signal strength formula --> faster way of doing this maybe in a grouped way?
  group_by(internal.id) %>%
  mutate(weight.pij = case_when(is.nan(as.numeric(signal.sij / sum(signal.sij, na.rm = T))) ~ as.numeric(0),
                                TRUE ~ as.numeric(signal.sij / sum(signal.sij, na.rm = T)))) %>% 
  ungroup()


saveRDS(dev.to.cell, "C:/Users/Marco/OneDrive - Universiteit Utrecht/MNO/working objects/dev.to.cell.final.better.threshold.rds")
dev.to.cell <- readRDS("C:/Users/Marco/OneDrive - Universiteit Utrecht/MNO/working objects/dev.to.cell.final.better.threshold.rds")


dev.to.cell.classified <- dev.to.cell %>% 
  dplyr::select(internal.id, antenna.ID, pop, weight.pij) %>% 
  # st_drop_geometry() %>%
  mutate(coverage.kind = case_when(pop == 0 ~ "0 population",
                                   pop >= 1 & weight.pij == 1 ~ "covered completely by one tile",
                                   pop >= 1 & weight.pij > 0 & weight.pij < 1 ~ "covered by multpile tiles",
                                   pop >= 1 & weight.pij == 0 ~ "tile uncovered sufficiently"))



# saveRDS(dev.to.cell.classified, "C:/Users/Marco/OneDrive - Universiteit Utrecht/MNO/working objects/dev.to.cell.classified.final.rds")
dev.to.cell.classified <- readRDS("C:/Users/Marco/OneDrive - Universiteit Utrecht/MNO/working objects/dev.to.cell.classified.final.better.threshold.rds")

#### Plots

# coverage.intensity <- dev.to.cell.classified %>% 
#   # sample_n(100000) %>%
#   filter(!weight.pij == 0) %>% 
#   group_by(internal.id) %>% 
#   summarise(n.cells.cover = n()) %>% 
#   arrange(n.cells.cover) %>% 
#   mutate(prob = 1 / n()) %>% 
#   mutate(cum.prob = cumsum(prob))
  # mutate(log10.cum.prob.comp = log10(1 - cum.prob)) %>% 
  # mutate(log10.n.cells.cover = log10(n.cells.cover))

# d %>%  
#   sample_n(10000) %>%
#   ggplot() +
#   geom_point(aes(x = n.cells.cover, y = n.cells.cover)) +
#   geom_hline(yintercept = -0.3010300, linetype = "dotted") +
#   geom_hline(yintercept = -1, linetype = "dotted") +
#   geom_text(x = 0.75, y = -0.2, label = "50% of the data") +
#   geom_text(x = 0.75, y = -0.9, label = "90% of the data") 


# Fig.coverage <- coverage.intensity %>%  
#   ggplot() + 
#   stat_count(aes(n.cells.cover), fill = "#4477A9") +
#   scale_x_continuous(breaks=c(1:11)) +
#   # geom_vline(yintercept = -0.3010300, linetype = "dotted") + implement half line
#   labs(y = "Count of tiles", x = "Covered by ... antennas", 
#        colour = "") +
#   theme(legend.position="bottom")

########

# Differentiating workflows between device to cell associations depending on coverage.kind variable
# One object where tiles are completely covered by one cell (no stochastic process)
C.vec.fixed.helper <- dev.to.cell.classified %>% 
  filter(coverage.kind == "covered completely by one tile") %>%
  st_drop_geometry() %>% 
  select(antenna.ID, pop)

# saveRDS(C.vec.fixed.helper, "C:/Users/Marco/OneDrive - Universiteit Utrecht/MNO/working objects/C.vec.multiple.helper.rds")
# C.vec.fixed.helper <- readRDS("C:/Users/Marco/OneDrive - Universiteit Utrecht/MNO/working objects/C.vec.multiple.helper.rds")

# One object where tiles are covered by multiple cells
C.vec.multiple.helper <- dev.to.cell.classified %>% 
  st_drop_geometry() %>% 
  filter(coverage.kind == "covered by multpile tiles") %>% 
  split(.$internal.id) 

# Dropping associations for tiles with 0 population and tiles which are not sufficiently covered (in this case the coverage network was optimized that every tile is sufficiently covered)

# saveRDS(C.vec.multiple.helper, "C:/Users/Marco/OneDrive - Universiteit Utrecht/MNO/working objects/C.vec.multiple.helper.rds")
# C.vec.multiple.helper <- readRDS("C:/Users/Marco/OneDrive - Universiteit Utrecht/MNO/working objects/C.vec.multiple.helper.rds")

# Calculate the number of cores
no_cores <- availableCores() - 1
plan(multisession, workers = no_cores)

set.seed(5)

C.vec.multiple <- C.vec.multiple.helper %>% 
  future_map(~sample(x = .$antenna.ID, mean(.$pop),
                     replace = T, prob = .$weight.pij), .progress = T) %>% 
  future_map(as_tibble, .id = "internal.id", .progress = T) %>% 
  future_map(~group_by(., value), .progress = T) %>% 
  future_map(~summarise(., pop.count.rand = n(), .groups = "drop"), .progress = T)

# saveRDS(C.vec.multiple, "C:/Users/Marco/OneDrive - Universiteit Utrecht/MNO/working objects/C.vec.multiple.rds")
# C.vec.multiple <- readRDS("C:/Users/Marco/OneDrive - Universiteit Utrecht/MNO/working objects/C.vec.multiple.rds")

C.vec.df <- C.vec.multiple %>% 
  bind_rows() %>% 
  select(antenna.ID = value, pop = pop.count.rand) %>% 
  bind_rows(C.vec.fixed.helper) %>% 
  group_by(antenna.ID) %>% 
  summarise(phones.sum = sum(pop))

# saveRDS(C.vec.df, "C:/Users/Marco/OneDrive - Universiteit Utrecht/MNO/working objects/C.vec.df.final.rds")
# C.vec.df <- readRDS("C:/Users/Marco/OneDrive - Universiteit Utrecht/MNO/working objects/C.vec.df.final.rds")
 
  
# sparse P matrix of device to cell
P.helper <- dev.to.cell %>% 
  dplyr::select(internal.id, antenna.ID, weight.pij) %>% 
  mutate_at(vars(c("internal.id", "antenna.ID")), factor) %>% 
  mutate(weight.pij = round(weight.pij, 4)) %>%
  st_drop_geometry()

P.mat <- sparseMatrix(i = as.numeric(P.helper$antenna.ID), 
                      j = as.numeric(P.helper$internal.id), 
                      x = P.helper$weight.pij,
                      dimnames = list(levels(P.helper$antenna.ID), levels(P.helper$internal.id)))


# saveRDS(Pcensus.de.100m.tile.mat, file = "C:/Users/Marco/OneDrive - Universiteit Utrecht/MNO/working objects/P.mat.rds")



# u-vector
U.vec <- census.de.100m.tile %>% 
  dplyr::select(internal.id, pop)

# saveRDS(U.vec, file = "C:/Users/Marco/OneDrive - Universiteit Utrecht/MNO/working objects/U.vec.rds")

