library(tidyverse)
library(data.table)
library(sf)
library(furrr)
library(Matrix)
library(ggthemes)

setwd("C:/Users/Marco/")

set.seed(2)

census.de.100m.tile <- readRDS("Vysoká škola ekonomická v Praze/Tony Wei Tse Hung - YAY/working objects/census.tile.final.rds") %>% 
  dplyr::select(-c(pop.true, cluster_id, pop.raster, cluster.tile.n))

coverage.areas <- readRDS("Vysoká škola ekonomická v Praze/Tony Wei Tse Hung - YAY/working objects/coverage.areas.rds")




signal_strength <- Vectorize(function(distance, loss.exp, max.equal = 0.85, min.threshold = 0.001) {
  sij.calc <- 1 - ((-10 * loss.exp * log10(distance))/ - 200)
  
  if (sij.calc < min.threshold) { # min.threshold (nu)
    sij <- as.numeric(0)
  } else if (sij.calc > max.equal) { # maximum sij where it doesnt make a difference anymore if closer
    sij <- as.numeric(1)
  } else {
    sij <- sij.calc
  }
  
  return(sij)
})


sim <- tibble(id = 1:15000, dist.sij = c(1:12000, 1:2500, 1:500), loss.exp = c(rep(5, 12000), rep(3, 2500), rep(2, 500)),
              area.kind = c(rep("Layer 1", 12000), rep("Layer 2", 2500), rep("Layer 3", 500))) %>% 
  mutate(signal.sij = signal_strength(distance = dist.sij, loss.exp = loss.exp))

labels <- c("Path Loss Exp. = 5", "Path Loss Exp. = 3", "Path Loss Exp. = 2")
names(labels) <- c("Layer 1", "Layer 2", "Layer 3")

signal.strength.plot <- sim %>% 
  ggplot() +
  geom_line(aes(x = dist.sij, y = signal.sij, color = area.kind), size = 1) +
  facet_grid(~area.kind, scales = "free",
            labeller = as_labeller(labels)) +
  scale_color_ptol() +
  guides(color = F) +
  labs(y = "Received signal strength (sij)", x = "Distance between cell and tile in m") +
  ggtitle("", subtitle = "Synthetic distribution sij") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 9, hjust = 0.5))
saveRDS(signal.strength.plot, "Vysoká škola ekonomická v Praze/Tony Wei Tse Hung - YAY/Plots/signal.strength.dist.plot.rds")



dev.to.cell.join <- census.de.100m.tile %>% 
  mutate(tile.centroid = st_centroid(geometry)) %>%
  st_join(coverage.areas, left = F) %>%
  st_transform(crs = 3035) %>% 
  st_sf(sf_column_name = "antenna.centroid", crs = 3035) %>% 
  mutate(dist.sij = as.numeric(st_distance(tile.centroid, antenna.centroid, by_element = T)))


saveRDS(dev.to.cell.join, "Vysoká škola ekonomická v Praze/Tony Wei Tse Hung - YAY/working objects/dev.to.cell.join.rds")
dev.to.cell.join <- readRDS("Vysoká škola ekonomická v Praze/Tony Wei Tse Hung - YAY/working objects/dev.to.cell.join.rds")

dev.to.cell.metric <- dev.to.cell.join %>% 
  # st_drop_geometry() %>% 
  select(internal.id, antenna.ID, pop, dist.sij, loss.exp) %>% 
  mutate(signal.sij = signal_strength(distance = dist.sij, loss.exp = loss.exp)) %>% 
  group_by(internal.id) %>%
  mutate(weight.pij = case_when(is.nan(as.numeric(signal.sij / sum(signal.sij, na.rm = T))) ~ as.numeric(0),
                                TRUE ~ as.numeric(signal.sij / sum(signal.sij, na.rm = T)))) %>% 
  ungroup()

saveRDS(dev.to.cell.metric, "Vysoká škola ekonomická v Praze/Tony Wei Tse Hung - YAY/working objects/dev.to.cell.metric.rds")
dev.to.cell.metric <- readRDS("Vysoká škola ekonomická v Praze/Tony Wei Tse Hung - YAY/working objects/dev.to.cell.metric.rds")


dev.to.cell.classified <- dev.to.cell.metric %>% 
  dplyr::select(internal.id, antenna.ID, pop, weight.pij) %>% 
  mutate(coverage.kind = case_when(pop == 0 ~ "0 population",
                                   pop >= 1 & weight.pij == 1 ~ "covered completely by one tile",
                                   pop >= 1 & weight.pij > 0 & weight.pij < 1 ~ "covered by multpile tiles",
                                   pop >= 1 & weight.pij == 0 ~ "tile uncovered sufficiently"))



saveRDS(dev.to.cell.classified, "Vysoká škola ekonomická v Praze/Tony Wei Tse Hung - YAY/working objects/dev.to.cell.classified.final.rds")
dev.to.cell.classified <- readRDS("Vysoká škola ekonomická v Praze/Tony Wei Tse Hung - YAY/working objects/dev.to.cell.classified.final.rds")

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
  st_drop_geometry() %>% 
  filter(coverage.kind == "covered completely by one tile") %>%
  dplyr::select(antenna.ID, pop)

saveRDS(C.vec.fixed.helper, "Vysoká škola ekonomická v Praze/Tony Wei Tse Hung - YAY/working objects/C.vec.fixed.helper.rds")
# C.vec.fixed.helper <- readRDS("Vysoká škola ekonomická v Praze/Tony Wei Tse Hung - YAY/working objects/C.vec.fixed.helper.rds")

# One object where tiles are covered by multiple cells
C.vec.multiple.helper <- dev.to.cell.classified %>% 
  st_drop_geometry() %>%
  filter(coverage.kind == "covered by multpile tiles") %>% 
  split(.$internal.id) 

# Dropping associations for tiles with 0 population and tiles which are not sufficiently covered (in this case the coverage network was optimized that every tile is sufficiently covered)

saveRDS(C.vec.multiple.helper, "Vysoká škola ekonomická v Praze/Tony Wei Tse Hung - YAY/working objects/C.vec.multiple.helper.rds")
# C.vec.multiple.helper <- readRDS("Vysoká škola ekonomická v Praze/Tony Wei Tse Hung - YAY/working objects/C.vec.multiple.helper.rds")

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

saveRDS(C.vec.multiple, "Vysoká škola ekonomická v Praze/Tony Wei Tse Hung - YAY/working objects/C.vec.multiple.rds")
# C.vec.multiple <- readRDS("Vysoká škola ekonomická v Praze/Tony Wei Tse Hung - YAY/working objects/C.vec.multiple.rds")

C.vec.df <- C.vec.multiple %>% 
  bind_rows() %>% 
  dplyr::select(antenna.ID = value, pop = pop.count.rand) %>% 
  bind_rows(C.vec.fixed.helper) %>% 
  group_by(antenna.ID) %>% 
  summarise(phones.sum = sum(pop))

saveRDS(C.vec.df, "Vysoká škola ekonomická v Praze/Tony Wei Tse Hung - YAY/working objects/C.vec.df.final.new.rds")
# C.vec.df <- readRDS("Vysoká škola ekonomická v Praze/Tony Wei Tse Hung - YAY/working objects/C.vec.df.final.new.rds")


# sparse P matrix of device to cell
P.helper <- dev.to.cell.classified %>% 
  dplyr::select(internal.id, antenna.ID, weight.pij) %>% 
  mutate_at(vars(c("internal.id", "antenna.ID")), factor) %>% 
  mutate(weight.pij = round(weight.pij, 4))

saveRDS(P.helper, file = "Vysoká škola ekonomická v Praze/Tony Wei Tse Hung - YAY/working objects/P.helper.new.rds")
# P.helper <- readRDS(file = "Vysoká škola ekonomická v Praze/Tony Wei Tse Hung - YAY/working objects/P.helper.new.rds")

P.helper.missing <- P.helper %>% 
  st_drop_geometry()

saveRDS(P.helper.missing, file = "Vysoká škola ekonomická v Praze/Tony Wei Tse Hung - YAY/working objects/P.helper.missing.rds")
# P.helper.missing <- readRDS(file = "Vysoká škola ekonomická v Praze/Tony Wei Tse Hung - YAY/working objects/P.helper.missing.rds")


P.mat <- sparseMatrix(i = as.numeric(P.helper$antenna.ID), 
                      j = as.numeric(P.helper$internal.id), 
                      x = P.helper$weight.pij,
                      dimnames = list(levels(P.helper$antenna.ID), levels(P.helper$internal.id)))


saveRDS(P.mat, file = "Vysoká škola ekonomická v Praze/Tony Wei Tse Hung - YAY/working objects/P.mat.rds")




# u-vector
U.vec <- census.de.100m.tile %>% 
  dplyr::select(internal.id, pop)

saveRDS(U.vec, file = "Vysoká škola ekonomická v Praze/Tony Wei Tse Hung - YAY/working objects/U.vec.rds")
