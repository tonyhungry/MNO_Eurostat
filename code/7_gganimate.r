## here we build the plots for the geograhical evaluation. For every final estimation we build 
## plot based on fixed tile categories based on the number of mobile phones. This means we are
## visualizing the tile estiamnds. The performance measure for this evaluation is the visual
## resemblance of the tile categories of the estimands and the tile categories of the actual
## true population values for each tile. therefore we will build the same plot for the true
## population and set them in the final notebook side by side to make them easily comparable

library(tidyverse)
library(data.table)
library(sf)
library(gganimate)
library(ggthemes)
library(transformr)


setwd("C:/Users/Marco/")

## Data prep and plot for the true population map
census.de.100m.tile <- readRDS("Vysoká škola ekonomická v Praze/Tony Wei Tse Hung - YAY/working objects/census.tile.final.rds") 
census.geo <- census.de.100m.tile %>% 
  dplyr::select(internal.id, pop) %>% 
  arrange(internal.id)

true.pop.data <- census.de.100m.tile %>%
  mutate(pop.cat = case_when(pop <= 1 ~ "Less or equal 1",
                             pop > 1 & pop <= 10 ~ "Between 2 and 10",
                             pop > 10 & pop <= 30 ~ "Between 11 and 30",
                             pop > 30 ~"More than 30")) %>% 
  mutate(mobile = factor(pop.cat, levels = c("Less or equal 1", "Between 2 and 10", "Between 11 and 30", "More than 30"))) %>%
  group_by(mobile) %>% 
  summarise(geometry = st_union(geometry))
saveRDS(true.pop.data, "Vysoká škola ekonomická v Praze/Tony Wei Tse Hung - YAY/Estimates/Plot.files/true.pop_data.rds")

true.pop.plot <- true.pop.data %>% 
  ggplot() +
  geom_sf(aes(fill = factor(mobile), color = factor(mobile))) +
  labs(x = "", y = "", title = "", subtitle = "True population value", size = 20) +
  scale_fill_ptol(name = "Mobile phones") +
  scale_color_ptol(guide = F)
saveRDS(true.pop.plot, "Vysoká škola ekonomická v Praze/Tony Wei Tse Hung - YAY/Estimates/Plot.files/true.pop_map.rds")

#########################

## data prep and plot: MLE equal p.matrix

estimations.equal.part1 <- readRDS("Vysoká škola ekonomická v Praze/Tony Wei Tse Hung - YAY/Estimates/P.equal/u.est.non.inf.P.equal.part1_400.rds") 
estimations.equal1 <- estimations.equal.part1 %>% 
  select(j, u0, u1, u5, u10, u20, u100, u400)  %>% 
  arrange(j)
rm(estimations.equal.part1)
estimations.equal.part2 <- readRDS("Vysoká škola ekonomická v Praze/Tony Wei Tse Hung - YAY/Estimates/P.equal/u.est.non.inf.P.equal.part401_800.rds") 
estimations.equal2 <- estimations.equal.part2 %>% 
  select(j, u600 = u200, u800 = u400) %>% 
  arrange(j) %>% 
  select(-j)
rm(estimations.equal.part2)
estimations.equal.part3 <- readRDS("Vysoká škola ekonomická v Praze/Tony Wei Tse Hung - YAY/Estimates/P.equal/u.est.non.inf.P.equal.part401_800.rds") 
estimations.equal3 <- estimations.equal.part3 %>% 
  select(j, u1000 = u200) %>% 
  arrange(j) %>% 
  select(-j)
rm(estimations.equal.part3)

estimations.equal.final <- bind_cols(estimations.equal1, estimations.equal2, estimations.equal3) %>% 
  mutate_at(vars(starts_with("u")), ~case_when(. <= 1 ~ "Less or equal 1",
                                               . > 1 & . <= 10 ~ "Between 2 and 10",
                                               . > 10 & . <= 30 ~ "Between 11 and 30",
                                               . > 30 ~"More than 30")) %>% 
  left_join(census.geo, by = c("j" = "internal.id"))
  
estimations.equal.long <- melt(data = estimations.equal.final, id.vars = c("j", "geometry"))

estimations.equal.geo <- estimations.equal.long[, .(geom = st_union(geometry)), by = list(variable, value)]

# saveRDS(estimations.equal.geo, "Vysoká škola ekonomická v Praze/Tony Wei Tse Hung - YAY/Estimates/Plot.files/estimations.long.equal.rds")
estimations.equal.geo <- readRDS("Vysoká škola ekonomická v Praze/Tony Wei Tse Hung - YAY/Estimates/Plot.files/estimations.long.equal_new.rds")

p.equal <- estimations.equal.geo %>%
  mutate(iteration = as.numeric(variable)) %>%
  st_as_sf() %>%
  st_transform(crs = 3035) %>%
  mutate(mobile = factor(value, levels = c("Less or equal 1", "Between 2 and 10", "Between 11 and 30", "More than 30"))) %>%
  mutate(new_var_iter = case_when(variable == "u0" ~ 0,
                                  variable == "u1" ~ 1,
                                  variable == "u5" ~ 5 ,
                                  variable == "u10" ~ 10,
                                  variable == "u20" ~ 20,
                                  variable == "u100" ~ 100,
                                  variable == "u400" ~ 400,
                                  variable == "u600" ~ 600,
                                  variable == "u800" ~ 800,
                                  variable == "u1000" ~ 1000)) %>%
  ggplot(aes(label = paste("Iteration:", new_var_iter ))) +
  geom_sf(aes(fill = factor(mobile), color = factor(mobile))) +
  geom_text(aes( 4470000, 2700000,), size = 6, color = "black")+
  labs(x = "", y = "", title = "", subtitle = "MLE equal P.matrix 1000 iter. estimate", size = 20) +
  scale_fill_ptol(name = "Mobile phones") +
  scale_color_ptol(guide = F) +
  transition_manual(iteration)

saveRDS(p.equal, "Vysoká škola ekonomická v Praze/Tony Wei Tse Hung - YAY/Estimates/Plot.files/MLE.equal.estimate_map_new.rds")
p.equal <- readRDS("Vysoká škola ekonomická v Praze/Tony Wei Tse Hung - YAY/Estimates/Plot.files/MLE.oracle.estimate_map_new.rds")

p.equal.anim <- animate(p.equal, fps = 10)

anim_save(filename = "Vysoká škola ekonomická v Praze/Tony Wei Tse Hung - YAY/Estimates/Plot.files/MLE.equal.estimate_map_new.gif", animation = p.equal.anim)



## data prep and plot: MLE true p.matrix

estimations.true.part1 <- readRDS("Vysoká škola ekonomická v Praze/Tony Wei Tse Hung - YAY/Estimates/P.oracle/u.est.non.inf.P.true.part1_400.rds") 
estimations.true1 <- estimations.true.part1 %>% 
  select(j, u0, u1, u5, u10, u20, u100, u400)  %>% 
  arrange(j)
rm(estimations.true.part1)
estimations.true.part2 <- readRDS("Vysoká škola ekonomická v Praze/Tony Wei Tse Hung - YAY/Estimates/P.oracle/u.est.non.inf.P.true.part401_800.rds") 
estimations.true2 <- estimations.true.part2 %>% 
  select(j, u600 = u200, u800 = u400) %>% 
  arrange(j) %>% 
  select(-j)
rm(estimations.true.part2)
estimations.true.part3 <- readRDS("Vysoká škola ekonomická v Praze/Tony Wei Tse Hung - YAY/Estimates/P.oracle/u.est.non.inf.P.true.part401_800.rds") 
estimations.true3 <- estimations.true.part3 %>% 
  select(j, u1000 = u200) %>% 
  arrange(j) %>% 
  select(-j)
rm(estimations.true.part3)

estimations.true.final <- bind_cols(estimations.true1, estimations.true2, estimations.true3) %>% 
  mutate_at(vars(starts_with("u")), ~case_when(. <= 1 ~ "Less or equal 1",
                                               . > 1 & . <= 10 ~ "Between 2 and 10",
                                               . > 10 & . <= 30 ~ "Between 11 and 30",
                                               . > 30 ~"More than 30")) %>% 
  left_join(census.geo, by = c("j" = "internal.id"))

estimations.true.long <- melt(data = estimations.true.final, id.vars = c("j", "geometry"))

estimations.true.geo <- estimations.true.long[, .(geom = st_union(geometry)), by = list(variable, value)]

# saveRDS(estimations.true.geo, "Vysoká škola ekonomická v Praze/Tony Wei Tse Hung - YAY/Estimates/Plot.files/estimations.long.true.rds")
estimations.true.geo <- readRDS("Vysoká škola ekonomická v Praze/Tony Wei Tse Hung - YAY/Estimates/Plot.files/estimations.long.true_new.rds")

### adjustment
estimations.true.geo <- estimations.true.geo %>% 
  mutate(value = case_when(value == "Less or true 1" ~ "Less or equal 1",
                           TRUE ~ value))

p.true <- estimations.true.geo %>%
  mutate(iteration = as.numeric(variable)) %>%
  st_as_sf() %>%
  st_transform(crs = 3035) %>%
  mutate(mobile = factor(value, levels = c("Less or equal 1", "Between 2 and 10", "Between 11 and 30", "More than 30"))) %>%
  mutate(new_var_iter = case_when(variable == "u0" ~ 0,
                                  variable == "u1" ~ 1,
                                  variable == "u5" ~ 5 ,
                                  variable == "u10" ~ 10,
                                  variable == "u20" ~ 20,
                                  variable == "u100" ~ 100,
                                  variable == "u400" ~ 400,
                                  variable == "u600" ~ 600,
                                  variable == "u800" ~ 800,
                                  variable == "u1000" ~ 1000)) %>%
  ggplot(aes(label = paste("Iteration:", new_var_iter ))) +
  geom_sf(aes(fill = factor(mobile), color = factor(mobile))) +
  geom_text(aes( 4470000, 2700000,), size = 6, color = "black")+
  labs(x = "", y = "", title = "", subtitle = "MLE oracle P.matrix 1000 iter. estimate", size = 20) +
  scale_fill_ptol(name = "Mobile phones") +
  scale_color_ptol(guide = F) +
  transition_manual(iteration)
saveRDS(p.true, "Vysoká škola ekonomická v Praze/Tony Wei Tse Hung - YAY/Estimates/Plot.files/MLE.oracle.estimate_map_new.rds")
p.true <- readRDS("Vysoká škola ekonomická v Praze/Tony Wei Tse Hung - YAY/Estimates/Plot.files/MLE.oracle.estimate_map_new.rds")

p.true.anim <- animate(p.true, fps = 10)

anim_save(filename = "Vysoká škola ekonomická v Praze/Tony Wei Tse Hung - YAY/Estimates/Plot.files/MLE.oracle.estimate_map_new.gif", animation = p.true.anim)



## data prep and plot: Voronoi Tower
estimations.Voronoi.both <- readRDS("Vysoká škola ekonomická v Praze/Tony Wei Tse Hung - YAY/Estimates/Voronoi both/Voronoi.estimates.rds")

estimations.VT <- estimations.Voronoi.both %>% 
  ungroup() %>% 
  select(internal.id, voronoi.est.tower) %>% 
  mutate(pop.cat = case_when(voronoi.est.tower <= 1 ~ "Less or equal 1",
                             voronoi.est.tower > 1 & voronoi.est.tower <= 10 ~ "Between 2 and 10",
                             voronoi.est.tower > 10 & voronoi.est.tower <= 30 ~ "Between 11 and 30",
                             voronoi.est.tower > 30 ~"More than 30")) %>% 
  mutate(mobile = factor(pop.cat, levels = c("Less or equal 1", "Between 2 and 10", "Between 11 and 30", "More than 30"))) %>%
  arrange(internal.id) %>% 
  mutate(geometry = census.geo$geometry) %>% 
  select(internal.id, mobile, geometry) %>% 
  st_as_sf() %>% 
  group_by(mobile) %>% 
  summarise(geometry = st_union(geometry))
saveRDS(estimations.VT, "Vysoká škola ekonomická v Praze/Tony Wei Tse Hung - YAY/Estimates/Plot.files/VT.estimate_data.rds")


VT.plot <- estimations.VT %>% 
  ggplot() +
  geom_sf(aes(fill = factor(mobile), color = factor(mobile))) +
  labs(x = "", y = "", title = "", subtitle = "Voronoi tower estimate", size = 20) +
  scale_fill_ptol(name = "Mobile phones") +
  scale_color_ptol(guide = F)
saveRDS(VT.plot, "Vysoká škola ekonomická v Praze/Tony Wei Tse Hung - YAY/Estimates/Plot.files/VT.estimate_map.rds")


## data prep and plot: Voronoi antenna
estimations.Voronoi.both <- readRDS("Vysoká škola ekonomická v Praze/Tony Wei Tse Hung - YAY/Estimates/Voronoi both/Voronoi.estimate.rds")

estimations.VA <- estimations.Voronoi.both %>% 
  ungroup() %>% 
  select(internal.id, voronoi.est.antenna) %>% 
  mutate(pop.cat = case_when(voronoi.est.antenna <= 1 ~ "Less or equal 1",
                             voronoi.est.antenna > 1 & voronoi.est.antenna <= 10 ~ "Between 2 and 10",
                             voronoi.est.antenna > 10 & voronoi.est.antenna <= 30 ~ "Between 11 and 30",
                             voronoi.est.antenna > 30 ~ "More than 30")) %>% 
  mutate(mobile = factor(pop.cat, levels = c("Less or equal 1", "Between 2 and 10", "Between 11 and 30", "More than 30"))) %>%
  arrange(internal.id) %>% 
  mutate(geometry = census.geo$geometry) %>% 
  select(internal.id, mobile, geometry) %>% 
  st_as_sf() %>% 
  group_by(mobile) %>% 
  summarise(geometry = st_union(geometry))
saveRDS(estimations.VA, "Vysoká škola ekonomická v Praze/Tony Wei Tse Hung - YAY/Estimates/Plot.files/VA.estimate_data.rds")

VA.plot <- estimations.VA %>% 
  ggplot() +
  geom_sf(aes(fill = factor(mobile), color = factor(mobile))) +
  labs(x = "", y = "", title = "", subtitle = "Voronoi antenna estimate", size = 20) +
  scale_fill_ptol(name = "Mobile phones") +
  scale_color_ptol(guide = F)
saveRDS(VA.plot, "Vysoká škola ekonomická v Praze/Tony Wei Tse Hung - YAY/Estimates/Plot.files/VA.estimate_map.rds")

