library(tidyverse)
library(data.table)
library(sf)
library(gganimate)
library(ggthemes)
library(transformr)


setwd("C:/Users/Marco/")

census.de.100m.tile <- readRDS("C:/Users/ramljak/Desktop/marco/working objects/census.tile.final.rds") 
census.geo <- census.de.100m.tile %>% 
  dplyr::select(internal.id)

estimations.equal.part1 <- readRDS("C:/Users/ramljak/Desktop/marco/Estimates/P.equal/u.est.non.inf.P.equal.part1_400.rds") 
estimations.equal1 <- estimations.equal.part1 %>% 
  select(j, u0, u1, u5, u10, u20, u100, u400)  %>% 
  arrange(j)
rm(estimations.equal.part1)
estimations.equal.part2 <- readRDS("C:/Users/ramljak/Desktop/marco/Estimates/P.equal/u.est.non.inf.P.equal.part401_800.rds") 
estimations.equal2 <- estimations.equal.part2 %>% 
  select(j, u600 = u200, u800 = u400) %>% 
  arrange(j) %>% 
  select(-j)
rm(estimations.equal.part2)
estimations.equal.part3 <- readRDS("C:/Users/ramljak/Desktop/marco/Estimates/P.equal/u.est.non.inf.P.equal.part401_800.rds") 
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

saveRDS(estimations.equal.geo, "C:/Users/ramljak/Desktop/marco/Estimates/Plot.files/estimations.long.equal.rds")
# estimations.equal.geo <- readRDS("C:/Users/ramljak/Desktop/marco/Estimates/Plot.files/estimations.long.equal.rds")


estimations.true.part1 <- readRDS("C:/Users/ramljak/Desktop/marco/Estimates/P.oracle/u.est.non.inf.P.true.part1_400.rds") 
estimations.true1 <- estimations.true.part1 %>% 
  select(j, u0, u1, u5, u10, u20, u100, u400)  %>% 
  arrange(j)
rm(estimations.true.part1)
estimations.true.part2 <- readRDS("C:/Users/ramljak/Desktop/marco/Estimates/P.oracle/u.est.non.inf.P.true.part401_800.rds") 
estimations.true2 <- estimations.true.part2 %>% 
  select(j, u600 = u200, u800 = u400) %>% 
  arrange(j) %>% 
  select(-j)
rm(estimations.true.part2)
estimations.true.part3 <- readRDS("C:/Users/ramljak/Desktop/marco/Estimates/P.oracle/u.est.non.inf.P.true.part401_800.rds") 
estimations.true3 <- estimations.true.part3 %>% 
  select(j, u1000 = u200) %>% 
  arrange(j) %>% 
  select(-j)
rm(estimations.true.part3)

estimations.true.final <- bind_cols(estimations.true1, estimations.true2, estimations.true3) %>% 
  mutate_at(vars(starts_with("u")), ~case_when(. <= 1 ~ "Less or true 1",
                                               . > 1 & . <= 10 ~ "Between 2 and 10",
                                               . > 10 & . <= 30 ~ "Between 11 and 30",
                                               . > 30 ~"More than 30")) %>% 
  left_join(census.geo, by = c("j" = "internal.id"))

estimations.true.long <- melt(data = estimations.true.final, id.vars = c("j", "geometry"))

estimations.true.geo <- estimations.true.long[, .(geom = st_union(geometry)), by = list(variable, value)]

saveRDS(estimations.true.geo, "C:/Users/ramljak/Desktop/marco/Estimates/Plot.files/estimations.long.true.rds")
# estimations.true.geo <- readRDS("C:/Users/ramljak/Desktop/marco/Estimates/Plot.files/estimations.long.true.rds")


##### Plots and animationns

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
  labs(x = "", y = "", title = "", subtitle = "Geographical evaluation (equal P.matrix)", size = 20) +
  scale_fill_ptol(name = "Mobile phones") +
  scale_color_ptol(guide = F) +
  transition_manual(iteration)

saveRDS(p.equal, "C:/Users/ramljak/Desktop/marco/Estimates/Plot.files/equal_map.rds")

p.equal.anim <- animate(p.equal, fps = 10)

anim_save(filename = "C:/Users/ramljak/Desktop/marco/Estimates/Plot.files/equal_map.gif", animation = p.equal.anim)


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
  labs(x = "", y = "", title = "", subtitle = "Geographical evaluation (true P.matrix)", size = 20) +
  scale_fill_ptol(name = "Mobile phones") +
  scale_color_ptol(guide = F) +
  transition_manual(iteration)
saveRDS(p.true, "C:/Users/ramljak/Desktop/marco/Estimates/Plot.files/true_map.rds")

p.true.anim <- animate(p.true, fps = 10)

anim_save(filename = "C:/Users/ramljak/Desktop/marco/Estimates/Plot.files/true_map.gif", animation = p.true.anim)

# census picture


  
