library(tidyverse)
library(ggthemes)
library(sf)

# setwd("C:/Users/Marco/")

# Loading In
voro <- readRDS("~/OneDrive - Vysoká škola ekonomická v Praze/YAY/Estimates/Voronoi both/Voronoi.estimates.rds")
equal <- readRDS("~/OneDrive - Vysoká škola ekonomická v Praze/YAY/Estimates/P.equal/u.est.non.inf.P.equal.part801_1000_new.rds")[,c(1,202)]
oracle <- readRDS("~/OneDrive - Vysoká škola ekonomická v Praze/YAY/Estimates/P.oracle/u.est.non.inf.P.oracle.part801_1000_new.rds")[,c(1,202)]
census.de.100m.tile <- readRDS("~/OneDrive - Vysoká škola ekonomická v Praze/YAY/working objects/census.tile.final.rds") 


## This function computes ECCDF data frames with log10 transformations of the x and y axis

custom_ecdf_prep <- function(df) {
  data <- df %>% 
    mutate(pop.plot = pop + 1) %>%  
    arrange(pop.plot) %>%  
    mutate(prob = 1 / n()) %>%  
    mutate(cum.prob = cumsum(prob)) %>%  
    mutate(cum.prob.comp = 1 - cum.prob) %>%  
    mutate(log10.cum.prob.comp = log10(cum.prob.comp)) %>% 
    mutate(log10.pop = log10(pop.plot)) %>%  
    mutate(cum.prob.comp = 1 - cum.prob)
  
  return(data)
}

## example

## Voronoi Antenna
# voro.ant.ECCDF <- voro %>%
#   ungroup() %>%
#   dplyr::select(internal.id, voronoi.est.antenna) %>%
#   mutate(pop.plot = voronoi.est.antenna + 1) %>%
#   arrange(pop.plot) %>%
#   mutate(prob = 1 / n()) %>%
#   mutate(cum.prob = cumsum(prob)) %>%
#   mutate(log10.cum.prob.comp = 1 - cum.prob) %>%
#   mutate(log10.pop = log10(pop.plot)) %>%
#   mutate(cum.prob.comp = 1 - cum.prob) %>%
#   select(log10.cum.prob.comp, log10.pop) %>%
#   mutate(log10.cum.prob.comp = round(log10.cum.prob.comp, 4)) %>% # effective plot sample --> faster plotting excluding overplot
#   distinct() %>%
#   mutate(estimate = "Voronoi antenna")

###-------------------------------##

# Voronoi antenna

voro.ant.ECCDF <- voro %>% 
  ungroup() %>% 
  dplyr::select(internal.id, pop = voronoi.est.antenna) %>% 
  custom_ecdf_prep() %>% 
  select(log10.cum.prob.comp, log10.pop) %>%
  mutate(log10.cum.prob.comp = round(log10.cum.prob.comp, 4)) %>% # effective plot sample --> faster plotting excluding overplot
  distinct() %>% 
  mutate(estimate = "Voronoi antenna")

# Voronoi tower

voro.tower.ECCDF <- voro %>% 
  ungroup() %>% 
  dplyr::select(internal.id, pop = voronoi.est.tower) %>% 
  custom_ecdf_prep() %>% 
  select(log10.cum.prob.comp, log10.pop) %>%
  mutate(log10.cum.prob.comp = round(log10.cum.prob.comp, 4)) %>% # effective plot sample --> faster plotting excluding overplot
  distinct() %>% 
  mutate(estimate = "Voronoi tower")

# Equal prob

MLE.equal.ECCDF <- equal %>% 
  as_tibble() %>% 
  ungroup() %>% 
  dplyr::select(internal.id = j, pop = u200) %>% 
  custom_ecdf_prep() %>% 
  select(log10.cum.prob.comp, log10.pop) %>%
  mutate(log10.cum.prob.comp = round(log10.cum.prob.comp, 4)) %>% # effective plot sample --> faster plotting excluding overplot
  distinct() %>% 
  mutate(estimate = "MLE equal P.matrix")

# Oracle prob

MLE.oracle.ECCDF <- oracle %>%
   as_tibble() %>%
   ungroup() %>%
  dplyr::select(internal.id = j, pop = u200) %>%
  custom_ecdf_prep() %>%
  select(log10.cum.prob.comp, log10.pop) %>%
  mutate(log10.cum.prob.comp = round(log10.cum.prob.comp, 4)) %>% # effective plot sample --> faster plotting excluding overplot
  distinct() %>%
  mutate(estimate = "MLE oracle P.matrix")

# True dist

true.ECCDF <- census.de.100m.tile %>% 
  st_drop_geometry() %>% 
  ungroup() %>% 
  dplyr::select(internal.id, pop) %>% 
  custom_ecdf_prep() %>% 
  select(log10.cum.prob.comp, log10.pop) %>%
  mutate(log10.cum.prob.comp = round(log10.cum.prob.comp, 4)) %>% # effective plot sample --> faster plotting excluding overplot
  distinct() %>% 
  mutate(estimate = "True")

## combining all estimator objects to one for easier plotting

final.ECCDF <- bind_rows(true.ECCDF, voro.ant.ECCDF, voro.tower.ECCDF, MLE.equal.ECCDF, MLE.oracle.ECCDF)
breaks <- c("True", "Voronoi antenna", "Voronoi tower", "MLE equal P.matrix", "MLE oracle P.matrix")

## ECCDF comparison plot

ECCDF.estimates <- final.ECCDF %>%   
  ggplot() + 
  geom_path(aes(x = log10.pop, y = log10.cum.prob.comp, color = estimate), size = 1) + 
  scale_color_ptol(breaks = breaks) + 
  ggtitle("", subtitle = "ECCDF comparison") +  
  labs(y = "Prob(Y > x)", x = "Mobile phones", color = "Estimator")

saveRDS(ECCDF.estimates, "~/OneDrive - Vysoká škola ekonomická v Praze/YAY/Estimates/Plot.files/ECCDF.estimates.rds")


#######-------------------------###

# ECDF plots

##############

# Voronoi antenna

voro.ant.ECDF <- voro %>% 
  ungroup() %>% 
  dplyr::select(internal.id, pop = voronoi.est.antenna) %>% 
  custom_ecdf_prep() %>% 
  select(cum.prob.comp, pop.plot) %>%
  mutate(cum.prob.comp = round(cum.prob.comp, 4)) %>% # effective plot sample --> faster plotting excluding overplot
  distinct() %>% 
  mutate(estimate = "Voronoi antenna")


# Voronoi tower

voro.tower.ECDF <- voro %>% 
  ungroup() %>% 
  dplyr::select(internal.id, pop = voronoi.est.tower) %>% 
  custom_ecdf_prep() %>% 
  select(cum.prob.comp, pop.plot) %>%
  mutate(cum.prob.comp = round(cum.prob.comp, 4)) %>% # effective plot sample --> faster plotting excluding overplot
  distinct() %>% 
  mutate(estimate = "Voronoi tower")

# Equal prob

MLE.equal.ECDF <- equal %>% 
  as_tibble() %>% 
  ungroup() %>% 
  dplyr::select(internal.id = j, pop = u200) %>% 
  custom_ecdf_prep() %>% 
  select(cum.prob.comp, pop.plot) %>%
  mutate(cum.prob.comp = round(cum.prob.comp, 4)) %>% # effective plot sample --> faster plotting excluding overplot
  distinct() %>% 
  mutate(estimate = "MLE equal P.matrix")

# Oracle prob

 MLE.oracle.ECDF <- oracle %>%
   as_tibble() %>%
   ungroup() %>%
   dplyr::select(internal.id = j, pop = u200) %>%
   custom_ecdf_prep() %>%
   select(cum.prob.comp, pop.plot) %>%
   mutate(cum.prob.comp = round(cum.prob.comp, 4)) %>% # effective plot sample --> faster plotting excluding overplot
   distinct() %>%
   mutate(estimate = "MLE oracle P.matrix")

# True dist

true.ECDF <- census.de.100m.tile %>% 
  st_drop_geometry() %>% 
  ungroup() %>% 
  dplyr::select(internal.id, pop) %>% 
  custom_ecdf_prep() %>% 
  select(cum.prob.comp, pop.plot) %>%
  mutate(cum.prob.comp = round(cum.prob.comp, 4)) %>% # effective plot sample --> faster plotting excluding overplot
  distinct() %>% 
  mutate(estimate = "True")

## combining all estimator objects to one for easier plotting

final.ECDF <- bind_rows(true.ECDF, voro.ant.ECDF, voro.tower.ECDF, MLE.equal.ECDF, MLE.oracle.ECDF)
breaks <- c("True", "Voronoi antenna", "Voronoi tower", "MLE equal P.matrix", "MLE oracle P.matrix")

## ECCDF comparison plot

ECDF.estimates <- final.ECDF %>%   
  ggplot() + 
  geom_path(aes(x = pop.plot, y = cum.prob.comp, color = estimate)) + 
  scale_color_ptol(breaks = breaks, guide = FALSE, expand = c(0, 0)) +
  labs(y = "", x = "") +
  theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 9, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

saveRDS(ECDF.estimates, "~/OneDrive - Vysoká škola ekonomická v Praze/YAY/Estimates/Plot.files/ECDF.estimates.rds")


#### Only true plot

# True dist

true.ECCDF.first <- census.de.100m.tile %>% 
  st_drop_geometry() %>% 
  ungroup() %>% 
  custom_ecdf_prep() %>% 
  dplyr::select(log10.cum.prob.comp, log10.pop, pop.area.kind) %>%
  mutate(log10.cum.prob.comp = round(log10.cum.prob.comp, 4)) %>% # effective plot sample --> faster plotting excluding overplot
  distinct()


true.ECDF.first <- census.de.100m.tile %>% 
  st_drop_geometry() %>% 
  ungroup() %>% 
  custom_ecdf_prep() %>% 
  dplyr::select(cum.prob.comp, pop.plot, pop.area.kind) %>%
  mutate(cum.prob.comp = round(cum.prob.comp, 4)) %>% # effective plot sample --> faster plotting excluding overplot
  distinct()

breaks.first <- c("Uninhabitated", "Rural", "Suburban", "Urban")
ECCDF.true <- true.ECCDF.first %>%   
  ggplot() + 
  geom_point(aes(x = log10.pop, y = log10.cum.prob.comp, color = pop.area.kind)) + 
  scale_color_ptol(breaks = breaks.first) + 
  ggtitle("", subtitle = "ECCDF true mobile phone density") +  
  labs(y = "Prob(Y > x)", x = "Mobile phones", color = "")

saveRDS(ECCDF.true, "Vysoká škola ekonomická v Praze/Tony Wei Tse Hung - YAY/Estimates/Plot.files/ECCDF.true.rds")


ECDF.true <- true.ECDF.first %>%   
  ggplot() + 
  geom_point(aes(x = pop.plot, y = cum.prob.comp, color = pop.area.kind)) + 
  scale_color_ptol(breaks = breaks.first, guide = FALSE, expand = c(0, 0)) +
  labs(y = "", x = "") +
  theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 9, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

saveRDS(ECDF.true, "Vysoká škola ekonomická v Praze/Tony Wei Tse Hung - YAY/Estimates/Plot.files/ECDF.true.rds")



#### bar plot pop area kind

true.ECCDF.complete <- census.de.100m.tile %>% 
  st_drop_geometry() %>% 
  ungroup() %>% 
  custom_ecdf_prep() %>% 
  dplyr::select(log10.cum.prob.comp, log10.pop, pop.area.kind) %>%
  mutate(log10.cum.prob.comp = round(log10.cum.prob.comp, 4)) 

prop.class <- data.frame( 
  class = c("Rural","Suburban","Urban"), 
  n = summary(as.factor(true.ECCDF.complete$pop.area.kind)), 
  prop = round(summary(as.factor(true.ECCDF.complete$pop.area.kind)) / length(census.de.100m.tile$internal.id), digits = 4) * 100)  %>% 
  mutate(class = factor(class)) %>% 
  arrange(desc(class)) %>% 
  mutate(lab.ypos = cumsum(prop) - 0.5 * prop) 

tile.prop.plot <- prop.class %>% 
  ggplot(aes(x = "", y = prop, fill = class)) + 
  geom_bar(width = 0.5, stat = "identity", color = "white") + 
  scale_fill_ptol(breaks = c("Rural", "Suburban", "Urban")) +
  geom_text(aes(y = lab.ypos, label = paste0(prop, "%")), color = "black") +
  labs(x = "", y = "Proportion of tiles", title = "", fill = "",
       subtitle = "Fig. 2a: Tile proportion") + 
  theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 9, hjust = 0.5))


saveRDS(tile.prop.plot, "Vysoká škola ekonomická v Praze/Tony Wei Tse Hung - YAY/Estimates/Plot.files/tile.prop.plot.rds")
