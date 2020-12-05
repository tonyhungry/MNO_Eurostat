library(data.table)
library(tidyverse)

# Take a stratified sample (n_strata = 50) based on pop area kind, join all estimation parts togethe to one file with 150 observations and 1000 iterations


setwd("C:/Users/Marco/")

census.de.100m.tile <- readRDS("Vysoká škola ekonomická v Praze/Tony Wei Tse Hung - YAY/working objects/census.tile.final.rds") 
census.geo.sample <- census.de.100m.tile %>% 
  st_drop_geometry() %>% 
  dplyr::select(internal.id, pop.area.kind, pop) %>% 
  group_by(pop.area.kind) %>% 
  sample_n(50)

#####

part1 <- readRDS("Vysoká škola ekonomická v Praze/Tony Wei Tse Hung - YAY/Estimates/P.equal/u.est.non.inf.P.equal.part1_400.rds")
part1.sample <- part1[j %in% census.geo.sample$internal.id]
name.equal.part1.old <- names(part1.sample)[1:401] 
name.equal.part1.new <- c("internal.id", paste0("u.P.equal", 0:399))
part1.sample.final <- part1.sample %>% 
  rename_at(vars(name.equal.part1.old), ~name.equal.part1.new)
rm(part1)


part2 <- readRDS("Vysoká škola ekonomická v Praze/Tony Wei Tse Hung - YAY/Estimates/P.equal/u.est.non.inf.P.equal.part401_800.rds")
part2.sample <- part2[j %in% census.geo.sample$internal.id]
name.equal.part2.old <- names(part2.sample)[1:401] 
name.equal.part2.new <- c("internal.id", paste0("u.P.equal", 400:799))
part2.sample.final <- part2.sample %>% 
  rename_at(vars(name.equal.part2.old), ~name.equal.part2.new)
rm(part2)


part3 <- readRDS("Vysoká škola ekonomická v Praze/Tony Wei Tse Hung - YAY/Estimates/P.equal/u.est.non.inf.P.equal.part801_1000.rds")
part3.sample <- part3[j %in% census.geo.sample$internal.id]
name.equal.part3.old <- names(part3.sample)[1:202] 
name.equal.part3.new <- c("internal.id", paste0("u.P.equal", 800:1000))
part3.sample.final <- part3.sample %>% 
  rename_at(vars(name.equal.part3.old), ~name.equal.part3.new)
rm(part3)

equal.combined.sample <- census.geo.sample %>% 
  left_join(part1.sample.final, by = "internal.id") %>% 
  left_join(part2.sample.final, by = "internal.id") %>% 
  left_join(part3.sample.final, by = "internal.id") 

saveRDS(equal.combined.sample, "Vysoká škola ekonomická v Praze/Tony Wei Tse Hung - YAY/Estimates/Plot.files/P.equal.sample.rds")


## empty directory
#### oracle

part1 <- readRDS("Vysoká škola ekonomická v Praze/Tony Wei Tse Hung - YAY/Estimates/P.oracle/u.est.non.inf.P.oracle.part1_400.rds")
part1.sample <- part1[j %in% census.geo.sample$internal.id]
name.oracle.part1.old <- names(part1.sample)[1:401] 
name.oracle.part1.new <- c("internal.id", paste0("u.P.true", 0:399))
part1.sample.final <- part1.sample %>% 
  rename_at(vars(name.oracle.part1.old), ~name.oracle.part1.new)
rm(part1)


part2 <- readRDS("Vysoká škola ekonomická v Praze/Tony Wei Tse Hung - YAY/Estimates/P.oracle/u.est.non.inf.P.oracle.part401_800.rds")
part2.sample <- part2[j %in% census.geo.sample$internal.id]
name.oracle.part2.old <- names(part2.sample)[1:401] 
name.oracle.part2.new <- c("internal.id", paste0("u.P.true", 400:799))
part2.sample.final <- part2.sample %>% 
  rename_at(vars(name.oracle.part2.old), ~name.oracle.part2.new)
rm(part2)


part3 <- readRDS("Vysoká škola ekonomická v Praze/Tony Wei Tse Hung - YAY/Estimates/P.oracle/u.est.non.inf.P.oracle.part801_1000.rds")
part3.sample <- part3[j %in% census.geo.sample$internal.id]
name.oracle.part3.old <- names(part3.sample)[1:202] 
name.oracle.part3.new <- c("internal.id", paste0("u.P.true", 800:1000))
part3.sample.final <- part3.sample %>% 
  rename_at(vars(name.oracle.part3.old), ~name.oracle.part3.new)
rm(part3)

true.combined.sample <- census.geo.sample %>% 
  left_join(part1.sample.final, by = "internal.id") %>% 
  left_join(part2.sample.final, by = "internal.id") %>% 
  left_join(part3.sample.final, by = "internal.id") 

saveRDS(true.combined.sample, "Vysoká škola ekonomická v Praze/Tony Wei Tse Hung - YAY/Estimates/Plot.files/P.true.sample.rds")

            