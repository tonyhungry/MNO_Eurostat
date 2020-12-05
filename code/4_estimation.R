library(tidyverse)
library(Matrix)
library(sf)
library(data.table)

census.de.100m.tile <- readRDS("C:/Users/ramljak/Desktop/marco/working objects/census.tile.final.rds") %>% 
  dplyr::select(-c(cluster_id, pop.raster, cluster.tile.n))

C.vec.df <- readRDS("C:/Users/ramljak/Desktop/marco/working objects/C.vec.df.final.new.rds")

P.orcale.mat <- readRDS("C:/Users/ramljak/Desktop/marco/working objects/P.mat.rds")

# P.helper.missing <- readRDS(file = "C:/Users/ramljak/Desktop/marco/working objects/P.helper.missing.rds")

coverage.areas <- readRDS("C:/Users/ramljak/Desktop/marco/working objects/coverage.areas.rds")


# New MLE iteration function by Matyas
SB_u_est_EM_mat <- function(c.vec, P.dt, a.vec, n.iter) {
  
  
  cdt <- data.table(c = c.vec)
  cdt <- cdt[, .(i = 1:.N, c = c)]
  pij <- cdt[P.dt, on = "i"]
  pij <- pij[c > 0] #remove those lines where c==0 because it will create 0 division
  adt <- data.table(a = a.vec)
  tiles <- adt[, .(j = 1:.N, u0 = a)]
  
  for(m in 0:(n.iter - 1)){
    cat(format(Sys.time()), paste0("---- calculating u", m + 1), "----\n")
    cols <- c("j", paste0("u", m))
    ju <- tiles[, cols, with = F]
    setnames(ju, c("j", "u"))
    pij <- ju[pij, on = "j"]
    denom <- pij[, .(sum_pik_uk = sum(u * pij)), by = i]
    pij <- denom[pij, on = "i"]
    faktor <- pij[, .(f = sum(c * pij / sum_pik_uk)), by = j]
    pij[, c("u", "sum_pik_uk") := NULL]
    tiles <- faktor[tiles, on = "j"]
    tiles <- eval(parse(text = paste0("tiles[,u", m+1, ":=u", m, "*f]")))
    tiles[, "f" := NULL]
  }
  
  return(tiles)
  
}


# non-informative prior
a.non.inf.vec <- rep(1, length(census.de.100m.tile$internal.id))
saveRDS(a.non.inf.vec, file = "C:/Users/ramljak/Desktop/marco/working objects/a.non.inf.vec.rds")

# informative true prior oracle
a.true.vec <- census.de.100m.tile %>% 
  st_drop_geometry() %>% 
  dplyr::select(internal.id, pop) %>% 
  arrange(internal.id) %>% 
  deframe()
saveRDS(a.true.vec, file = "C:/Users/ramljak/Desktop/marco/working objects/a.true.vec.rds")


# C vector, adding antennas that have 0 phones to complete the vector
C.vec.df.final <- C.vec.df %>% 
  right_join(coverage.areas, by = "antenna.ID") %>% 
  mutate(phones.sum = case_when(is.na(phones.sum) ~ 0,
                                TRUE ~ phones.sum)) 

# final c vector
c.vec <- C.vec.df.final %>%
  arrange(antenna.ID) %>% 
  dplyr::select(antenna.ID, phones.sum) %>% 
  deframe()
saveRDS(c.vec, file = "C:/Users/ramljak/Desktop/marco/working objects/c.vec.rds")


# P matrix oracle (P)
P.oracle.summ <- summary(P.orcale.mat)
P.oracle.dt <- data.table(i = P.oracle.summ$i,
                          j = P.oracle.summ$j,
                          pij = P.oracle.summ$x)
saveRDS(P.oracle.dt, file = "C:/Users/ramljak/Desktop/marco/working objects/P.oracle.dt.rds")


# P matrix equal prob (P')
P.prime.mat.helper <- P.orcale.mat
P.prime.mat.helper@x[P.prime.mat.helper@x > 0] <- 1
P.prime.summ <- summary(P.prime.mat.helper)
p.helper.dt <- data.table(i = P.prime.summ$i,
                          j = P.prime.summ$j,
                          pij = P.prime.summ$x)
P.prime.dt <- p.helper.dt[, .(i = i, pij = pij / sum(pij)), by = "j"]
setcolorder(P.prime.dt, c("i", "j", "pij"))
saveRDS(P.prime.dt, file = "C:/Users/ramljak/Desktop/marco/working objects/P.prime.dt.rds")




####################################

# Estimating
# Oracle model (non.informative prior, P = P)
u.true.oracle <- SB_u_est_EM_mat(c.vec, P.oracle.dt, a.true.vec, n.iter = 1)
u.true.oracle$u.true <- a.true.vec

# MLE model 1 (non.informative prior, P = P')
a.non.inf.vec <- readRDS(file = "C:/Users/ramljak/Desktop/marco/working objects/a.non.inf.vec.rds")
P.prime.dt <- readRDS(file = "C:/Users/ramljak/Desktop/marco/working objects/P.prime.dt.rds")
c.vec <- readRDS(file = "C:/Users/ramljak/Desktop/marco/working objects/c.vec.rds")

u.est.non.inf.P.equal <- SB_u_est_EM_mat(c.vec, P.prime.dt, a.non.inf.vec, n.iter = 400)
saveRDS(u.est.non.inf.P.equal, "C:/Users/ramljak/Desktop/marco/Estimates/u.est.non.inf.P.equal.part1_400.rds")
non.inf.P.equal.prior_400 <- u.est.non.inf.P.equal$u400
rm(u.est.non.inf.P.equal)

u.est.non.inf.P.equal.part401_800 <- SB_u_est_EM_mat(c.vec, P.prime.dt, non.inf.P.equal.prior_400, n.iter = 400)
saveRDS(u.est.non.inf.P.equal.part401_800, "C:/Users/ramljak/Desktop/marco/Estimates/u.est.non.inf.P.equal.part401_800.rds")
non.inf.P.equal.prior_800 <- u.est.non.inf.P.equal.part401_800$u400
rm(u.est.non.inf.P.equal.part401_800)

u.est.non.inf.P.equal.part801_1000 <- SB_u_est_EM_mat(c.vec, P.prime.dt, non.inf.P.equal.prior_800, n.iter = 200)
saveRDS(u.est.non.inf.P.equal.part801_1000, "C:/Users/ramljak/Desktop/marco/Estimates/u.est.non.inf.P.equal.part801_1000.rds")

# 
# names.p.equal.old <- c("j", paste0("u", 0:400), "u.true")
# names.p.equal.new <- c("internal.id", paste0("u.P.equal", 0:400), "u.true")
# u.est.non.inf.P.equal.final <- u.est.non.inf.P.equal %>% 
#   rename_at(vars(names.p.equal.old), ~names.p.equal.new) %>% 
#   mutate(internal.id = as.numeric(internal.id)) %>% 
#   left_join(census.de.100m.tile, by = "internal.id")
# 
# u.est.non.inf.P.equal.final.sample <- u.est.non.inf.P.equal.final %>% 
#   dplyr::select(-geometry) %>% 
#   group_by(pop.area.kind) %>% 
#   sample_n(50)
# saveRDS(u.est.non.inf.P.equal.final.sample, "C:/Users/ramljak/Desktop/marco/working objects/u.est.non.inf.P.equal.sample.rds")
# 
# 
# u.est.non.inf.P.equal.geo <- u.est.non.inf.P.equal.final %>% 
#   select(internal.id, u.P.equal1, u.P.equal2, u.P.equal20, u.P.equal50, u.P.equal100, u.P.equal200, u.P.equal300, u.P.equal400, u.true, geometry)
# 
# saveRDS(u.est.non.inf.P.equal.geo, "C:/Users/ramljak/Desktop/marco/working objects/u.est.non.inf.P.equal.geo.rds")



# var.all.equal <- names(u.est.non.inf.P.equal.final)
# var.select.equal <- var.all.equal[seq(1, length(var.all.equal), 3)]

# u.est.non.inf.P.equal.selected <- u.est.non.inf.P.equal.final %>% 
#   select(var.select.equal, u.true)
# saveRDS(u.est.non.inf.P.equal.selected, "C:/Users/ramljak/Desktop/marco/working objects/u.est.non.inf.P.equal.selected.rds")
# u.est.non.inf.P.equal.selected <- readRDS("C:/Users/ramljak/Desktop/marco/working objects/u.est.non.inf.P.equal.selected.rds")

# MLE model 2 (non.informative prior, P = P)
a.non.inf.vec <- readRDS(file = "C:/Users/ramljak/Desktop/marco/working objects/a.non.inf.vec.rds")
P.oracle.dt <- readRDS(file = "C:/Users/ramljak/Desktop/marco/working objects/P.oracle.dt.rds")
c.vec <- readRDS(file = "C:/Users/ramljak/Desktop/marco/working objects/c.vec.rds")

u.est.non.inf.P.oracle <- SB_u_est_EM_mat(c.vec, P.oracle.dt, a.non.inf.vec, n.iter = 400)
saveRDS(u.est.non.inf.P.oracle, "C:/Users/ramljak/Desktop/marco/working objects/u.est.non.inf.P.oracle.part1_400.rds")
non.inf.P.oracle.prior_400 <- u.est.non.inf.P.oracle$u400
rm(u.est.non.inf.P.oracle)

u.est.non.inf.P.oracle.part401_800 <- SB_u_est_EM_mat(c.vec, P.oracle.dt, non.inf.P.oracle.prior_400, n.iter = 400)
saveRDS(u.est.non.inf.P.oracle.part401_800, "C:/Users/ramljak/Desktop/marco/Estimates/u.est.non.inf.P.oracle.part401_800.rds")
non.inf.P.oracle.prior_800 <- u.est.non.inf.P.oracle.part401_800$u400
rm(u.est.non.inf.P.oracle.part401_800)

u.est.non.inf.P.oracle.part801_1000 <- SB_u_est_EM_mat(c.vec, P.oracle.dt, non.inf.P.oracle.prior_800, n.iter = 200)
saveRDS(u.est.non.inf.P.oracle.part801_1000, "C:/Users/ramljak/Desktop/marco/Estimates/u.est.non.inf.P.oracle.part801_1000.rds")


names.p.oracle.old <- c("j", paste0("u", 0:100), "u.true")
names.p.oracle.new <- c("internal.id", paste0("u.P.oracle", 0:100), "u.true")
u.est.non.inf.P.oracle.final <- u.est.non.inf.P.oracle %>% 
  rename_at(vars(names.p.oracle.old), ~names.p.oracle.new) %>% 
  mutate(internal.id = as.numeric(internal.id)) %>% 
  left_join(census.de.100m.tile, by = "internal.id")

u.est.non.inf.P.oracle.geo <- u.est.non.inf.P.oracle.final %>% 
  select(internal.id, u.P.oracle1, u.P.oracle2, u.P.oracle10, u.P.oracle20, u.P.oracle50, u.P.oracle100, u.true, geometry)

# join estimates together
estimations.geo <- u.est.non.inf.P.equal.geo %>% 
  left_join(u.est.non.inf.P.oracle.geo, by = "internal.id") %>% 
  st_as_sf()

saveRDS(estimations.geo, "C:/Users/Marco/OneDrive - Universiteit Utrecht/MNO/working objects/estimations.geo.rds")
estimations.geo <- readRDS("C:/Users/Marco/OneDrive - Universiteit Utrecht/MNO/working objects/estimations.geo.rds")

# estimations.dt <- melt(estimations.geo, id.vars = c("internal.id", "u.true.x"),
#              measure.vars = c("dob_child1", "dob_child2", "dob_child3"))


estimations.new <- estimations.geo %>%
  st_drop_geometry() %>% 
  left_join(census.de.100m.tile, by = "internal.id") %>% 
  filter(!pop.area.kind == "Rural") %>%
  # filter(u.true.x > 10) %>%
  mutate(u.lower = u.true.x - (u.true.x * 0.5)) %>% 
  mutate(u.upper = u.true.x + (u.true.x * 0.5)) %>% 
  summarise_at(vars(starts_with("u.P.")), ~mean(. >= u.lower & . <= u.upper))




estimations.geo.final <- estimations.geo %>% 
  mutate_at(vars(starts_with("u.P.")), ~case_when(. >= 50 ~ 1,
                                                  TRUE ~ 0))


u.P.equal1 <- estimations.geo.final %>%
  select(u.P.equal1) %>% 
  group_by(u.P.equal1) %>% 
  summarize(geometry = st_union(geometry.x))
u.P.equal2 <- estimations.geo.final %>%
  select(u.P.equal2) %>% 
  group_by(u.P.equal2) %>% 
  summarize(geometry = st_union(geometry.x))
u.P.equal10 <- estimations.geo.final %>%
  select(u.P.equal10) %>% 
  group_by(u.P.equal10) %>% 
  summarize(geometry = st_union(geometry.x))
u.P.equal20 <- estimations.geo.final %>%
  select(u.P.equal20) %>% 
  group_by(u.P.equal20) %>% 
  summarize(geometry = st_union(geometry.x))
u.P.equal50 <- estimations.geo.final %>%
  select(u.P.equal50) %>% 
  group_by(u.P.equal50) %>% 
  summarize(geometry = st_union(geometry.x))
u.P.equal100 <- estimations.geo.final %>%
  select(u.P.equal100) %>% 
  group_by(u.P.equal100) %>% 
  summarize(geometry = st_union(geometry.x))

u.P.oracle1 <- estimations.geo.final %>%
  select(u.P.oracle1) %>% 
  group_by(u.P.oracle1) %>% 
  summarize(geometry = st_union(geometry.x))
u.P.oracle2 <- estimations.geo.final %>%
  select(u.P.oracle2) %>% 
  group_by(u.P.oracle2) %>% 
  summarize(geometry = st_union(geometry.x))
u.P.oracle10 <- estimations.geo.final %>%
  select(u.P.oracle10) %>% 
  group_by(u.P.oracle10) %>% 
  summarize(geometry = st_union(geometry.x))
u.P.oracle20 <- estimations.geo.final %>%
  select(u.P.oracle20) %>% 
  group_by(u.P.oracle20) %>% 
  summarize(geometry = st_union(geometry.x))
u.P.oracle50 <- estimations.geo.final %>%
  select(u.P.oracle50) %>% 
  group_by(u.P.oracle50) %>% 
  summarize(geometry = st_union(geometry.x))
u.P.oracle100 <- estimations.geo.final %>%
  select(u.P.oracle100) %>% 
  group_by(u.P.oracle100) %>% 
  summarize(geometry = st_union(geometry.x))

u.true <- estimations.geo.final %>%
  select(u.true.x) %>% 
  group_by(u.true.x) %>% 
  summarize(geometry = st_union(geometry.x))


u.P.equal1.plot <- u.P.equal1 %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf(color = "black")
u.P.equal2.plot <- u.P.equal2 %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf(color = "black")
u.P.equal10.plot <- u.P.equal10 %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf(color = "black")
u.P.equal20.plot <- u.P.equal20 %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf(color = "black")
u.P.equal50.plot <- u.P.equal50 %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf(color = "black")
u.P.equal100.plot <- u.P.equal100 %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf(color = "black")

u.P.oracle1.plot <- u.P.oracle1 %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf(color = "black")
u.P.oracle2.plot <- u.P.oracle2 %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf(color = "black")
u.P.oracle10.plot <- u.P.oracle10 %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf(color = "black")
u.P.oracle20.plot <- u.P.oracle20 %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf(color = "black")
u.P.oracle50.plot <- u.P.oracle50 %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf(color = "black")
u.P.oracle100.plot <- u.P.oracle100 %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf(color = "black")

u.true.plot <- u.true %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf(color = "black")

saveRDS(u.P.equal1.plot, "C:/Users/Marco/OneDrive - Universiteit Utrecht/MNO/estimation plots/u.P.equal1.rds")
saveRDS(u.P.equal2.plot, "C:/Users/Marco/OneDrive - Universiteit Utrecht/MNO/estimation plots/u.P.equal2.rds")
saveRDS(u.P.equal10.plot, "C:/Users/Marco/OneDrive - Universiteit Utrecht/MNO/estimation plots/u.P.equal10.rds")
saveRDS(u.P.equal20.plot, "C:/Users/Marco/OneDrive - Universiteit Utrecht/MNO/estimation plots/u.P.equal20.rds")
saveRDS(u.P.equal50.plot, "C:/Users/Marco/OneDrive - Universiteit Utrecht/MNO/estimation plots/u.P.equal50.rds")
saveRDS(u.P.equal100.plot, "C:/Users/Marco/OneDrive - Universiteit Utrecht/MNO/estimation plots/u.P.equal100.rds")
saveRDS(u.P.oracle1.plot, "C:/Users/Marco/OneDrive - Universiteit Utrecht/MNO/estimation plots/u.P.oracle1.rds")
saveRDS(u.P.oracle2.plot, "C:/Users/Marco/OneDrive - Universiteit Utrecht/MNO/estimation plots/u.P.oracle2.rds")
saveRDS(u.P.oracle10.plot, "C:/Users/Marco/OneDrive - Universiteit Utrecht/MNO/estimation plots/u.P.oracle10.rds")
saveRDS(u.P.oracle20.plot, "C:/Users/Marco/OneDrive - Universiteit Utrecht/MNO/estimation plots/u.P.oracle20.rds")
saveRDS(u.P.oracle50.plot, "C:/Users/Marco/OneDrive - Universiteit Utrecht/MNO/estimation plots/u.P.oracle50.rds")
saveRDS(u.P.oracle100.plot, "C:/Users/Marco/OneDrive - Universiteit Utrecht/MNO/estimation plots/u.P.oracle100.rds")
saveRDS(u.true.plot, "C:/Users/Marco/OneDrive - Universiteit Utrecht/MNO/estimation plots/true.rds")


est_plot <- function(data, variable) {

  variable1 <- sym(variable)
  
  dat <- data %>% 
    select(!!variable1) %>% 
    group_by(!!variable1) %>% 
    summarize(geometry = st_union(geometry.x))
  
  plot <- dat %>%
    ggplot() +
    geom_sf(color = "black") +
    ggtitle("", subtitle = variable) +
    theme(plot.margin = unit(c(0, 0, 0, 0), "mm"))
  saveRDS(plot, paste0("C:/Users/Marco/OneDrive - Universiteit Utrecht/MNO/estimation plots/", variable, ".rds"))
  rm(plot)
}


variables <- estimations.geo %>% 
  select(starts_with("u.P")) %>% 
  st_drop_geometry() %>% 
  names(.)

t <- head(estimations.geo.final, 1000)

e <- est_plot(estimations.geo.final, "u.P.equal20")

fi <- c("649725", "620677", "570726")

  
walk(variables, ~est_plot(estimations.geo, .x))

d1 <- readRDS("C:/Users/Marco/OneDrive - Universiteit Utrecht/MNO/estimation plots/u.P.equal1.rds")
