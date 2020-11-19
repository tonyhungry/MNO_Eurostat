library(tidyverse)
library(Matrix)
library(sf)
library(data.table)
library(svglite)

census.de.100m.tile <- readRDS("C:/Users/Marco/OneDrive - Universiteit Utrecht/MNO/working objects/census.tile.final.rds") %>% 
  dplyr::select(-c(cluster_id, pop.raster, cluster.tile.n))

C.vec.df <- readRDS("C:/Users/Marco/OneDrive - Universiteit Utrecht/MNO/working objects/C.vec.df.final.rds")

P.orcale.mat <- readRDS("C:/Users/Marco/OneDrive - Universiteit Utrecht/MNO/working objects/P.mat.rds")

coverage.areas <- readRDS("C:/Users/Marco/OneDrive - Universiteit Utrecht/MNO/working objects/coverage.areas.rds")


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

# informative true prior oracle
a.true.vec <- census.de.100m.tile %>% 
  st_drop_geometry() %>% 
  dplyr::select(internal.id, pop) %>% 
  arrange(internal.id) %>% 
  deframe()


# C vector, adding antennas that have 0 phones to complete the vector
C.vec.df.final <- C.vec.df %>% 
  right_join(coverage.areas, by = "antenna.ID") %>% 
  mutate(phones.sum = case_when(is.na(phones.sum) ~ 0,
                                TRUE ~ phones.sum)) %>% 
  filter(!antenna.ID == "RT6.A.1")
# final c vector
c.vec <- C.vec.df.final %>%
  arrange(antenna.ID) %>% 
  dplyr::select(antenna.ID, phones.sum) %>% 
  deframe()


# P matrix oracle (P)
P.oracle.summ <- summary(P.orcale.mat)
P.oracle.dt <- data.table(i = P.oracle.summ$i,
                          j = P.oracle.summ$j,
                          pij = P.oracle.summ$x)

# P matrix equal prob (P')
P.prime.mat.helper <- P.orcale.mat
P.prime.mat.helper@x[P.prime.mat.helper@x > 0] <- 1
P.prime.summ <- summary(P.prime.mat.helper)
p.helper.dt <- data.table(i = P.prime.summ$i,
                          j = P.prime.summ$j,
                          pij = P.prime.summ$x)
P.prime.dt <- p.helper.dt[, .(i = i, pij = pij / sum(pij)), by = "j"]
setcolorder(P.prime.dt, c("i", "j", "pij"))




####################################

# Estimating
# Oracle model (non.informative prior, P = P)
u.true.oracle <- SB_u_est_EM_mat(c.vec, P.oracle.dt, a.true.vec, n.iter = 1)
u.true.oracle$u.true <- a.true.vec

# MLE model 1 (non.informative prior, P = P')
u.est.non.inf.P.equal <- SB_u_est_EM_mat(c.vec, P.prime.dt, a.non.inf.vec, n.iter = 100)
u.est.non.inf.P.equal$u.true <- a.true.vec
# saveRDS(u.est.non.inf.P.equal, "C:/Users/Marco/OneDrive - Universiteit Utrecht/MNO/working objects/u.est.non.inf.P.equal.rds")
u.est.non.inf.P.equal <- readRDS("C:/Users/Marco/OneDrive - Universiteit Utrecht/MNO/working objects/u.est.non.inf.P.equal.rds")

names.p.equal.old <- c("j", paste0("u", 0:100), "u.true")
names.p.equal.new <- c("internal.id", paste0("u.P.equal", 0:100), "u.true")
u.est.non.inf.P.equal.final <- u.est.non.inf.P.equal %>% 
  rename_at(vars(names.p.equal.old), ~names.p.equal.new) %>% 
  mutate(internal.id = as.numeric(internal.id)) %>% 
  left_join(census.de.100m.tile, by = "internal.id")

u.est.non.inf.P.equal.geo <- u.est.non.inf.P.equal.final %>% 
  select(internal.id, u.P.equal1, u.P.equal2, u.P.equal10, u.P.equal20, u.P.equal50, u.P.equal100, u.true, geometry)


# MLE model 2 (non.informative prior, P = P)
u.est.non.inf.P.oracle <- SB_u_est_EM_mat(c.vec, P.oracle.dt, a.non.inf.vec, n.iter = 100)
u.est.non.inf.P.oracle$u.true <- a.true.vec
# saveRDS(u.est.non.inf.P.oracle, "C:/Users/Marco/OneDrive - Universiteit Utrecht/MNO/working objects/u.est.non.inf.P.oracle.rds")
u.est.non.inf.P.oracle <- readRDS("C:/Users/Marco/OneDrive - Universiteit Utrecht/MNO/working objects/u.est.non.inf.P.oracle.rds")

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



estimations.geo.final <- estimations.geo %>% 
  mutate_at(vars(starts_with("u.P.")), ~case_when(. >= 50 ~ 1,
                                                  TRUE ~ 0))


f <- e %>% 
  group_by(u.P.equal20) %>% 
  summarize(geometry = st_union(geometry.x))


r %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf(color = "black")


est_plot <- function(data, variable) {
  
  dat <- 
  
  title <- variable
  
  plot <- data %>%
    ggplot() +
    geom_sf(aes(color = factor(variable))) +
    scale_color_manual(values = c("grey", "black"),
                       labels = c("u hat > 50", "u <= 50"),
                       name = "") +
    ggtitle("", subtitle = variable) +
    theme(plot.margin = unit(c(0, 0, 0, 0), "mm"))
  saveRDS(plot, paste0("C:/Users/Marco/OneDrive - Universiteit Utrecht/MNO/estimation plots/", title, ".rds"))
  rm(plot)
}


variables <- estimations.geo %>% 
  select(starts_with("u.P")) %>% 
  st_drop_geometry() %>% 
  names(.)

  
walk(variables, ~est_plot(estimations.geo, .x))

d1 <- readRDS("C:/Users/Marco/OneDrive - Universiteit Utrecht/MNO/estimation plots/u.P.equal1.rds")
