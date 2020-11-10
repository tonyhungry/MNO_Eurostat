library(tidyverse)
library(Matrix)

# toy data
c.vec <- c(12, 10, 30) # three antennas
P.mat <- Matrix(matrix(c(0,1,0, 0,0.5,0.5, 1/3, 1/3, 1/3), nrow = 3, ncol = 3, byrow = F), sparse = T)
a.vec <- c(2, 1, 1) # 3 tiles

# non informative prior
u1 = (0 / (0 + 0 + (1/3)) * 12) + (1 / (1 + 0.5 + (1/3)) * 10) + (0 / (0 + 0.5 + (1/3)) * 30) 
u2 = (0 / (0 + 0 + (1/3)) * 12) + (0.5 / (1 + 0.5 + (1/3)) * 10) + (0.5 / (0 + 0.5 + (1/3)) * 30) 
u3 = ((1/3) / (0 + 0 + (1/3)) * 12) + ((1/3) / (1 + 0.5 + (1/3)) * 10) + ((1/3) / (0 + 0.5 + (1/3)) * 30)
c.sum = u1 + u2 + u3

# informative prior
u1 = 2 * (0 / (0 * 2 + 0 * 1 + (1/3) * 1) * 12) + (1 / (1 * 2 + 0.5 * 1 + (1/3) * 1) * 10) + (0 / (0 * 2 + 0.5 * 1 + (1/3) * 1) * 30))
2 * 3.529412
u2 = 1 * (0 / (0 * 2 + 0 * 1 + (1/3) * 1) * 12) + (0.5 / (1 * 2 + 0.5 * 1 + (1/3) * 1) * 10) + (0.5 / (0 * 2 + 0.5 * 1 + (1/3) * 1) * 30) 
1 * 19.76471
u3 = 1 * ((1/3) / (0 * 2 + 0 * 1 + (1/3) * 1) * 12) + ((1/3) / (1 * 2 + 0.5 * 1 + (1/3) * 1) * 10) + ((1/3) / (0 * 2 + 0.5 * 1 + (1/3) * 1) * 30)
1 * 25.17647
c.sum = u1 + u2 + u3



SB_u_est <- function(c.vec, P.mat, a.vec) {
  P.trans.mat <- t(P.mat)
  P.correct.mat <- P.mat
  P.correct.mat@x[P.correct.mat@x > 0] <- 1
  denominator.summary <- tibble(u = c(1:length(c.vec)), value = colSums(P.trans.mat * a.vec)) # of length u
  denominator.vec <- colSums(P.trans.mat * a.vec)
  
  corrector <- P.correct.mat * denominator.summary$value
  
  sX <- summary(P.correct.mat)
  sY <- summary(corrector)
  sRes <- merge(sX, sY, by=c("i", "j"))
  f <- sparseMatrix(i = sRes[,1], j = sRes[,2], x = sRes[,3]/sRes[,4],
                    dimnames = dimnames(P.correct.mat))
  
  fraction.final <- colSums((f * P.mat) * c.vec, na.rm = T)
  u <- fraction.final * a.vec
  return(u)
}


sum(SB_u_est(c.vec, P.mat, a.vec))





P.mat.large <- readRDS("C:/Users/Marco/OneDrive - Universiteit Utrecht/MNO/working objects/P.mat.rds")
u.df.large <- readRDS("C:/Users/Marco/OneDrive - Universiteit Utrecht/MNO/working objects/census.tile.final.rds")
c.df.large <- readRDS("C:/Users/Marco/OneDrive - Universiteit Utrecht/MNO/working objects/C.vec.df.final.rds")

# populating the c vector to match up with p matrix
c.df.large.man <- c.df.large %>% 
  add_row(antenna.ID = "e", phones.sum = 0) %>% 
  add_row(antenna.ID = "e", phones.sum = 0) %>% 
  add_row(antenna.ID = "e", phones.sum = 0) %>% 
  add_row(antenna.ID = "e", phones.sum = 0) %>% 
  add_row(antenna.ID = "e", phones.sum = 0) %>% 
  add_row(antenna.ID = "e", phones.sum = 0) %>% 
  add_row(antenna.ID = "e", phones.sum = 0) %>% 
  add_row(antenna.ID = "e", phones.sum = 0) %>% 
  add_row(antenna.ID = "e", phones.sum = 0) %>% 
  add_row(antenna.ID = "e", phones.sum = 0) %>% 
  add_row(antenna.ID = "e", phones.sum = 0)

# Preparing data
u.true.large <- u.df$pop
u.df.final <- u.df.large %>% 
  mutate(a.vec = case_when(pop.area.kind == "Uninhabitated" ~ 1,
                           pop.area.kind == "Rural" ~ 2,
                           pop.area.kind == "Suburban" ~ 3,
                           pop.area.kind == "Urban" ~ 4))
a.vec.large <- u.df.final$a.vec
c.vec.large <- c.df.large.man$phones.sum



# Estimating
u.est <- SB_u_est(c.vec.large, P.mat.large, a.vec.large) # ca 10 min
u.total.est <- sum(u.est)


# Equal probability P.matrix
P.mat.large.ep <- P.mat.large
P.mat.large.ep@x[P.mat.large.ep@x > 0] <- 1

m_final <- t(t(P.mat.large.ep)/colSums(P.mat.large.ep))
d <- colSums(m_final)

e <- summary(m_final)
