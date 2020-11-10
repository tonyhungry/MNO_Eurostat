library(tidyverse)
library(Matrix)


u1 = (0 / (0 + 0 + (1/3)) * 12) + (1 / (1 + 0.5 + (1/3)) * 10) + (0 / (0 + 0.5 + (1/3)) * 30) 
u2 = (0 / (0 + 0 + (1/3)) * 12) + (0.5 / (1 + 0.5 + (1/3)) * 10) + (0.5 / (0 + 0.5 + (1/3)) * 30) 
u3 = ((1/3) / (0 + 0 + (1/3)) * 12) + ((1/3) / (1 + 0.5 + (1/3)) * 10) + ((1/3) / (0 + 0.5 + (1/3)) * 30)
c.sum = u1 + u2 + u3


u1 = 2 * (0 / (0 * 2 + 0 * 1 + (1/3) * 1) * 12) + (1 / (1 * 2 + 0.5 * 1 + (1/3) * 1) * 10) + (0 / (0 * 2 + 0.5 * 1 + (1/3) * 1) * 30))
2 * 3.529412
u2 = 1 * (0 / (0 * 2 + 0 * 1 + (1/3) * 1) * 12) + (0.5 / (1 * 2 + 0.5 * 1 + (1/3) * 1) * 10) + (0.5 / (0 * 2 + 0.5 * 1 + (1/3) * 1) * 30) 
1 * 19.76471
u3 = 1 * ((1/3) / (0 * 2 + 0 * 1 + (1/3) * 1) * 12) + ((1/3) / (1 * 2 + 0.5 * 1 + (1/3) * 1) * 10) + ((1/3) / (0 * 2 + 0.5 * 1 + (1/3) * 1) * 30)
1 * 25.17647

c.sum = u1 + u2 + u3


c.vec <- c(12, 10, 30)
P.mat <- Matrix(matrix(c(0,1,0, 0,0.5,0.5, 1/3, 1/3, 1/3), nrow = 3, ncol = 3, byrow = F), sparse = T)
a.vec <- c(2, 1, 1)

SB_u_est <- function(c.vec, P.mat, a.vec) {
  P.trans <- t(P.mat)
  denominator.vec <- colSums(P.trans * a.vec)
  denominator.summary <- tibble(u = c(1:length(a.vec)), value = colSums(P.trans * a.vec)) # of length u
  fraction.vec <- colSums(matrix(((P.vec / denominator.vec) * c.vec), nrow = length(c.vec), ncol = length(a.vec)))
  u <- fraction.vec * a.vec
  return(u)
}

SB_u_est <- function(c.vec, P.mat, a.vec) {
  P.trans.mat <- t(P.mat)
  P.correct.mat <- P.mat
  P.correct.mat[P.correct.mat > 0] <- 1
  denominator.summary <- tibble(u = c(1:length(a.vec)), value = colSums(P.trans.mat * a.vec)) # of length u
  denominator.vec <- colSums(P.trans.mat * a.vec)
  
  
  corrector <- P.correct.mat * denominator.summary$value
  fraction.final <- colSums(((P.correct.mat / corrector) * P.mat) * c.vec, na.rm = T)
  
  u <- fraction.final * a.vec
  return(u)
}

SB_u_est()

P.mat.new <- P.mat
P.mat.new[P.mat.new > 0] <- 1
d <- P.mat.new * denominator.summary$value
e <- colSums(((P.mat.new / d) * P.vec) * c.vec, na.rm = T)
e * a.vec

sum(SB_u_est(c.vec, P.mat, a.vec))

sX <- summary(P.mat)
sY <- summary(Y)
sRes <- merge(sX, sY, by=c("i", "j"))

d <- as.matrix(P.mat)



