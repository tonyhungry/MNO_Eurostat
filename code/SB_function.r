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
  P.vec <- P.mat$
  fraction.vec <- colSums(matrix(((P.vec / denominator.vec) * c.vec), nrow = length(c.vec), ncol = length(a.vec)))
  u <- fraction.vec * a.vec
  return(u)
}

sum(SB_u_est(c.vec, P.mat, a.vec))



