require(tidyverse)
require(ggthemes)

# Loading In
voro <- readRDS("~/OneDrive - Vysoká škola ekonomická v Praze/YAY/Estimates/Voronoi both/Voronoi.estimates.rds")
equal <- readRDS("~/OneDrive - Vysoká škola ekonomická v Praze/YAY/Estimates/P.equal/u.est.non.inf.P.equal.part801_1000.rds")[,202]
true <- readRDS("~/OneDrive - Vysoká škola ekonomická v Praze/YAY/Estimates/P.oracle/u.est.non.inf.P.oracle.part801_1000.rds")[,202]

voroa = cbind(voro, equal, true)
colnames(voroa) = c("internal.id","pop","u.antenna","u.tower","u.equal","u.true")

eccdf <- function(x) { return( 1 - ecdf(x)(x) ) } # ECCDF function

vorol = voroa %>%
  pivot_longer(-c(pop,internal.id), names_to = "type", values_to = "final") %>%
  mutate(e = eccdf(final))

vorol %>%
  ggplot(aes(y = e)) + 
  facet_grid(vars(type),scales="free") + 
  scale_color_ptol(breaks = c("u.antenna","u.tower","u.equal","u.true"))