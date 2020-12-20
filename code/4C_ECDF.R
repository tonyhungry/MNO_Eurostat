require(tidyverse)
require(ggthemes)

# Loading In
voro <- readRDS("~/OneDrive - Vysoká škola ekonomická v Praze/YAY/Estimates/Voronoi both/Voronoi.estimates.rds")
equal <- readRDS("~/OneDrive - Vysoká škola ekonomická v Praze/YAY/Estimates/P.equal/u.est.non.inf.P.equal.part801_1000_new.rds")[,202]
true <- readRDS("~/OneDrive - Vysoká škola ekonomická v Praze/YAY/Estimates/P.oracle/u.est.non.inf.P.oracle.part801_1000_new.rds")[,202]

# Voronoi Antenna
voro.ant.ECCDF = voro %>% 
  # sample_n(1000) %>%
  mutate(pop.plot = voronoi.est.antenna + 1) %>%  
  arrange(pop.plot) %>%  
  mutate(prob = 1 / n()) %>%  
  mutate(cum.prob = cumsum(prob)) %>%  
  mutate(log10.cum.prob.comp = 1 - cum.prob) %>%  
  mutate(log10.pop = pop.plot) %>%  
  mutate(cum.prob.comp = 1 - cum.prob)

voro.ant.eccdf.plot = voro.ant.ECCDF %>%   
  ggplot() + 
  geom_point(aes(x = log10.pop, y = log10.cum.prob.comp)) + 
  scale_color_ptol() + 
  ggtitle("", subtitle = "ECCDF of Voronoi ~ Antenna") +  
  labs(y = "Prob(Y > x)", x = "Mobile phones") + 
  ylim(-10, 0) +
  theme(legend.position = "bottom") 

voro %>%
  ggplot(aes(x=voronoi.est.antenna)) + geom_density()

voro %>%
  ggplot(aes(x=voronoi.est.antenna)) + geom_histogram(bins= 5)

voro.ant.ecdf.plot = voro %>%
  ggplot(aes(voronoi.est.antenna)) + stat_ecdf(pad = F) + 
  ggtitle("", subtitle = "ECDF of Voronoi ~ Antenna") +
  labs(y = "Prob(Y > x)", x = "Mobile phones")

# Voronoi Tower
voro.tow.ECCDF = voro %>% 
  # sample_n(1000) %>%
  mutate(pop.plot = voronoi.est.tower) %>%  
  arrange(pop.plot) %>%  
  mutate(prob = 1 / n()) %>%  
  mutate(cum.prob = cumsum(prob)) %>%  
  mutate(log10.cum.prob.comp = log10(1 - cum.prob)) %>%  
  mutate(log10.pop = log10(pop.plot)) %>%  
  mutate(cum.prob.comp = 1 - cum.prob)

voro.tow.eccdf.plot = voro.tow.ECCDF %>%   
  ggplot() + 
  geom_point(aes(x = log10.pop, y = log10.cum.prob.comp)) + 
  scale_color_ptol() + 
  ggtitle("", subtitle = "ECCDF of Voronoi ~ Tower") +  
  labs(y = "log10(Prob(Y > x))", x = "log10(Mobile phones)",  
       colour = "") + 
  ylim(-10, 0) +
  theme(legend.position = "bottom") 

voro.ant.ecdf.plot = voro %>%
  ggplot(aes(voronoi.est.tower)) + stat_ecdf(pad = F) + 
  ggtitle("", subtitle = "ECDF of Voronoi ~ Tower")

# MLE Equal 

mle.equal.ECCDF = as.data.frame(equal) %>% 
  # sample_n(1000) %>%
  mutate(pop.plot = equal + 1) %>%  
  arrange(pop.plot) %>%  
  mutate(prob = 1 / n()) %>%  
  mutate(cum.prob = cumsum(prob)) %>%  
  mutate(log10.cum.prob.comp = log10(1 - cum.prob)) %>%  
  mutate(log10.pop = log10(pop.plot)) %>%  
  mutate(cum.prob.comp = 1 - cum.prob)

mle.equal.eccdf.plot = mle.equal.ECCDF %>%   
  ggplot() + 
  geom_point(aes(x = log10.pop, y = log10.cum.prob.comp)) + 
  scale_color_ptol() + 
  ggtitle("", subtitle = "ECCDF of MLE ~ Equal") +  
  labs(y = "log10(Prob(Y > x))", x = "log10(Mobile phones)") + 
  ylim(-10, 0) +
  theme(legend.position = "bottom") 

mle.equal.ecdf.plot = as.data.frame(equal) %>%
  ggplot(aes(equal)) + stat_ecdf(pad = F) + 
  ggtitle("", subtitle = "ECDF of MLE ~ Equal")

# MLE true 

mle.true.ECCDF = as.data.frame(true) %>% 
  # sample_n(1000) %>%
  mutate(pop.plot = true + 1) %>%  
  arrange(pop.plot) %>%  
  mutate(prob = 1 / n()) %>%  
  mutate(cum.prob = cumsum(prob)) %>%  
  mutate(log10.cum.prob.comp = log10(1 - cum.prob)) %>%  
  mutate(log10.pop = log10(pop.plot)) %>%  
  mutate(cum.prob.comp = 1 - cum.prob)

mle.true.eccdf.plot = mle.true.ECCDF %>%   
  ggplot() + 
  geom_point(aes(x = log10.pop, y = log10.cum.prob.comp)) + 
  scale_color_ptol() + 
  ggtitle("", subtitle = "ECCDF of MLE ~ True") +  
  labs(y = "log10(Prob(Y > x))", x = "log10(Mobile phones)") + 
  ylim(-10, 0) +
  theme(legend.position = "bottom") 

mle.true.ecdf.plot = as.data.frame(true) %>%
  ggplot(aes(true)) + stat_ecdf(pad = F) + 
  ggtitle("", subtitle = "ECDF of MLE ~ True")

#### Potential New Work Flow for ECCDF ####
voroa = cbind(voro, equal, true)
colnames(voroa) = c("internal.id","pop","u.antenna","u.tower","u.equal","u.true")

eccdf <- function(x) { return( 1 - ecdf(x)(x) ) } # ECCDF function

vorol = voroa %>%
  pivot_longer(-c(pop,internal.id), names_to = "type", values_to = "final") %>%
  mutate(e = eccdf(final))

# ECDF
vorol %>%
  ggplot(aes(y =final)) + stat_ecdf() + 
  facet_grid(vars(type),scales="free") + 
  scale_color_ptol(breaks = c("u.antenna","u.tower","u.equal","u.true"))

# ECCDF
vorol %>%
  ggplot(aes(y = e)) + geom_line() + 
  facet_grid(vars(type),scales="free") + 
  scale_color_ptol(breaks = c("u.antenna","u.tower","u.equal","u.true"))