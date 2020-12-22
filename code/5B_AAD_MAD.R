require(tidyverse)
require(kableExtra)

#### 1000 iters EQUAL POPULATION MATRIX ####

voro <- readRDS("~/OneDrive - Vysoká škola ekonomická v Praze/YAY/Estimates/Voronoi both/Voronoi.estimates.rds") # getting the true pop vector from another object
pop = voro$pop
voro = voro %>% ungroup()

# Average Absolute Discrepancy
equal1 <- readRDS("~/OneDrive - Vysoká škola ekonomická v Praze/YAY/Estimates/P.equal/u.est.non.inf.P.equal.part1_400_new.rds") 
equal1 = cbind(equal1, pop)

bigu = sum(equal1$pop) # useful to have this calculated and stored as a constant already 

aad1 = equal1 %>%  # mutating 
  transmute_at(vars(contains("u")),funs("diff" = abs(pop - .))) %>% 
  summarise_all(funs("i" = sum(.)/bigu))

colnames(aad1) = 0:400

equal2 <- readRDS("~/OneDrive - Vysoká škola ekonomická v Praze/YAY/Estimates/P.equal/u.est.non.inf.P.equal.part401_800_new.rds")
equal2 = cbind(equal2,pop)

aad2 = equal2 %>%  # mutating 
  transmute_at(vars(contains("u")),funs("diff" = abs(pop - .))) %>% 
  summarise_all(funs("i" = sum(.)/bigu))

colnames(aad2) = 401:801

equal3 <- readRDS("~/OneDrive - Vysoká škola ekonomická v Praze/YAY/Estimates/P.equal/u.est.non.inf.P.equal.part801_1000_new.rds")
equal3 = cbind(equal3,pop)

aad3 = equal3 %>%  # mutating 
  transmute_at(vars(contains("u")),funs("diff" = abs(pop - .))) %>% 
  summarise_all(funs("i" = sum(.)/bigu))

colnames(aad3) = 802:1002

aad = cbind(aad1,aad2,aad3)
aadt = aad %>%
  pivot_longer(-c(), names_to = "iter", values_to = "aad")

aadt$iter = as.numeric(aadt$iter)

equal.aad.plot = aadt %>% 
  ggplot(aes(x = iter, y = aad)) + 
  geom_line() + labs(x = "Number of Iteration", y = "Average Absolute Discrepancy", title = "Equal Probability Matrix ~ AAD") + theme(axis.text.x = element_text(angle = 90),legend.position = "bottom") +
  scale_x_continuous(breaks=seq(0, 1000, 50))

saveRDS(equal.aad.plot, file = "equal.aad.plot.rds")

# Maximum Absolute Discrepancy

mad1 = equal1 %>%  # mutating 
  transmute_at(vars(contains("u")),funs("diff" = abs(pop - .))) %>% 
  summarise_all(funs("i" = sum(.)))
colnames(mad1) = 0:400

mad2 = equal2 %>%  # mutating 
  transmute_at(vars(contains("u")),funs("diff" = abs(pop - .))) %>% 
  summarise_all(funs("i" = sum(.)))
colnames(mad2) = 401:801

mad3 = equal3 %>%  # mutating 
  transmute_at(vars(contains("u")),funs("diff" = abs(pop - .))) %>% 
  summarise_all(funs("i" = sum(.)))
colnames(mad3) = 802:1002

mad = cbind(mad1,mad2,mad3)
madt = mad %>%
  pivot_longer(-c(),names_to = "iter", values_to = "mad")

madt$iter = as.numeric(madt$iter)

equal.mad.plot = madt %>% 
  ggplot(aes(x = iter, y = mad)) + 
  geom_line() + labs(x = "Number of Iteration", y = "Maximum Absolute Discrepancy", title = "Equal Probability Matrix ~ MAD") + theme(axis.text.x = element_text(angle = 90),legend.position = "bottom") +
  scale_x_continuous(breaks=seq(0, 1000, 50))

saveRDS(equal.mad.plot, file = "equal.mad.plot.rds")

#### 1000 iters TRUE POPULATION MATRIX ####

rm(list = ls()) # remove unused objects

voro <- readRDS("~/OneDrive - Vysoká škola ekonomická v Praze/YAY/Estimates/Voronoi both/Voronoi.estimates.rds") # getting the true pop vector from another object
pop = voro$pop
rm(voro)

# Average Absolute Discrepancy
true1 <- readRDS("~/OneDrive - Vysoká škola ekonomická v Praze/YAY/Estimates/P.oracle/u.est.non.inf.P.oracle.part1_400_new.rds")
true1 = cbind(true1,pop)

bigu = sum(true1$pop) # useful to have this calculated and stored as a constant already 

aad1 = true1 %>%  # mutating 
  transmute_at(vars(contains("u")),funs("diff" = abs(pop - .))) %>% 
  summarise_all(funs("i" = sum(.)/bigu))

colnames(aad1) = 0:400
rm(true1)
gc()
gc()

true2 <- readRDS("~/OneDrive - Vysoká škola ekonomická v Praze/YAY/Estimates/P.oracle/u.est.non.inf.P.oracle.part401_800_new.rds")
true2 = cbind(true2,pop)

aad2 = true2 %>%  # mutating 
  transmute_at(vars(contains("u")),funs("diff" = abs(pop - .))) %>% 
  summarise_all(funs("i" = sum(.)/bigu))

colnames(aad2) = 401:801
rm(true2)
gc()
gc()

true3 <- readRDS("~/OneDrive - Vysoká škola ekonomická v Praze/YAY/Estimates/P.oracle/u.est.non.inf.P.oracle.part801_1000_new.rds")
true3 = cbind(true3,pop)

aad3 = true3 %>%  # mutating 
  transmute_at(vars(contains("u")),funs("diff" = abs(pop - .))) %>% 
  summarise_all(funs("i" = sum(.)/bigu))

colnames(aad3) = 802:1002
rm(true3)
gc()
gc()

aad = cbind(aad1,aad2,aad3)
aadt = aad %>%
  pivot_longer(-c(), names_to = "iter", values_to = "aad")

aadt$iter = as.numeric(aadt$iter)

true.aad.plot = aadt %>% 
  ggplot(aes(x = iter, y = aad)) + 
  geom_line() + labs(x = "Number of Iteration", y = "Average Absolute Discrepancy", title = "True Probability Matrix ~ AAD") + theme(axis.text.x = element_text(angle = 90),legend.position = "bottom") +
  scale_x_continuous(breaks=seq(0, 1000, 50))

saveRDS(true.aad.plot, file = "true.aad.plot.rds")

# Maximum Absolute Discrepancy

rm(list = ls())

voro <- readRDS("~/OneDrive - Vysoká škola ekonomická v Praze/YAY/Estimates/Voronoi both/Voronoi.estimates.rds") # getting the true pop vector from another object
pop = voro$pop
rm(voro)

true1 <- readRDS("~/OneDrive - Vysoká škola ekonomická v Praze/YAY/Estimates/P.oracle/u.est.non.inf.P.oracle.part1_400_new.rds")
true1 = cbind(true1,pop)
mad1 = true1 %>%  # mutating 
  transmute_at(vars(contains("u")),funs("diff" = abs(pop - .))) %>% 
  summarise_all(funs("i" = sum(.)))
colnames(mad1) = 0:400
rm(true1)
gc()
gc()

true2 <- readRDS("~/OneDrive - Vysoká škola ekonomická v Praze/YAY/Estimates/P.oracle/u.est.non.inf.P.oracle.part401_800_new.rds")
true2 = cbind(true2,pop)
mad2 = true2 %>%  # mutating 
  transmute_at(vars(contains("u")),funs("diff" = abs(pop - .))) %>% 
  summarise_all(funs("i" = sum(.)))
colnames(mad2) = 401:801
rm(true2)
gc()
gc()

true3 <- readRDS("~/OneDrive - Vysoká škola ekonomická v Praze/YAY/Estimates/P.oracle/u.est.non.inf.P.oracle.part801_1000_new.rds")
true3 = cbind(true3,pop)
mad3 = true3 %>%  # mutating 
  transmute_at(vars(contains("u")),funs("diff" = abs(pop - .))) %>% 
  summarise_all(funs("i" = sum(.)))
colnames(mad3) = 802:1002
rm(true3)
gc()
gc()

mad = cbind(mad1,mad2,mad3)
madt = mad %>%
  pivot_longer(-c(),names_to = "iter", values_to = "mad")

madt$iter = as.numeric(madt$iter)

true.mad.plot = madt %>% 
  ggplot(aes(x = iter, y = mad)) + 
  geom_line() + labs(x = "Number of Iteration", y = "Maximum Absolute Discrepancy", title = "True Probability Matrix ~ MAD") + theme(axis.text.x = element_text(angle = 90),legend.position = "bottom") +
  scale_x_continuous(breaks=seq(0, 1000, 50))

saveRDS(true.mad.plot, file = "true.mad.plot.rds")

rm(list = ls())

#### Unused 100 iters EQUAL PROBABILITY MATRIX ####

# equal <- readRDS("~/OneDrive - Vysoká škola ekonomická v Praze/YAY/working objects/u.est.non.inf.P.equal.rds")
# equal = equal %>% rename(pop = u.true)
# 
# # Average Absolute Discrepancy
# bigu = sum(equal$pop) # useful to have this calculated and stored as a constant already 
# aadt = equal %>%  # mutating 
#   transmute_at(vars(contains("u")),funs("diff" = abs(pop - .))) %>% 
#   summarise_all(funs("i" = sum(.)/bigu)) %>%
#   pivot_longer(-c(), names_to = "iter", values_to = "aad")
# 
# aadt$iter = 0:100
# 
# equal.aad.plot = aadt %>% 
#   ggplot(aes(x = iter, y = aad)) + 
#   geom_line() + labs(x = "Number of Iteration", y = "Average Absolute Discrepancy", title = "Equal Probability Matrix ~ AAD") + theme(axis.text.x = element_text(angle = 90),legend.position = "bottom") +
#   scale_x_continuous(breaks=seq(0, 100, 5))
# 
# # Maximum Absolute Discrepancy
# 
# madt = equal %>%  # mutating 
#   transmute_at(vars(contains("u")),funs("diff" = abs(pop - .))) %>% 
#   summarise_all(funs("i" = sum(.))) %>%
#   pivot_longer(-c(), names_to = "iter", values_to = "mad")
# 
# madt$iter = 0:100
# 
# equal.mad.plot = madt %>% 
#   ggplot(aes(x = iter, y = mad)) + 
#   geom_line() + labs(x = "Number of Iteration", y = "Maximum Absolute Discrepancy", title = "Equal Probability Matrix ~ AAD") + theme(axis.text.x = element_text(angle = 90),legend.position = "bottom") +
#   scale_x_continuous(breaks=seq(0, 100, 5))
