require(tidyverse)
require(kableExtra)

#### 1000 iters TRUE POPULATION MATRIX
# Average Absolute Discrepancy
true1 <- readRDS("~/OneDrive - Vysoká škola ekonomická v Praze/YAY/working objects/u.est.non.inf.P.oracle.rds")
true1 = true1 %>% rename(pop = u.true)

bigu = sum(true1$pop) # useful to have this calculated and stored as a constant already 

aad1 = true1 %>%  # mutating 
  transmute_at(vars(contains("u")),funs("diff" = abs(pop - .))) %>% 
  summarise_all(funs("i" = sum(.)/bigu))

colnames(aad1) = 0:400

true2 <- readRDS("~/OneDrive - Vysoká škola ekonomická v Praze/YAY/working objects/u.est.non.inf.P.equal.part401_800.rds")
true2 = true2 %>% rename(pop = u.true)

aad2 = true2 %>%  # mutating 
  transmute_at(vars(contains("u")),funs("diff" = abs(pop - .))) %>% 
  summarise_all(funs("i" = sum(.)/bigu))

colnames(aad2) = 401:801

true3 <- readRDS("~/OneDrive - Vysoká škola ekonomická v Praze/YAY/working objects/u.est.non.inf.P.equal.part801_1000.rds")
true3 = true3 %>% rename(pop = u.true)

aad3 = true3 %>%  # mutating 
  transmute_at(vars(contains("u")),funs("diff" = abs(pop - .))) %>% 
  summarise_all(funs("i" = sum(.)/bigu))

colnames(aad3) = 802:1002

aad = cbind(aad1,aad2,aad3)
aadt = aad %>%
  pivot_longer(-c(), names_to = "iter", values_to = "aad")

aadt$iter = as.numeric(aadt$iter)

true.aad.plot = aadt %>% 
  ggplot(aes(x = iter, y = aad)) + 
  geom_line() + labs(x = "Number of Iteration", y = "Average Absolute Discrepancy", title = "True Probability Matrix ~ AAD") + theme(axis.text.x = element_text(angle = 90),legend.position = "bottom") +
  scale_x_continuous(breaks=seq(0, 1000, 50))

# Maximum Absolute Discrepancy

mad1 = true1 %>%  # mutating 
  transmute_at(vars(contains("u")),funs("diff" = abs(pop - .))) %>% 
  summarise_all(funs("i" = sum(.)))
colnames(mad1) = 0:400

mad2 = true2 %>%  # mutating 
  transmute_at(vars(contains("u")),funs("diff" = abs(pop - .))) %>% 
  summarise_all(funs("i" = sum(.)))
colnames(mad2) = 401:801

mad3 = true3 %>%  # mutating 
  transmute_at(vars(contains("u")),funs("diff" = abs(pop - .))) %>% 
  summarise_all(funs("i" = sum(.)))
colnames(mad3) = 802:1002

mad = cbind(mad1,mad2,mad3)
madt = mad %>%
  pivot_longer(-c(),names_to = "iter", values_to = "mad")

madt$iter = as.numeric(madt$iter)

true.mad.plot = madt %>% 
  ggplot(aes(x = iter, y = mad)) + 
  geom_line() + labs(x = "Number of Iteration", y = "Maximum Absolute Discrepancy", title = "True Probability Matrix ~ MAD") + theme(axis.text.x = element_text(angle = 90),legend.position = "bottom") +
  scale_x_continuous(breaks=seq(0, 1000, 50))

#### 100 iters EQUAL PROBABILITY MATRIX ####

equal <- readRDS("~/OneDrive - Vysoká škola ekonomická v Praze/YAY/working objects/u.est.non.inf.P.equal.rds")
equal = equal %>% rename(pop = u.true)

# Average Absolute Discrepancy
bigu = sum(equal$pop) # useful to have this calculated and stored as a constant already 
aadt = equal %>%  # mutating 
  transmute_at(vars(contains("u")),funs("diff" = abs(pop - .))) %>% 
  summarise_all(funs("i" = sum(.)/bigu)) %>%
  pivot_longer(-c(), names_to = "iter", values_to = "aad")

aadt$iter = 0:100

equal.aad.plot = aadt %>% 
  ggplot(aes(x = iter, y = aad)) + 
  geom_line() + labs(x = "Number of Iteration", y = "Average Absolute Discrepancy", title = "Equal Probability Matrix ~ AAD") + theme(axis.text.x = element_text(angle = 90),legend.position = "bottom") +
  scale_x_continuous(breaks=seq(0, 100, 5))

# Maximum Absolute Discrepancy

madt = equal %>%  # mutating 
  transmute_at(vars(contains("u")),funs("diff" = abs(pop - .))) %>% 
  summarise_all(funs("i" = sum(.))) %>%
  pivot_longer(-c(), names_to = "iter", values_to = "mad")

madt$iter = 0:100

equal.mad.plot = madt %>% 
  ggplot(aes(x = iter, y = mad)) + 
  geom_line() + labs(x = "Number of Iteration", y = "Maximum Absolute Discrepancy", title = "Equal Probability Matrix ~ AAD") + theme(axis.text.x = element_text(angle = 90),legend.position = "bottom") +
  scale_x_continuous(breaks=seq(0, 100, 5))