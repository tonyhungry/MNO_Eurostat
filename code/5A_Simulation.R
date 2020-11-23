require(tidyverse)
require(kableExtra)
require(ggthemes)

#### EQUAL POPULATION MATRIX ####
# sample <- readRDS("~/Desktop/Eurostat/est.equal.1000iter.sample.rds")
esample <- readRDS("~/Desktop/Eurostat/est.equal.1000iter.sample.rds")
# esample$pop.area.kind = as.factor(esample$pop.area.kind)
esample$internal.id = as.factor(esample$internal.id)
esample = esample %>% select(-c(u.true.y))
esample = esample %>% select(-c(pop.area.kind.helper,u.true))
length(colnames(esample)[406:1007])

iters = c() 
for (x in 0:9) {
  iters = c(iters, paste("u000",x,sep = ""))
}
for (x in 10:99) {
  iters = c(iters, paste("u00",x,sep = ""))
}
for (x in 100:400) {
  iters = c(iters, paste("u0",x,sep = ""))
}

colnames(esample)[2:402] = iters

iters2 = c()
for (x in 401:999) {
  iters2 = c(iters2, paste("u0",x,sep = ""))
}
for (x in 1000:1002){
  iters2 = c(iters2, paste("u",x,sep = ""))
}
colnames(esample)[406:1007] = iters2

equal = esample %>% 
  select(-c(pop)) %>% 
  pivot_longer(-c(internal.id, pop.area.kind,u.true.x), names_to = "iter", values_to = "sim") %>%
  mutate(lsim = log(sim, base=10))

# saveRDS(equal, file = "equal.pop.rds")

# Simulated Population over Iterations
equal.plot = equal %>% 
  ggplot(aes(x = iter, y = sim, group = internal.id)) + 
  geom_line(aes(color = pop.area.kind)) + labs(x = "Number of Iteration", y = "Simulated Population", title = "Equal Probability Matrix") + theme(axis.text.x = element_text(angle = 45,size=2)) + 
  scale_color_ptol(breaks = c("Rural", "Suburban", "Urban")) + 
  facet_grid(vars(pop.area.kind),scales="free")

saveRDS(equal.plot, file = "equal.plot.rds")

equal.lplot = equal %>% 
  ggplot(aes(x = iter, y = lsim, group = internal.id)) + 
  geom_line(aes(color = pop.area.kind)) + labs(x = "Number of Iteration", y = "log(Simulated Population), base = 10", title = "Equal Probability Matrix") + theme(axis.text.x = element_text(angle = 90,size=2)) + 
  scale_color_ptol(breaks = c("Rural", "Suburban", "Urban")) + 
  facet_grid(vars(pop.area.kind))

saveRDS(equal.lplot, file = "equal.lplot.rds")

# Residual Plot

esample <- readRDS("~/Desktop/Eurostat/est.equal.1000iter.sample.rds")
esample = esample %>% select(-c(u.true.y,pop.area.kind.helper))
colnames(esample)[2:402] = iters
colnames(esample)[406:1007] = iters2

equalr = esample %>% 
  mutate(r0001 = u.true - u0001, r0002 = u.true - u0002, r0005 = u.true - u0005,
         r0010 = u.true - u0010, r0020 = u.true - u0020, r0050 = u.true - u0050,
         r0100 = u.true - u0100, r0200 = u.true - u0200, r0300 = u.true - u0300,
         r0400 = u.true - u0400, r0500 = u.true - u0500, r0600 = u.true - u0600,
         r0700 = u.true - u0700, r0800 = u.true - u0800, r0900 = u.true - u0900,
         r1000 = u.true - u1000
         ) %>%
  select(pop.area.kind,internal.id,r0001,r0002,r0005,r0010,r0020,r0050,r0100,r0200,r0300,r0400,r0500,r0600,r0700,r0800,r0900,r1000) %>%
  pivot_longer(-c(internal.id, pop.area.kind), names_to = "iter", values_to = "resid")
  
equal.resid.plot = equalr %>%
  ggplot(aes(x = iter, y = resid, group = internal.id)) + 
  geom_line(aes(color = pop.area.kind)) + labs(x = "Number of Iteration", y = "Residual = Actual - Predicted", title = "Equal Probability Matrix ~ Residual") + theme(axis.text.x = element_text(angle = 90)) + scale_color_ptol(breaks = c("Rural", "Suburban", "Urban")) + 
  facet_grid(vars(pop.area.kind)) 

# saveRDS(equal.resid.plot, file = "equal.resid.plot.rds")

# Root Mean Squared Prediction Error
equalc = esample %>% 
  mutate(r0001 = (u.true - u0001)^2, r0002 = (u.true - u0002)^2, r0005 = (u.true - u0005)^2,
         r0010 = (u.true - u0010)^2, r0020 = (u.true - u0020)^2, r0050 = (u.true - u0050)^2,
         r0100 = (u.true - u0100)^2, r0200 = (u.true - u0200)^2, r0300 = (u.true - u0300)^2,
         r0400 = (u.true - u0400)^2, r0500 = (u.true - u0500)^2, r0600 = (u.true - u0600)^2,
         r0700 = (u.true - u0700)^2, r0800 = (u.true - u0800)^2,
         r0900 = (u.true - u0900)^2, r1000 = (u.true - u1000)^2
  ) %>%
  group_by(pop.area.kind) %>% 
  summarise(i0001 = sqrt(mean(r0001)), i0002 = sqrt(mean(r0002)), i0005 = sqrt(mean(r0005)),
            i0010 = sqrt(mean(r0010)), i0020 = sqrt(mean(r0020)), i0050 = sqrt(mean(r0050)),
            i0100 = sqrt(mean(r0100)), i0200 = sqrt(mean(r0200)), i0300 = sqrt(mean(r0300)),
            i0400 = sqrt(mean(r0400)), i0500 = sqrt(mean(r0500)), i0600 = sqrt(mean(r0600)),
            i0700 = sqrt(mean(r0700)), i0800 = sqrt(mean(r0800)), i0900 = sqrt(mean(r0900)),
            i1000 = sqrt(mean(r1000))
            ) %>% 
  kbl(caption = "Comparison between simulation results using Mean Squared Prediction Error") %>% 
  kable_minimal() 

saveRDS(equalc, file = "equal.rmspe.rds")

#### TRUE POPULATION MATRIX ####

tsample <- readRDS("~/Desktop/Eurostat/est.true.1000iter.sample.rds")
tsample$internal.id = as.factor(tsample$internal.id)
colnames(tsample)
tsample = tsample %>% select(-c(u.true.y))

colnames(tsample)[2:402] = iters
colnames(tsample)[406:1007] = iters2
tsample = tsample[1:806]

tequal = tsample %>%
  select(-c(pop)) %>%
  pivot_longer(-c(internal.id, pop.area.kind,u.true.x), names_to = "iter", values_to = "sim") %>%
  mutate(lsim = log(sim, base = 10))

# Simulated Population over Iterations plot
true.plot = ggplot(data = tequal, aes(x = iter, y = sim, group = internal.id)) + 
  geom_line(aes(color = pop.area.kind)) + labs(x = "Number of Iteration", y = "Simulated Population",     title = "True Population Matrix, 800 iterations") + 
  theme(axis.text.x = element_text(angle = 90, size=2)) +
  scale_color_ptol(breaks = c("Rural", "Suburban", "Urban")) + 
  facet_grid(vars(pop.area.kind)) 

saveRDS(true.plot, file = "true.plot.rds")

true.lplot = ggplot(data = tequal, aes(x = iter, y = lsim, group = internal.id)) + 
  geom_line(aes(color = pop.area.kind)) + 
  labs(x = "Number of Iteration", y = "log(Simulated Population), base = 10", 
       title = "True Population Matrix, 800 iterations") + 
  theme(axis.text.x = element_text(angle = 90, size=2)) +
  scale_color_ptol(breaks = c("Rural", "Suburban", "Urban")) + 
  facet_grid(vars(pop.area.kind)) 

# Residual Plot

tequalr = tsample %>% 
  mutate(r001 = u.true.x - u0001, r002 = u.true.x - u0002, r005 = u.true.x - u0005,
         r010 = u.true.x - u0010, r020 = u.true.x - u0020, r050 = u.true.x - u0050,
         r100 = u.true.x - u0100, r200 = u.true.x - u0200, r300 = u.true.x - u0300,
         r400 = u.true.x - u0400, r500 = u.true.x - u0500, r600 = u.true.x - u0600,
         r700 = u.true.x - u0700, r800 = u.true.x - u0800
  ) %>%
  select(pop.area.kind,internal.id,r001,r002,r005,r010,r020,r050,r100,r200,r300,r400,r500,r600,r700,r800) %>%
  pivot_longer(-c(internal.id, pop.area.kind), names_to = "iter", values_to = "resid")

true.resid.plot = ggplot(data = tequalr, aes(x = iter, y = resid, group = internal.id)) + 
  geom_line(aes(color = pop.area.kind)) + labs(x = "Number of Iteration", y = "Residual = Actual - Predicted", title = "True Population Matrix ~ Residual") + 
  theme(axis.text.x = element_text(angle = 90)) + 
  scale_color_ptol(breaks = c("Rural", "Suburban", "Urban")) + 
  facet_grid(vars(pop.area.kind)) 

saveRDS(true.resid.plot, file = "true.resid.plot.rds")

# Root Mean Squared Prediction Error
tequalc = tsample %>% 
  mutate(r0001 = (u.true.x - u0001)^2, r0002 = (u.true.x - u0002)^2, r0005 = (u.true.x - u0005)^2,
         r0010 = (u.true.x - u0010)^2, r0020 = (u.true.x - u0020)^2, r0050 = (u.true.x - u0050)^2,
         r0100 = (u.true.x - u0100)^2, r0200 = (u.true.x - u0200)^2, r0300 = (u.true.x - u0300)^2,
         r0400 = (u.true.x - u0400)^2, r0500 = (u.true.x - u0500)^2, r0600 = (u.true.x - u0600)^2,
         r0700 = (u.true.x - u0700)^2, r0800 = (u.true.x - u0800)^2
  ) %>%
  group_by(pop.area.kind) %>% 
  summarise(i001 = sqrt(mean(r0001)), i002 = sqrt(mean(r0002)), i005 = sqrt(mean(r0005)),
            i010 = sqrt(mean(r0010)), i020 = sqrt(mean(r0020)), i050 = sqrt(mean(r0050)),
            i100 = sqrt(mean(r0100)), i200 = sqrt(mean(r0200)), i300 = sqrt(mean(r0300)),
            i400 = sqrt(mean(r0400)), i500 = sqrt(mean(r0500)), i600 = sqrt(mean(r0600)),
            i700 = sqrt(mean(r0700)), i800 = sqrt(mean(r0800))
            ) %>% 
  kbl(caption = "Comparison between simulation results using Mean Squared Prediction Error") %>% 
  kable_minimal() 

saveRDS(tequalc, file = "true.rmspe.rds")

#### Unused Codes ####

# The first 20 iterations
b = esample[,1:22] %>% 
  pivot_longer(-c(internal.id), names_to = "iter", values_to = "sim") %>%
  mutate(lsim = log(sim, base=10))
b %>% 
  ggplot(aes(x = iter, y = sim, group = internal.id)) + 
  geom_line()

a = equal[1:100,]
a %>% ggplot(aes(x = iter, y = sim, group = internal.id)) + 
  geom_line(aes(color = internal.id)) + labs(x = "Number of Iteration", y = "Simulated Population", title = "Equal Probability Matrix ~ Urban") + theme(axis.text.x = element_text(angle = 90,size=5))

equal %>% filter(pop.area.kind == "Urban") %>%
  ggplot(aes(x = iter, y = sim, group = internal.id)) + 
  geom_line(aes(color = internal.id)) + labs(x = "Number of Iteration", y = "Simulated Population", title = "Equal Probability Matrix ~ Urban") + theme(axis.text.x = element_text(angle = 90,size=2))