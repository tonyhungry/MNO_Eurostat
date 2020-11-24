require(tidyverse)
require(kableExtra)
require(ggthemes)

# Change the ticks to showing a few ticks
# Change the simulated to estimated
# EQUAL and TRUE

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
  iters = c(iters, paste("000",x,sep = ""))
}
for (x in 10:99) {
  iters = c(iters, paste("00",x,sep = ""))
}
for (x in 100:400) {
  iters = c(iters, paste("0",x,sep = ""))
}

colnames(esample)[2:402] = iters

iters2 = c()
for (x in 401:999) {
  iters2 = c(iters2, paste("0",x,sep = ""))
}
for (x in 1000:1002){
  iters2 = c(iters2, paste("",x,sep = ""))
}
colnames(esample)[406:1007] = iters2

equal = esample %>% 
  select(-c(pop)) %>% 
  pivot_longer(-c(internal.id, pop.area.kind,u.true.x), names_to = "iter", values_to = "sim")

equal$iter = as.numeric(equal$iter)

# saveRDS(equal, file = "equal.pop.rds")

# Simulated Population over Iterations
equal.plot = equal %>% 
  ggplot(aes(x = iter, y = sim, group = internal.id)) + 
  geom_line(aes(color = pop.area.kind)) + labs(x = "Number of Iteration", y = "Estimated Population", title = "Equal Probability Matrix") + theme(axis.text.x = element_text(angle = 90,size=10)) + 
  scale_color_ptol(breaks = c("Rural", "Suburban", "Urban")) + 
  facet_grid(vars(pop.area.kind),scales="free") +
  scale_x_continuous(breaks=seq(0, 1000, 50))

saveRDS(equal.plot, file = "equal.plot.rds")

# Residual Plot

esample <- readRDS("~/Desktop/Eurostat/est.equal.1000iter.sample.rds")
esample = esample %>% select(-c(u.true.y,pop.area.kind.helper))

niters = c() 
for (x in 0:9) {
  niters = c(niters, paste("u000",x,sep = ""))
}
for (x in 10:99) {
  niters = c(niters, paste("u00",x,sep = ""))
}
for (x in 100:400) {
  niters = c(niters, paste("u0",x,sep = ""))
}

colnames(esample)[2:402] = niters

niters2 = c()
for (x in 401:999) {
  niters2 = c(niters2, paste("u0",x,sep = ""))
}
for (x in 1000:1002){
  niters2 = c(niters2, paste("u",x,sep = ""))
}
colnames(esample)[406:1007] = niters2

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

saveRDS(equal.resid.plot, file = "equal.resid.plot.rds")

#### TRUE POPULATION MATRIX ####

tsample <- readRDS("~/Desktop/Eurostat/est.true.1000iter.sample.rds")
tsample$internal.id = as.factor(tsample$internal.id)
colnames(tsample)

colnames(tsample)[2:402] = iters
colnames(tsample)[405:1006] = iters2

tequal = tsample %>%
  pivot_longer(-c(internal.id, pop.area.kind,pop), names_to = "iter", values_to = "sim") 

tequal$iter = as.numeric(tequal$iter)

# Estimated Population over Iterations plot
true.plot = ggplot(data = tequal, aes(x = iter, y = sim, group = internal.id)) + 
  geom_line(aes(color = pop.area.kind)) + labs(x = "Number of Iteration", y = "Estimated Population",     title = "True Population Matrix") + 
  theme(axis.text.x = element_text(angle = 90, size=10)) +
  scale_color_ptol(breaks = c("Rural", "Suburban", "Urban")) + 
  facet_grid(vars(pop.area.kind), scales = "free") +
  scale_x_continuous(breaks=seq(0, 1000, 50))

saveRDS(true.plot, file = "true.plot.rds")

# Residual Plot

colnames(tsample)[2:402] = niters
colnames(tsample)[405:1006] = niters2

tequalr = tsample %>% 
  mutate(r001 = pop - u0001, r002 = pop - u0002, r005 = pop - u0005,
         r010 = pop - u0010, r020 = pop - u0020, r050 = pop - u0050,
         r100 = pop - u0100, r200 = pop - u0200, r300 = pop - u0300,
         r400 = pop - u0400, r500 = pop - u0500, r600 = pop - u0600,
         r700 = pop - u0700, r800 = pop - u0800, r900 = pop - u0900,
         r1000 = pop - u1000
  ) %>%
  select(pop.area.kind,internal.id,r001,r002,r005,r010,r020,r050,r100,r200,r300,r400,r500,r600,r700,r800) %>%
  pivot_longer(-c(internal.id, pop.area.kind), names_to = "iter", values_to = "resid")

true.resid.plot = ggplot(data = tequalr, aes(x = iter, y = resid, group = internal.id)) + 
  geom_line(aes(color = pop.area.kind)) + labs(x = "Number of Iteration", y = "Residual = Actual - Predicted", title = "True Population Matrix ~ Residual") + 
  theme(axis.text.x = element_text(angle = 90)) + 
  scale_color_ptol(breaks = c("Rural", "Suburban", "Urban")) + 
  facet_grid(vars(pop.area.kind)) 

saveRDS(true.resid.plot, file = "true.resid.plot.rds")

#### Unused Codes ####

# EQUAL: Root Mean Squared Prediction Error
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

# TRUE: Root Mean Squared Prediction Error
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