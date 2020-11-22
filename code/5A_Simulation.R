require(tidyverse)
require(kableExtra)
require(ggthemes)

#### EQUAL POPULATION MATRIX ####

esample <- readRDS("~/Desktop/Eurostat/u.est.non.inf.P.equal.sample.rds")
# esample$pop.area.kind = as.factor(esample$pop.area.kind)
esample$internal.id = as.factor(esample$internal.id)

iters = c() 
for (x in 0:9) {
  iters = c(iters, paste("u00",x,sep = ""))
}
for (x in 10:99) {
  iters = c(iters, paste("u0",x,sep = ""))
}
for (x in 100:400) {
  iters = c(iters, paste("u",x,sep = ""))
}

colnames(esample)[2:402] = iters

equal = esample %>% 
  select(-c(pop.true,pop,pop.area.kind.helper,suburban.dummy,urban.dummy)) %>% 
  mutate(pop.area.kind = case_when(u.true == "0" ~ "Uninhabitated", TRUE ~ pop.area.kind)) %>%
  pivot_longer(-c(internal.id, pop.area.kind,u.true), names_to = "iter", values_to = "sim")

# saveRDS(equal, file = "equal.pop.rds")

# Simulated Population over Iterations
equal.plot = equal %>% 
  ggplot(aes(x = iter, y = sim, group = internal.id)) + 
  geom_line(aes(color = pop.area.kind)) + labs(x = "Number of Iteration", y = "Simulated Population", title = "Equal Probability Matrix") + theme(axis.text.x = element_text(angle = 90,size=2)) + 
  scale_color_ptol(breaks = c("Uninhabitated", "Rural", "Suburban", "Urban")) + 
  facet_grid(vars(pop.area.kind))

# saveRDS(equal.plot, file = "equal.plot.rds")

# Residual Plot

equalr = esample %>% 
  mutate(r001 = u.true - u001, r002 = u.true - u002, r005 = u.true - u005,
         r010 = u.true - u010, r020 = u.true - u020, r050 = u.true - u050,
         r100 = u.true - u100, r200 = u.true - u200, r300 = u.true - u300,
         r400 = u.true - u400
         ) %>%
  mutate(pop.area.kind = case_when(u.true == "0" ~ "Uninhabitated", TRUE ~ pop.area.kind)) %>%
  select(pop.area.kind,internal.id,r001,r002,r005,r010,r020,r050,r100,r200,r300,r400) %>%
  pivot_longer(-c(internal.id, pop.area.kind), names_to = "iter", values_to = "resid")
  
equal.resid.plot = equalr %>%
  ggplot(aes(x = iter, y = resid, group = internal.id)) + 
  geom_line(aes(color = pop.area.kind)) + labs(x = "Number of Iteration", y = "Residual = Actual - Predicted", title = "Equal Probability Matrix ~ Residual") + theme(axis.text.x = element_text(angle = 90)) + scale_color_ptol(breaks = c("Uninhabitated", "Rural", "Suburban", "Urban")) + 
  facet_grid(vars(pop.area.kind)) 

# saveRDS(equal.resid.plot, file = "equal.resid.plot.rds")

# Root Mean Squared Prediction Error
equalc = esample %>% 
  mutate(r001 = (u.true - u001)^2, r002 = (u.true - u002)^2, r005 = (u.true - u005)^2,
         r010 = (u.true - u010)^2, r020 = (u.true - u020)^2, r050 = (u.true - u050)^2,
         r100 = (u.true - u100)^2, r200 = (u.true - u200)^2, r300 = (u.true - u300)^2,
         r400 = (u.true - u400)^2
  ) %>%
  group_by(pop.area.kind) %>% 
  summarise(i001 = sqrt(mean(r001)), i002 = sqrt(mean(r002)), i005 = sqrt(mean(r005)),
            i010 = sqrt(mean(r010)), i020 = sqrt(mean(r020)), i050 = sqrt(mean(r050)),
            i100 = sqrt(mean(r100)), i200 = sqrt(mean(r200)), i300 = sqrt(mean(r300)),
            i400 = sqrt(mean(r400))) %>% 
  kbl(caption = "Comparison between simulation results using Mean Squared Prediction Error") %>% 
  kable_minimal() 

# saveRDS(equalc, file = "equal.rmspe.rds")

#### TRUE POPULATION MATRIX ####

tsample <- readRDS("~/Desktop/Eurostat/u.est.non.inf.P.true.final.sample.rds")
tsample$internal.id = as.factor(tsample$internal.id)

colnames(tsample)[2:402] = iters

tequal = tsample %>%
  select(-c(pop.true,pop,pop.area.kind.helper,suburban.dummy,urban.dummy)) %>%
  mutate(pop.area.kind = case_when(u.true == "0" ~ "Uninhabitated", TRUE ~ pop.area.kind)) %>%
  pivot_longer(-c(internal.id, pop.area.kind,u.true), names_to = "iter", values_to = "sim") 

# Simulated Population over Iterations plot
true.plot = ggplot(data = tequal, aes(x = iter, y = sim, group = internal.id)) + 
  geom_line(aes(color = pop.area.kind)) + labs(x = "Number of Iteration", y = "Simulated Population", title = "True Population Matrix") + theme(axis.text.x = element_text(angle = 90, size=2)) + scale_color_ptol(breaks = c("Uninhabitated", "Rural", "Suburban", "Urban")) + 
  facet_grid(vars(pop.area.kind)) 

saveRDS(true.plot, file = "true.plot.rds")

# Residual Plot

tequalr = tsample %>% 
  mutate(r001 = u.true - u001, r002 = u.true - u002, r005 = u.true - u005,
         r010 = u.true - u010, r020 = u.true - u020, r050 = u.true - u050,
         r100 = u.true - u100, r200 = u.true - u200, r300 = u.true - u300,
         r400 = u.true - u400
  ) %>%
  mutate(pop.area.kind = case_when(u.true == "0" ~ "Uninhabitated", TRUE ~ pop.area.kind)) %>%
  select(pop.area.kind,internal.id,r001,r002,r005,r010,r020,r050,r100) %>%
  pivot_longer(-c(internal.id, pop.area.kind), names_to = "iter", values_to = "resid")

true.resid.plot = ggplot(data = tequalr, aes(x = iter, y = resid, group = internal.id)) + 
  geom_line(aes(color = pop.area.kind)) + labs(x = "Number of Iteration", y = "Residual = Actual - Predicted", title = "True Population Matrix ~ Residual") + 
  theme(axis.text.x = element_text(angle = 90)) + 
  scale_color_ptol(breaks = c("Uninhabitated", "Rural", "Suburban", "Urban")) + 
  facet_grid(vars(pop.area.kind)) 

saveRDS(true.resid.plot, file = "true.resid.plot.rds")

# Root Mean Squared Prediction Error
tequalc = tsample %>% 
  mutate(r001 = (u.true - u001)^2, r002 = (u.true - u002)^2, r005 = (u.true - u005)^2,
         r010 = (u.true - u010)^2, r020 = (u.true - u020)^2, r050 = (u.true - u050)^2,
         r100 = (u.true - u100)^2, r200 = (u.true - u200)^2, r300 = (u.true - u300)^2,
         r400 = (u.true - u400)^2
  ) %>%
  group_by(pop.area.kind) %>% 
  summarise(i001 = sqrt(mean(r001)), i002 = sqrt(mean(r002)), i005 = sqrt(mean(r005)),
            i010 = sqrt(mean(r010)), i020 = sqrt(mean(r020)), i050 = sqrt(mean(r050)),
            i100 = sqrt(mean(r100)), i200 = sqrt(mean(r200)), i300 = sqrt(mean(r300)),
            i400 = sqrt(mean(r400))
            ) %>% 
  kbl(caption = "Comparison between simulation results using Mean Squared Prediction Error") %>% 
  kable_minimal() 

saveRDS(tequalc, file = "true.rmspe.rds")

#### Unused Codes ####
oracle = sample[c(22:24)] %>% 
  select(oracle, pop,pop.area.kind) %>%
  group_by(pop.area.kind) %>%
  mutate(pe = (mean(pop - oracle))^2)

# Mean Squared Prediction Error
sum(oracle$pe[1:30]) # rural
sum(oracle$pe[31:60]) # Suburban
sum(oracle$pe[61:90]) # Urban
oracle.s = c(sum(oracle$pe[1:30]),sum(oracle$pe[31:60]),sum(oracle$pe[61:90]))

df = rbind(true.p,oracle.s,equal.p)
colnames(df) = c("Rural","Suburabn","Urban")
df %>%   
  kbl(caption = "Comparison between simulation results using Mean Squared Prediction Error") %>% 
  kable_minimal() 