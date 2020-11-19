require(tidyverse)
require(kableExtra)

#### Equal ####

esample <- readRDS("~/Desktop/Eurostat/u.est.non.inf.P.equal.sample.rds")
esample$pop.area.kind = as.factor(esample$pop.area.kind)
esample$internal.id = as.factor(esample$internal.id)

equal = esample %>% 
  rename( 
    u01 = u1, u02 = u2, u03 = u3, u04 = u4, u05 = u5,     
    u06 = u6, u07 = u7, u08 = u8, u09 = u9, u10 = u10, v100 = u100    
  ) %>% 
  pivot_longer(-c(internal.id, pop.area.kind,u.true,urban.dummy), names_to = "iter", values_to = "sim")

# saveRDS(equal, file = "equal.pop.rds")

# Simulated Population over Iterations
equal.plot = ggplot(data = equal, aes(x = iter, y = sim, group = internal.id)) + 
  geom_line(aes(color = pop.area.kind)) + labs(x = "Number of Iteration", y = "Simulated Population", title = "Equal Probability Matrix") + theme(axis.text.x = element_text(angle = 90))

saveRDS(equal.plot, file = "equal.plot.rds")

# Residual Plot

equalr = esample %>% 
  mutate(r001 = u.true - u1,
         r002 = u.true - u2,
         r005 = u.true - u5,
         r010 = u.true - u10,
         r020 = u.true - u20,
         r050 = u.true - u50,
         r100 = u.true - u100,
         ) %>%
  select(pop.area.kind,internal.id,r001,r002,r005,r010,r020,r050,r100) %>%
  pivot_longer(-c(internal.id, pop.area.kind), names_to = "iter", values_to = "resid")
  
equal.resid.plot = ggplot(data = equalr, aes(x = iter, y = resid, group = internal.id)) + 
  geom_line(aes(color = pop.area.kind)) + labs(x = "Number of Iteration", y = "Residual = Actual - Predicted", title = "Equal Probability Matrix ~ Residual") + theme(axis.text.x = element_text(angle = 90))

saveRDS(equal.resid.plot, file = "equal.resid.plot.rds")

# Root Mean Squared Prediction Error
equalc = esample %>% 
  mutate(r001 = (u.true - u1)^2,
         r002 = (u.true - u2)^2,
         r005 = (u.true - u5)^2,
         r010 = (u.true - u10)^2,
         r020 = (u.true - u20)^2,
         r050 = (u.true - u50)^2,
         r100 = (u.true - u100)^2,
  ) %>%
  group_by(pop.area.kind) %>% 
  summarise(i1 = sqrt(mean(r001)),
            i2 = sqrt(mean(r002)),
            i5 = sqrt(mean(r005)),
            i10 = sqrt(mean(r010)),
            i20 = sqrt(mean(r020)),
            i50 = sqrt(mean(r050)),
            i100 = sqrt(mean(r100))) 
  # %>% kbl(caption = "Comparison between simulation results using Mean Squared Prediction Error") %>% 
  #kable_minimal() 
saveRDS(equalc, file = "equal.rmspe.rds")

#### True ####

tsample <- readRDS("~/Desktop/Eurostat/u.est.non.inf.P.oracle.sample.rds")
tsample$pop.area.kind = as.factor(tsample$pop.area.kind)
tsample$internal.id = as.factor(tsample$internal.id)

tequal = tsample %>% 
  rename( 
    u01 = u1, u02 = u2, u03 = u3, u04 = u4, u05 = u5,     
    u06 = u6, u07 = u7, u08 = u8, u09 = u9, u10 = u10, v100 = u100    
  ) %>% 
  pivot_longer(-c(internal.id, pop.area.kind,u.true,urban.dummy), names_to = "iter", values_to = "sim")

# Simulated Population over Iterations
true.plot = ggplot(data = tequal, aes(x = iter, y = sim, group = internal.id)) + 
  geom_line(aes(color = pop.area.kind)) + labs(x = "Number of Iteration", y = "Simulated Population", title = "True Population Matrix") + theme(axis.text.x = element_text(angle = 90))

saveRDS(true.plot, file = "true.plot.rds")

# Residual Plot

tequalr = tsample %>% 
  mutate(r001 = u.true - u1,
         r002 = u.true - u2,
         r005 = u.true - u5,
         r010 = u.true - u10,
         r020 = u.true - u20,
         r050 = u.true - u50,
         r100 = u.true - u100,
  ) %>%
  select(pop.area.kind,internal.id,r001,r002,r005,r010,r020,r050,r100) %>%
  pivot_longer(-c(internal.id, pop.area.kind), names_to = "iter", values_to = "resid")

true.resid.plot = ggplot(data = tequalr, aes(x = iter, y = resid, group = internal.id)) + 
  geom_line(aes(color = pop.area.kind)) + labs(x = "Number of Iteration", y = "Residual = Actual - Predicted", title = "True Population Matrix ~ Residual") + theme(axis.text.x = element_text(angle = 90))

saveRDS(true.resid.plot, file = "true.resid.plot.rds")

# Root Mean Squared Prediction Error
tequalc = tsample %>% 
  mutate(r001 = (u.true - u1)^2,
         r002 = (u.true - u2)^2,
         r005 = (u.true - u5)^2,
         r010 = (u.true - u10)^2,
         r020 = (u.true - u20)^2,
         r050 = (u.true - u50)^2,
         r100 = (u.true - u100)^2,
  ) %>%
  group_by(pop.area.kind) %>% 
  summarise(i1 = sqrt(mean(r001)),
            i2 = sqrt(mean(r002)),
            i5 = sqrt(mean(r005)),
            i10 = sqrt(mean(r010)),
            i20 = sqrt(mean(r020)),
            i50 = sqrt(mean(r050)),
            i100 = sqrt(mean(r100)))   
  # %>% kbl(caption = "Comparison between simulation results using Mean Squared Prediction Error") %>% 
  # kable_minimal() 

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