require(tidyverse)
require(ggthemes)

# Loading in

voro <- readRDS("~/OneDrive - Vysoká škola ekonomická v Praze/YAY/Estimates/Voronoi both/Voronoi.estimates.rds")
equal <- readRDS("~/OneDrive - Vysoká škola ekonomická v Praze/YAY/Estimates/P.equal/u.est.non.inf.P.equal.part801_1000.rds")[,202]
true <- readRDS("~/OneDrive - Vysoká škola ekonomická v Praze/YAY/Estimates/P.oracle/u.est.non.inf.P.oracle.part801_1000.rds")[,202]

voro = cbind(voro, equal, true)
colnames(voro) = c("internal.id","pop","u.antenna","u.tower","u.equal","u.true")
voro = voro [,-1]

bigu = sum(voro$pop)

# Average Absolute Discrepancy
aad.table = voro %>%
  transmute_at(vars(contains("u.")),funs("diff" = abs(pop - .))) %>% 
  summarise_all(funs("i" = sum(.)/bigu)) 
colnames(aad.table) = c("Voronoi ~ Antenna","Voronoi ~ Tower","MLE ~ Equal","MLE ~ True")
aadl = aad.table %>%
  pivot_longer(-c(), names_to = "type", values_to = "aad")

aad.comp.plot = aadl %>%
  ggplot(aes(x = type,y = aad, fill = type)) + 
  geom_bar(stat="identity") + 
  labs(x = "Estimation Type", y = "Average Absolute Discrepancy",title = "Comparison Across Different Estimation Strategies ~ AAD") + 
  theme(axis.text.x = element_text(angle = 90),legend.position = "bottom") +
  scale_fill_ptol()
  

saveRDS(aad.comp.plot, file = "aad.comp.plot.rds")

# Maximum Absolute Discrepancy
mad.table = voro %>%
  transmute_at(vars(contains("u")),funs("diff" = abs(pop - .))) %>% 
  summarise_all(funs("i" = sum(.)))
colnames(mad.table) = c("Voronoi ~ Antenna","Voronoi ~ Tower","MLE ~ Equal","MLE ~ True")
madl = mad.table %>%
  pivot_longer(-c(), names_to = "type", values_to = "mad")

mad.comp.plot = madl %>%
  ggplot(aes(x = type,y = mad, fill = type)) + 
  geom_bar(stat="identity") + 
  labs(x = "Estimation Type", y = "Maximum Absolute Discrepancy",title = "Comparison Across Different Estimation Strategies ~ MAD") + 
  theme(axis.text.x = element_text(angle = 90),legend.position = "bottom") +
  scale_fill_ptol()

saveRDS(mad.comp.plot, file = "mad.comp.plot.rds")
