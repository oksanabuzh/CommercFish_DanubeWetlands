# Correlation among predictors 


# Predictors:
# Nat.LndCov.Ext - Extent of natural land cover types
# Nat.LndCov.Div - Diversity of natural land cover types
# Nat.Bank - Presence of natural bank material
# Nat.Littor.Zone - Extent of natural littoral zone
# Littor.Habitat.Ext - Extent of littoral habitat features
# Littor.Habitat.Div - Diversity of littoral habitat features
# Macroph_SR - Macrophyte species richness
# Macroph_Cover - Macrophyte total cover
# Macroph_Invas.Prcnt - Proportion of invasive species (% out of total cover) 

# Macrophyte groups:
# A - emergent broad-leaved herbs
# B - emergent reeds/sedges/rushes
# C - floating-leaved (rooted)
# D - free-floating
# E - submerged fine- and dissected-leaved
# F - submerged broad-leaved
# G - submerged linear-leaved macrophytes
# H - filamentous algae



# Data----

k.data <- read.csv( "Data/comer_fish.csv")
str(k.data)
names(k.data)


# create new df with variable means per each study system 

dat <- k.data %>% 
  group_by(System) %>% 
  summarise(Nat.LndCov.Ext = mean(Extent.of.natural.land.cover.types),
            Nat.LndCov.Div = mean(Diveristy.of.natural.land.cover.types),
            Nat.Bank = mean(Bank.naturalness..presence.of.natural.bank.material.),
            Nat.Littor.Zone = mean(Extent.of.natural.littoral.zone),
            Littor.Habitat.Ext = mean(Extent.of.littoral.habitat.features), 
            Littor.Habitat.Div = mean(Diversity.of.littoral.habitat.features),
            Macroph_SR = mean(Macrophytes_SpRich),
            Macroph_Cover = mean(Macrophytes_cover),
            Macroph_Invas.Prcnt = mean(Macrophytes_cover_INVASIVE_prcnt),
            A=mean(A), B=mean(B), C=mean(C), D=mean(D), E=mean(E), F=mean(F), G=mean(G), H=mean(H))

  
names(dat)
# correlation
df1 <- dat[2:10]
names(df1)

df2 <- dat[11:18]
names(df2)

corl1 <- cor(df1, method = c("pearson"))
round(corl1, 2)


corl2 <- cor(df2, method = c("pearson"))
round(corl2, 2)

write.csv(round(corl1, 2), "Results/corr_M1.csv")
write.csv(round(corl2, 2), "Results/corr_M2.csv")



# Plot  correlations
library(ggcorrplot)
x11(height=9,width=8.5)
ggcorrplot(corl1,  type="lower", lab = TRUE)
ggsave("Results/FigS1.png", width = 15, height = 15, units = "cm")

x11(height=9,width=8.5)
ggcorrplot(corl2,  type="lower", lab = TRUE)
ggsave("Results/FigS2.png", width = 15, height = 15, units = "cm")

