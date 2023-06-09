# Drivers of commercial fishes in wetlands

# response "Fish Abundance"

rm(list=ls(all=TRUE))

# Packages----


library(tidyverse)
library(ggplot2)
library(lme4)
library(MuMIn)
library(MASS)
library(car)

sessionInfo()
# R version 4.2.2 (2022-10-31 ucrt)
# Platform: x86_64-w64-mingw32/x64

# [1] car_3.1-1       carData_3.0-5   MASS_7.3-58.1   MuMIn_1.47.1    lme4_1.1-31     Matrix_1.5-1   
# [7] lubridate_1.9.2 forcats_1.0.0   stringr_1.5.0   dplyr_1.1.0     purrr_1.0.1     readr_2.1.4    
# [13] tidyr_1.3.0     tibble_3.1.8    ggplot2_3.4.1   tidyverse_2.0.0

citation()
citation("lme4")
citation("MASS")
citation("MuMIn")

# Data----


data <- read.csv( "Data/comer_fish.csv")
str(data)
names(data)


# Rename variable
k.data <- data %>% 
  rename(Nat.LndCov.Ext = Extent.of.natural.land.cover.types,
         Nat.Littor.Zone = Extent.of.natural.littoral.zone,
         Littor.Habitat.Ext = Extent.of.littoral.habitat.features, 
         Littor.Habitat.Div = Diversity.of.littoral.habitat.features,
         Macroph_Cover = Macrophytes_cover,
         Macroph_Invas.Prcnt = Macrophytes_cover_INVASIVE_prcnt) %>% 
  mutate(System =as_factor(System))

str(k.data)

# create other df (for Model 2) with the removed sites with zero macrophytes
df2 <- k.data %>% 
  filter(Macrophytes_SpRich != "0") 
str(df2)


# Predictors:
# Ecosystem - ecosystem type (lake vs pond)
# Nat.LndCov.Ext - Extent of natural land cover types
# Nat.Littor.Zone - Extent of natural littoral zone
# Littor.Habitat.Ext - Extent of littoral habitat features
# Littor.Habitat.Div - Diversity of littoral habitat features
# Macroph_Cover - Macrophyte total cover
# Macroph_Invas.Prcnt - Proportion of invasive species (% out of total cover) 

# Random effect:
# System - studied lake or pond

# Quick relationship check 

ggplot(k.data, aes(Ecosystem, Fish_abundance)) + geom_boxplot() 
ggplot(k.data, aes(Nat.LndCov.Ext, Fish_abundance)) + geom_point()
ggplot(k.data, aes(Nat.Littor.Zone, Fish_abundance)) + geom_point()
ggplot(k.data, aes(Littor.Habitat.Ext, Fish_abundance)) + geom_point()
ggplot(k.data, aes(Littor.Habitat.Div, Fish_abundance)) + geom_point()
ggplot(k.data, aes(Macrophytes_SpRich, Fish_abundance)) + geom_point()
ggplot(k.data, aes(Macroph_Cover, Fish_abundance)) + geom_point()
ggplot(k.data, aes(Macroph_Invas.Prcnt, Fish_abundance)) + geom_point()


# mixed effects models

# Model 1----
# includes all non-correlated predictors

mod1 <-  glmer ( Fish_abundance ~ Ecosystem + Nat.LndCov.Ext + Nat.Littor.Zone + 
                   Littor.Habitat.Ext +Littor.Habitat.Div + 
                   Macroph_Cover +  Macroph_Invas.Prcnt +
                    (1 | System), data = k.data, family = poisson(link = "log"),
                  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))

summary(mod1)

# multicolinearity:
car::vif(mod1)

# residuals:
plot(mod1)
qqnorm(resid(mod1))
qqline(resid(mod1))

# overdispersion:
resid_pearson <- residuals(mod1, type = "pearson")
SSQ <- sum(resid_pearson^2)
SSQ/df.residual(mod1) 
# [1] 3.275376 -  we have clear overdispersion

###--
# using quasipoisson family to account for the data overdispersion

mod1b <- glmmPQL(Fish_abundance ~ Ecosystem + Nat.LndCov.Ext + Nat.Littor.Zone + 
                   Littor.Habitat.Ext +Littor.Habitat.Div + 
                   Macroph_Cover +  Macroph_Invas.Prcnt,
                  random = ~ 1 | System,  
                  data = k.data, 
                  family = quasipoisson(link="log"))

plot(mod1b)
qqnorm(resid(mod1b))
qqline(resid(mod1b))

summary(mod1b)

# check multicolinearity
car::vif(mod1b)

# results:
car::Anova(mod1b)
r.squaredGLMM(mod1b)

write.csv(coef(summary(mod1b)),  file = "Results/Coefs_glmm_abundance_Mod1.csv")
write.csv(Anova(mod1b, type="2"),  file = "Results/Chisq_glmm_abundance_Mod1.csv")


# plot significant effects

## Littor.Habitat.Ext ----

newdat1 <- expand_grid(
  Littor.Habitat.Ext = min(k.data$Littor.Habitat.Ext):max(k.data$Littor.Habitat.Ext),
  Nat.LndCov.Ext = mean(k.data$Nat.LndCov.Ext),
  Nat.Littor.Zone = mean(k.data$Nat.Littor.Zone),
  Littor.Habitat.Div= mean(k.data$Littor.Habitat.Div),
 Macroph_Cover= mean(k.data$Macroph_Cover), 
  Macroph_Invas.Prcnt= mean(k.data$Macroph_Invas.Prcnt),
  Ecosystem = unique(k.data$Ecosystem))


newdat1$fit <- as.vector(predict(mod1b, type = "response",  newdata = newdat1, level=0))

fit_for_Littor.Habitat.Ext <- newdat1 %>% 
  pivot_wider(names_from = Ecosystem, values_from = fit) %>% 
  mutate(fit=(pond+lake)/2)

library(ggplot2)

Fig1a <- ggplot(k.data, aes(Littor.Habitat.Ext, Fish_abundance)) + 
  geom_jitter(aes(fill=Ecosystem), width =0.2, color="black", pch=21, size=3)+
  labs(x ="Extent of littoral habitat", y="Fish abundance") +
  scale_fill_manual(values=c("#66C2A5", "coral"))+
  geom_line(data = fit_for_Littor.Habitat.Ext, 
            aes(y=fit), 
            color = "black", linewidth=1)+
  theme_bw() +
  theme(axis.text.y=element_text(colour = "black", size=13),
        axis.text.x=element_text(colour = "black", size=13),
        axis.title=element_text(size=15),
        axis.line = element_line(colour = "black"),
        axis.ticks =  element_line(colour = "black")) +
  labs(fill='System type')

Fig1a

ggsave("Results/Fig1a.png", width = 10, height = 7, units = "cm")


## Macroph_Cover ----

newdat2 <- expand_grid(
  Macroph_Cover= min(k.data$Macroph_Cover):max(k.data$Macroph_Cover), 
  Littor.Habitat.Ext = mean(k.data$Littor.Habitat.Ext),
  Nat.LndCov.Ext = mean(k.data$Nat.LndCov.Ext),
  Nat.Littor.Zone = mean(k.data$Nat.Littor.Zone),
  Littor.Habitat.Div= mean(k.data$Littor.Habitat.Div),
  Macroph_Invas.Prcnt= mean(k.data$Macroph_Invas.Prcnt),
  Ecosystem = unique(k.data$Ecosystem))


newdat2$fit <- as.vector(predict(mod1b, type = "response",  newdata = newdat2, level=0))

fit_for_Macroph_Cover <- newdat2 %>% 
  pivot_wider(names_from = Ecosystem, values_from = fit) %>% 
  mutate(fit=(pond+lake)/2)

library(ggplot2)

Fig1b <- ggplot(k.data, aes(Macroph_Cover, Fish_abundance)) + 
  geom_jitter(aes(fill=Ecosystem), width =0.1, color="black", pch=21, size=2.5)+
  labs(x ="Macrophyte cover", y="Fish abundance") +
  scale_fill_manual(values=c("#66C2A5", "coral"))+
  geom_line(data = fit_for_Macroph_Cover, 
            aes(y=fit), 
            color = "black", linewidth=1)+
  theme_bw() +
  theme(axis.text.y=element_text(colour = "black", size=13),
        axis.text.x=element_text(colour = "black", size=13),
        axis.title=element_text(size=15),
        axis.line = element_line(colour = "black"),
        axis.ticks =  element_line(colour = "black")) +
  labs(fill='System type')


Fig1b

ggsave("Results/Fig1b.png", width = 10, height = 7, units = "cm")



#########-

# Model 2----
# includes macrophyte groups

# A - emergent broad-leaved herbs
# B - emergent reeds/sedges/rushes
# C - floating-leaved (rooted)
# D - free-floating
# E - submerged fine- and dissected-leaved
# F - submerged broad-leaved
# G - submerged linear-leaved macrophytes
# H - filamentous algae

# Random effect:
# System - studied lake or pond

# Quick relationship check 
ggplot(k.data, aes(A, Fish_abundance)) + geom_point()
ggplot(k.data, aes(B, Fish_abundance)) + geom_point()
ggplot(k.data, aes(C, Fish_abundance)) + geom_point()
ggplot(k.data, aes(D, Fish_abundance)) + geom_point()
ggplot(k.data, aes(E, Fish_abundance)) + geom_point()
ggplot(k.data, aes(F, Fish_abundance)) + geom_point()
ggplot(k.data, aes(G, Fish_abundance)) + geom_point()
ggplot(k.data, aes(H, Fish_abundance)) + geom_point()


# mixed effects models

mod2 = glmer (Fish_abundance ~ A  + B + C  + D  + E  + F  + G  + H +
                    (1 | System), data = df2, family = poisson(link = "log"),
                  control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))

summary(mod2)

# multicolinearity:
car::vif(mod2)

# residuals:
plot(mod2)
qqnorm(resid(mod2))
qqline(resid(mod2))

# overdispersion:
resid_pearson <- residuals(mod2, type = "pearson")
SSQ <- sum(resid_pearson^2)
SSQ/df.residual(mod2) 
# we have clear overdispersion

###--
# using quasipoisson family to account for the data overdispersion

library(MASS)

mod2b <- glmmPQL(Fish_abundance ~ A  + B + C  + D  + E  + F  + G  + H,
                 random = ~ 1 | System, data = df2, 
                 family = "quasipoisson") 

plot(mod2b)
qqnorm(resid(mod2b))
qqline(resid(mod2b))

summary(mod2b)

# multicolinearity:
car::vif(mod2b)

# results:
car::Anova(mod2b)
r.squaredGLMM(mod2b)

write.csv(coef(summary(mod2b)),  file = "Results/Coefs_glmm_abundance_Mod2.csv")
write.csv(Anova(mod2b, type="2"),  file = "Results/Chisq_glmm_abundance_Mod2.csv")

# Plot significant predictor:
## E ----

newdat3 <- expand_grid(
  E=seq(from = min(k.data$E),to=max(k.data$E), by=0.01),
  A = mean(k.data$A),
  B = mean(k.data$B),
  C = mean(k.data$C),
  D= mean(k.data$D),
  F= mean(k.data$F),
  G= mean(k.data$G),
  H= mean(k.data$H))


newdat3$fit <- as.vector(predict(mod2b, type = "response",  newdata = newdat3, level=0))


library(ggplot2)

Fig1e <- ggplot(k.data, aes(E, Fish_abundance)) + 
  geom_jitter(aes(fill=Ecosystem), height=0.5, width =0.5, color="black", pch=21, size=2.5)+
  labs(x ="Submerged fine and 
dissected macrophytes", y="Fish abundance") +
  scale_fill_manual(values=c("#66C2A5", "coral"))+
  geom_line(data = newdat3, 
            aes(y=fit), 
            color = "black", linewidth=1)+
  theme_bw() +
  theme(axis.text.y=element_text(colour = "black", size=13),
        axis.text.x=element_text(colour = "black", size=13),
        axis.title=element_text(size=15),
        axis.line = element_line(colour = "black"),
        axis.ticks =  element_line(colour = "black")) +
  labs(fill='System type')

Fig1e

ggsave("Results/Fig1e.png", width = 10, height = 7, units = "cm")



# End ----
