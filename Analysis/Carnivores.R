# Drivers of commercial fishes in Wetlands in Serbia 

# response "% of carnivore fishes out of total fish abundance"

rm(list=ls(all=TRUE))

# Packages----


library(tidyverse)
library(ggplot2)
library(lme4)
library(MuMIn)
library(MASS)
library(car)

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
  mutate(System = as_factor(System)) %>% 
  mutate(CAR_prcnt = CAR/ Fish_abundance) %>%     # calculate proportion of carnivores out of total fish abundance
  mutate(CAR_prcnt = replace_na(CAR_prcnt, 0))

k.data$CAR_prcnt

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

ggplot(k.data, aes(Ecosystem, CAR_prcnt)) + geom_boxplot() 
ggplot(k.data, aes(Nat.LndCov.Ext, CAR_prcnt)) + geom_point()
ggplot(k.data, aes(Nat.Littor.Zone, CAR_prcnt)) + geom_point()
ggplot(k.data, aes(Littor.Habitat.Ext, CAR_prcnt)) + geom_point()
ggplot(k.data, aes(Littor.Habitat.Div, CAR_prcnt)) + geom_point()
ggplot(k.data, aes(Macroph_Cover, CAR_prcnt)) + geom_point()
ggplot(k.data, aes(Macroph_Invas.Prcnt, CAR_prcnt)) + geom_point()


# mixed effects models

# Model 1----
# includes all non-correlated predictors

mod1 <-  glmer ( CAR_prcnt ~ Ecosystem + Nat.LndCov.Ext + Nat.Littor.Zone + 
                   Littor.Habitat.Ext +Littor.Habitat.Div + 
                   Macroph_Cover +  Macroph_Invas.Prcnt +
                    (1 | System), data = k.data,
                  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)),
                  family = "binomial", weights = Fish_abundance)

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
# no overdispersion


# results:
car::Anova(mod1)
r.squaredGLMM(mod1)

write.csv(coef(summary(mod1)),  file = "Results/Coefs_glmm_Car_Mod1.csv")
write.csv(Anova(mod1, type="2"),  file = "Results/Chisq_glmm_Car_Mod1.csv")


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
ggplot(k.data, aes(A, CAR_prcnt)) + geom_point()
ggplot(k.data, aes(B, CAR_prcnt)) + geom_point()
ggplot(k.data, aes(C, CAR_prcnt)) + geom_point()
ggplot(k.data, aes(D, CAR_prcnt)) + geom_point()
ggplot(k.data, aes(E, CAR_prcnt)) + geom_point()
ggplot(k.data, aes(F, CAR_prcnt)) + geom_point()
ggplot(k.data, aes(G, CAR_prcnt)) + geom_point()
ggplot(k.data, aes(H, CAR_prcnt)) + geom_point()


# mixed effects models

mod2 = glmer (CAR_prcnt ~ A  + B + C  + D  + E  + F  + G  + H +
                    (1|System), data = df2, 
             control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)),
              family = "binomial", weights = Fish_abundance)

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
#  no overdispersion

# results:
car::Anova(mod2)
#R2
r.squaredGLMM(mod2)
piecewiseSEM::rsquared(mod2)

write.csv(coef(summary(mod2)),  file = "Results/Coefs_glmm_CAR_Mod2.csv")
write.csv(Anova(mod2, type="2"),  file = "Results/Chisq_glmm_CAR_Mod2.csv")

# End