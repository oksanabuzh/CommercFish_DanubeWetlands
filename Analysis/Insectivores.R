# Drivers of commercial fishes in Wetlands

# response "% of insectivore fishes out of the total fish abundance"

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
  mutate(INS_prcnt = INS/ Fish_abundance) %>%     # calculate proportion of carnivores out of total fish abundance
  mutate(INS_prcnt = replace_na(INS_prcnt, 0))

k.data$INS_prcnt
k.data$INS
k.data$Fish_abundance

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

ggplot(k.data, aes(Ecosystem, INS_prcnt)) + geom_boxplot() 
ggplot(k.data, aes(Nat.LndCov.Ext, INS_prcnt)) + geom_point()
ggplot(k.data, aes(Nat.Littor.Zone, INS_prcnt)) + geom_point()
ggplot(k.data, aes(Littor.Habitat.Ext, INS_prcnt)) + geom_point()
ggplot(k.data, aes(Littor.Habitat.Div, INS_prcnt)) + geom_point()
ggplot(k.data, aes(Macroph_Cover, INS_prcnt)) + geom_point()
ggplot(k.data, aes(Macroph_Invas.Prcnt, INS_prcnt)) + geom_point()


# mixed effects models

# Model 1----
# includes all non-correlated predictors

mod1 <-  glmer ( INS_prcnt ~  
                    # Ecosystem + 
                      Nat.LndCov.Ext + # Nat.Littor.Zone + 
                   #  Littor.Habitat.Ext + 
                    Littor.Habitat.Div + 
                    Macroph_Cover +  Macroph_Invas.Prcnt  +
                    (1 | System), data = k.data,
                  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=10000)),
                  family = "binomial", weights = Fish_abundance)

summary(mod1)

# multicolinearity:
car::vif(mod1)

# results
Anova(mod1)
r.squaredGLMM(mod1)


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

write.csv(coef(summary(mod1)),  file = "Results/Coefs_glmm_INS_Mod1.csv")
write.csv(Anova(mod1, type="2"),  file = "Results/Chisq_glmm_INS_Mod1.csv")



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
ggplot(k.data, aes(A, INS_prcnt)) + geom_point()
ggplot(k.data, aes(B, INS_prcnt)) + geom_point()
ggplot(k.data, aes(C, INS_prcnt)) + geom_point()
ggplot(k.data, aes(D, INS_prcnt)) + geom_point()
ggplot(k.data, aes(E, INS_prcnt)) + geom_point()
ggplot(k.data, aes(F, INS_prcnt)) + geom_point()
ggplot(k.data, aes(G, INS_prcnt)) + geom_point()
ggplot(k.data, aes(H, INS_prcnt)) + geom_point()


# mixed effects models

mod2 = glmer (INS_prcnt ~ A  + B + C  + D  + E  + F  + G  + H +
                    (1|System), data = k.data, 
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


####
write.csv(coef(summary(mod2)),  file = "Results/Coefs_glmm_INS_Mod2.csv")
write.csv(Anova(mod2, type="2"),  file = "Results/Chisq_glmm_INS_Mod2.csv")



# Plot significant predictor:
## E ----

newd <- expand_grid(
  D= (-0.1*55):max(k.data$D*55), 
  A = mean(k.data$A),
  B = mean(k.data$B),
  C = mean(k.data$C),
  E= mean(k.data$E),
  F= mean(k.data$F),
  G= mean(k.data$G),
  H= mean(k.data$H))

newdat3 <- newd %>% 
  mutate(D=D/55)


newdat3$fit <- predict(mod2, type = "response",  newdata = newdat3, re.form = NA)


library(ggplot2)

ggplot(k.data, aes(D, INS_prcnt)) + 
  geom_jitter(aes(fill=Ecosystem), height=0.1, width =0.3, fill="#FFAA19", color="black", pch=21, size=3)+
  labs(x ="Free-floating macrophytes", y="Proportion of insectivores") +
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


# End