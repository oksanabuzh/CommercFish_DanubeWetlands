# Drivers of commercial fishes in Wetlands

# response "% of omnivore fishes out of the total fish abundance"

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
  mutate(OMN_prcnt = OMN/ Fish_abundance) %>%     # calculate proportion of carnivores out of total fish abundance
  mutate(OMN_prcnt = replace_na(OMN_prcnt, 0))

k.data$OMN_prcnt

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

ggplot(k.data, aes(Ecosystem, OMN_prcnt)) + geom_boxplot() 
ggplot(k.data, aes(Nat.LndCov.Ext, OMN_prcnt)) + geom_point()
ggplot(k.data, aes(Nat.Littor.Zone, OMN_prcnt)) + geom_point()
ggplot(k.data, aes(Littor.Habitat.Ext, OMN_prcnt)) + geom_point()
ggplot(k.data, aes(Littor.Habitat.Div, OMN_prcnt)) + geom_point()
ggplot(k.data, aes(Macroph_Cover, OMN_prcnt)) + geom_point()
ggplot(k.data, aes(Macroph_Invas.Prcnt, OMN_prcnt)) + geom_point()


# mixed effects models

# Model 1----
# includes all non-correlated predictors

mod1 <-  glmer ( OMN_prcnt ~ Ecosystem + Nat.LndCov.Ext + Nat.Littor.Zone + 
                   Littor.Habitat.Ext + Littor.Habitat.Div + 
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
# [1] 1.106272 - no overdispersion


# results:
car::Anova(mod1)
r.squaredGLMM(mod1)

write.csv(coef(summary(mod1)),  file = "Results/Coefs_glmm_OMN_Mod1.csv")
write.csv(Anova(mod1, type="2"),  file = "Results/Chisq_glmm_OMN_Mod1.csv")

# plot significant effects ---

# Littor.Habitat.Div ----

newd <- expand_grid(
  Littor.Habitat.Div= (0.8*55):(3.2*55),
  Littor.Habitat.Ext = mean(k.data$Littor.Habitat.Ext),
  Nat.LndCov.Ext = mean(k.data$Nat.LndCov.Ext),
  Nat.Littor.Zone = mean(k.data$Nat.Littor.Zone),
  Macroph_Cover= mean(k.data$Macroph_Cover), 
  Macroph_Invas.Prcnt= mean(k.data$Macroph_Invas.Prcnt),
  Ecosystem = unique(k.data$Ecosystem))

newdat1 <- newd %>% 
  mutate(Littor.Habitat.Div=Littor.Habitat.Div/55)

newdat1$fit <- (predict(mod1, type = "response",  newdata = newdat1, re.form = NA))

fit_for_Littor.Habitat.Div <- newdat1 %>% 
  pivot_wider(names_from = Ecosystem, values_from = fit) %>% 
  mutate(fit=(pond+lake)/2)

library(ggplot2)

ggplot(k.data, aes(Littor.Habitat.Div, OMN_prcnt)) + 
  geom_jitter(aes(fill=Ecosystem), width =0.25, fill="blue", color="black", pch=21, size=3)+
  labs(x ="Diversity of littoral habitat", y="Proportion of omnivores") +
  # scale_fill_manual(values=c("#66C2A5", "coral"))+
  geom_line(data = fit_for_Littor.Habitat.Div, 
            aes(y=fit), 
            color = "black", linewidth=1)+
  theme_bw() +
  theme(axis.text.y=element_text(colour = "black", size=13),
        axis.text.x=element_text(colour = "black", size=13),
        axis.title=element_text(size=15),
        axis.line = element_line(colour = "black"),
        axis.ticks =  element_line(colour = "black")) +
  labs(fill='System type')


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
ggplot(k.data, aes(A, OMN_prcnt)) + geom_point()
ggplot(k.data, aes(B, OMN_prcnt)) + geom_point()
ggplot(k.data, aes(C, OMN_prcnt)) + geom_point()
ggplot(k.data, aes(D, OMN_prcnt)) + geom_point()
ggplot(k.data, aes(E, OMN_prcnt)) + geom_point()
ggplot(k.data, aes(F, OMN_prcnt)) + geom_point()
ggplot(k.data, aes(G, OMN_prcnt)) + geom_point()
ggplot(k.data, aes(H, OMN_prcnt)) + geom_point()


# mixed effects models

mod2 = glmer (OMN_prcnt ~ A  + B + C  + D  + E  + F  + G  + H +
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

write.csv(coef(summary(mod2)),  file = "Results/Coefs_glmm_OMN_Mod2.csv")
write.csv(Anova(mod2, type="2"),  file = "Results/Chisq_glmm_OMN_Mod2.csv")

# End