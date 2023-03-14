# Drivers of commercial fishes in Wetlands 

# response "Fish species richness"

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

ggplot(k.data, aes(Ecosystem, Fish_SpRich)) + geom_boxplot() 
ggplot(k.data, aes(Nat.LndCov.Ext, Fish_SpRich)) + geom_point()
ggplot(k.data, aes(Nat.Littor.Zone, Fish_SpRich)) + geom_point()
ggplot(k.data, aes(Littor.Habitat.Ext, Fish_SpRich)) + geom_point()
ggplot(k.data, aes(Littor.Habitat.Div, Fish_SpRich)) + geom_point()
ggplot(k.data, aes(Macrophytes_SpRich, Fish_SpRich)) + geom_point()
ggplot(k.data, aes(Macroph_Cover, Fish_SpRich)) + geom_point()
ggplot(k.data, aes(sqrt(Macroph_Invas.Prcnt), Fish_SpRich)) + geom_jitter(width=0.3)


# mixed effects models

# Model 1----
# includes all non-correlated predictors

mod1 <-  glmer (Fish_SpRich ~ 
                  Fish_abundance + 
                  Ecosystem + 
                  Nat.LndCov.Ext + Nat.Littor.Zone + 
                  Littor.Habitat.Ext + Littor.Habitat.Div + 
                  Macroph_Cover +  
                  Macroph_Invas.Prcnt +
                    (1 | System), data = k.data, family = poisson(link = "log"),
                control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))

summary(mod1)

# residuals:
plot(mod1)
qqnorm(resid(mod1))
qqline(resid(mod1))
# overdispersion:
resid_pearson <- residuals(mod1, type = "pearson")
SSQ <- sum(resid_pearson^2)
SSQ/df.residual(mod1) 

# when fitted model we got WARNING: 
# boundary (singular) fit: see help('isSingular')

# check multicolinearity in fixed effects:
car::vif(mod1)
# no correlated fixed effects

# check random effect:
# Model 1 did fit, but it generated warning of boundary (singular) fit because our random effects might be very small.
# plot effect of "System"
ggplot(k.data,aes(x=System,y=Fish_SpRich,col=System)) + geom_jitter() + 
  geom_boxplot(alpha=0.2) 
# we see some effect of "System" exists, but this is when we did not account for the other covariates

# look at our fitted model with the covariates:
ranef(mod1)
# we see very small coefficients (all are 0), meaning that there's no really systematic effect coming from our random effect "System"
r.squaredGLMM(mod1)
# R2c is same as R2m, meaning that random effect expalins 0 variance in our responce variable

# we can also test random effect:
# specify model without random effect

mod1b <-  glm (Fish_SpRich ~ 
                  Fish_abundance + 
                  Ecosystem + 
                  Nat.LndCov.Ext + Nat.Littor.Zone + 
                  Littor.Habitat.Ext + Littor.Habitat.Div + 
                  Macroph_Cover +  
                  Macroph_Invas.Prcnt , data = k.data, family = poisson(link = "log"))

# compare two models
AIC(mod1, mod1b)
# we see that models do not substantially differ (i.e., delta AIC = 2), thus we can go for more parsinomous model and drop the random effect 

summary(mod1b)

# overdispersion:
with(summary(mod1b), deviance/df[2])
42.745/44
# dispersion is Ok

# residuals:
par(mfrow=c(2,2))
plot(mod1b)
par(mfrow=c(1,1))

# results:
car::Anova(mod1b)
#R2 (pseudo r-squared)
with(summary(mod1b), 1 - deviance/null.deviance)


write.csv(coef(summary(mod1b)),  file = "Results/Coefs_glm_SR_Mod1.csv")
write.csv(Anova(mod1b, type="2"),  file = "Results/Chisq_glm_SR_Mod1.csv")



## Macroph_Invas.Prcnt ----

newdat1 <- expand_grid(
  Macroph_Invas.Prcnt= min(k.data$Macroph_Invas.Prcnt):max(k.data$Macroph_Invas.Prcnt),
  Fish_abundance  = mean(k.data$Fish_abundance ),
  Littor.Habitat.Ext = mean(k.data$Littor.Habitat.Ext),
  Nat.LndCov.Ext = mean(k.data$Nat.LndCov.Ext),
  Nat.Littor.Zone = mean(k.data$Nat.Littor.Zone),
  Littor.Habitat.Div= mean(k.data$Littor.Habitat.Div),
  Macroph_Cover= mean(k.data$Macroph_Cover), 
  Ecosystem = unique(k.data$Ecosystem))


newdat1$fit <- predict(mod1b, type = "response",  newdata = newdat1)

fit_Macroph_Invas.Prcnt <- newdat1 %>% 
  pivot_wider(names_from = Ecosystem, values_from = fit) %>% 
  mutate(fit=(pond+lake)/2)

library(ggplot2)

ggplot(k.data, aes(Macroph_Invas.Prcnt, Fish_SpRich)) + 
  geom_jitter(aes(fill=Ecosystem), width =1, color="black", pch=21, size=3)+
  labs(x ="Invasive macrophytes, %", y="Fish species richness") +
  scale_fill_manual(values=c("#66C2A5", "coral"))+
  geom_line(data = fit_Macroph_Invas.Prcnt, 
            aes(y=fit), 
            color = "black", linewidth=1)+
  theme_bw() +
  theme(axis.text.y=element_text(colour = "black", size=13),
        axis.text.x=element_text(colour = "black", size=13),
        axis.title=element_text(size=15),
        axis.line = element_line(colour = "black"),
        axis.ticks =  element_line(colour = "black")) +
  labs(fill='System type')

####-
# Ecosystem type----

Data_EcosType <- k.data %>% # the names of the new data frame and the data frame to be summarised
  group_by(Ecosystem) %>%   # the grouping variable
  summarise(mean_PL = mean(Fish_SpRich),  # calculates the mean of each group
            sd_PL = sd(Fish_SpRich), # calculates the standard deviation of each group
            n_PL = n(),  # calculates the sample size per group
            SE_PL = sd(Fish_SpRich)/sqrt(n())) 
Data_EcosType

ggplot(Data_EcosType) +
  geom_bar( aes(x=Ecosystem, y=mean_PL, fill=Ecosystem), stat="identity", size=0.3, color="black") +
  geom_errorbar(aes(x=Ecosystem,ymin = mean_PL - SE_PL, ymax = mean_PL + SE_PL), width=0.2)+
 scale_fill_manual(values=c("#66C2A5", "coral"))+
  labs(x ="System type", y="Fish species richness") +
  theme_bw()+
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
ggplot(k.data, aes(A, Fish_SpRich)) + geom_point()
ggplot(k.data, aes(B, Fish_SpRich)) + geom_point()
ggplot(k.data, aes(C, Fish_SpRich)) + geom_point()
ggplot(k.data, aes(D, Fish_SpRich)) + geom_point()
ggplot(k.data, aes(E, Fish_SpRich)) + geom_point()
ggplot(k.data, aes(F, Fish_SpRich)) + geom_point()
ggplot(k.data, aes(G, Fish_SpRich)) + geom_point()
ggplot(k.data, aes(H, Fish_SpRich)) + geom_point()


# mixed effects models

mod2 = glmer(Fish_SpRich ~  Fish_abundance + 
               A  + B + C  + D  + E  + (F ) + G  + H + 
               (1 | System), data = df2, family = poisson(link = "log"),
             control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))

summary(mod2)

# residuals:
plot(mod2)
qqnorm(resid(mod2))
qqline(resid(mod2))
# overdispersion:
resid_pearson <- residuals(mod2, type = "pearson")
SSQ <- sum(resid_pearson^2)
SSQ/df.residual(mod2) 

# we got WARNING: boundary (singular) fit: see help('isSingular')
help('isSingular')

summary(mod2)

# check multicolinearity in fixed effects:
car::vif(mod2)
# no correlated fixed effects

# check random effect:
# Model 1 did fit, but it generated warning of boundary (singular) fit because our random effects might be very small.
# look at our fitted model with the covariates:
ranef(mod2)
# we see very small coefficients (all are 0), meaning that there's no really systematic effect coming from our random effect "System"

# we can also test random effect:
# specify model without random effect

mod2b <-  glm (Fish_SpRich ~  Fish_abundance + 
                 A  + B + C  + D  + E  + F  + G  + H , 
               data = df2, family = poisson(link = "log"))

# compare two models
AIC(mod2, mod2b)
# we see that models do not substantially differ thus we can go for more parsinomous model and drop the random effect 

summary(mod2b)

# overdispersion:
with(summary(mod2b), deviance/df[2])
37.989/39  
# dispersion is Ok

# residuals:
par(mfrow=c(2,2))
plot(mod2b)
par(mfrow=c(1,1))

# results:
car::Anova(mod2b)
#R2 (pseudo r-squared)
with(summary(mod2b), 1 - deviance/null.deviance)

write.csv(coef(summary(mod2b)),  file = "Results/Coefs_glm_SR_Mod2.csv")
write.csv(Anova(mod2b, type="2"),  file = "Results/Chisq_glm_SR_Mod2.csv")


# End