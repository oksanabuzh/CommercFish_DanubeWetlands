
# RDA analysis

rm(list=ls(all=TRUE))


Index <-read.csv( "Data/comer_fish.csv")

names(Index)

Macr_sp_composition <- read.csv ( "Data/Macr_sp_comp.csv", header=T)
names(Macr_sp_composition)

library(vegan)

Index2 <- Index [-c(6, 7, 36, 37, 39, 40, 41, 42, 53, 13, 14, 17, 18), ] # remove zeros fish species and zeros macrophytes

fish_sp_comp2 <-(Index2[,43:51])# community matrix based on fish abundance 
names(fish_sp_comp2)



Macr_sp_comp <- Macr_sp_composition [-c(6, 7, 36, 37, 39, 40, 41, 42, 53, 13, 14, 17, 18), ] # remove zeros fish species and zeros macrophytes

# presence-absance transformation to calculate species number per cite
Macr_sp_comp_pa <- decostand(Macr_sp_comp, 'pa')

#calculate sum per species
Macr_sp_sum <- apply (Macr_sp_comp_pa,2,sum)
sort(Macr_sp_sum)

#removal of rare species
#remove species that occure at less than 2 sites
Macr.sp.com <- Macr_sp_comp [, Macr_sp_sum>0.9 ]
names(Macr.sp.com)
decorana(Macr.sp.com)


# checking gradient lengths with 
Macr.sp.com_hel<- decostand(Macr.sp.com, "hellinger")
decorana(Macr.sp.com_hel)


# Since we did not standardise data, we should scale data
#This implies that changes in species abundances obtain equal considaration
#if we use hellinger transformation we do not need to scale data
model <- rda(fish_sp_comp2 ~ .,  data=Macr.sp.com, scale=TRUE)
model
summary(model)

#Global tests of the RDA results
set.seed(111)
anova.cca (model, step=1000)
#Test of all canonical axes
set.seed(111)
anova.cca (model, by ="axis")
scores(model)

x11(height=5.5,width=6.5)
plot(model,  scaling=1, display = "species", type="n")
text(model, display="bp", col='blue')
text(model, display="species", col='red', cex=0.8)


#  Forvard selection using adj.R2 as goodness of fit meausure
rdaplotpR2 <- ordiR2step (rda(fish_sp_comp2 ~ 1, data=Macr.sp.com), scope = formula(model), 
                                direction="both", pstep=1000)
rdaplotpR2
summary(rdaplotpR2)

#Global tests of the RDA results
set.seed(111)
anova.cca (rdaplotpR2, step=1000)
x11(height=5,width=5)
p_sign <- rda(fish_sp_comp2 ~  s27 + s8 + s17 + s29 + s15 + s9 + s30 + s16 + s34 + s43+
                              s19 + s41 ,
                               data = Macr.sp.com, scale=TRUE)

summary(p_sign)
set.seed(11111)
anova.cca (p_sign)
anova(p_sign)

x11(height=5,width=5)
plot(p_sign,  scaling=2, type="n")
text(p_sign, display="bp", col='blue')
text(p_sign, display="species",  col='red', cex=0.8)

scores(p_sign)$species
scores(p_sign)$sites


# ggplot
data.scores = as.data.frame(scores(p_sign)$sites)

species.scores <- as.data.frame(scores(p_sign)$species)  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores$species <- rownames(species.scores)  # create a column of species, from the rownames of species.scores
head(species.scores)

ordiArrowMul (p_sign)
scores(fit1, "vectors")
scor = scores(p_sign, display=c("sp", "cn", "bp"), scaling=2)

basplot <- plot(p_sign)
mult <- attributes(basplot$biplot)$arrow.mul

ordiArrowMul(fit_100)
# en_coord_cat <- as.data.frame(scores(fit5, "factors")) * ordiArrowMul(fit5)
# en_coord_cont = as.data.frame(scores(fit1, "vectors")) * ordiArrowMul(fit1)
en_coord_cont<- as.data.frame(scor$biplot)   * mult
en_coord_cont
en_coord_cont$species <- rownames(en_coord_cont) 

manure_continuous_arrows <- data.frame(scor$biplot)
manure_continuous_arrows
manure_continuous_arrows$manure_class <- rownames(manure_continuous_arrows) #turning rownames into a variable
manure_continuous_arrows

en_coord_cont[c(1),]


library(ggplot2)

library(ggrepel)
Col=c("red", "red", "blue", "orange1","red", "blue","red","red","orange1")
#      CAR	     CAR	    OMN	     INS	    CAR	    OMN	  CAR	  CAR	   INS

Col2=c("green2", "green2", "magenta", "green2","green2","magenta","green2","green2","green2","green2","green2","green2")
#        s27 +     s8 +      s17 +      s29 +    s15 +     s9 +     s30 +    s16 +    s34 +    s43+     s19 +     s41   
#invasive are s9, s17, and s32

x11(height=5,width=5)
ggplot(data = data.scores, aes(x = RDA1, y = RDA1)) +
  
  geom_text_repel (data=species.scores,aes(x=RDA1,y=RDA2,label=species), size = 5, colour= Col, position=position_jitter(h=0.2, w=0.2)) +
  # geom_point (data =data.scores,  size = 3, alpha = 0.2, position=position_jitter(h=0.2, w=0.2)) + 
  # scale_shape_manual(values = c(19, 1))  +
  #scale_colour_manual(values = c("springgreen4", "magenta"))  +
  geom_point (data = species.scores, aes(x=RDA1,y=RDA2), size = 3, pch=8, 
              colour=Col, fill=Col) + 
  
  #macrophyte species , 
  geom_segment(aes(x = 0, xend =RDA1,
                   y = 0, yend =  RDA2), 
               data = en_coord_cont, size =0.7, alpha = 1, colour = Col2)+ 
  # geom_text(data = en_coord_cont, aes(x = RDA1, y = RDA2,label=species), 
                #colour = Col2,  fontface = "bold", size = 5) + 
  
    theme(axis.title = element_text(size = 15, face = "bold", colour = "black"), 
        panel.background = element_blank(), panel.border = element_rect(fill = NA, colour = "grey30", size=0.5), 
        # axis.ticks = element_blank(), 
        axis.text = element_text(size = 13, colour = "black"), 
        legend.key = element_blank(), 
        legend.title = element_text(size = 14, face = "bold", colour = "black"), 
        legend.text = element_text(size = 14, colour = "black"),
        legend.position="bottom") +
 
  #ad text manuallz for each macrophzte species s
   geom_text(data = en_coord_cont[c(1),], aes(x = RDA1, y = RDA2,label=species), 
            colour = "green2", 
            # fontface = "bold",  
            size = 5) +
  geom_text(data = en_coord_cont[c(2),], aes(x = RDA1, y = RDA2,label=species), 
          colour = "green2", # fontface = "bold",  
          size = 5)+
geom_text(data = en_coord_cont[c(3),], aes(x = RDA1, y = RDA2,label=species), 
          colour = "magenta", 
          # fontface = "bold",  
          size = 5,
          vjust=0.3, hjust=-0.1) +
  geom_text(data = en_coord_cont[c(4),], aes(x = RDA1, y = RDA2,label=species), 
            colour = "green2", 
            # fontface = "bold",  
            size = 5) +
  geom_text(data = en_coord_cont[c(5),], aes(x = RDA1, y = RDA2,label=species), 
            colour = "green2", 
            # fontface = "bold",  
            size = 5,
            vjust=0.6, hjust=2) +
  geom_text(data = en_coord_cont[c(6),], aes(x = RDA1, y = RDA2,label=species), 
            colour = "magenta", 
            # fontface = "bold",  
            size = 5,
            vjust=1, hjust=0.9) +
  geom_text(data = en_coord_cont[c(7),], aes(x = RDA1, y = RDA2,label=species), 
            colour = "green2",# fontface = "bold", 
            size = 5,
            vjust=0, hjust=-0.1) +
geom_text(data = en_coord_cont[c(8),], aes(x = RDA1, y = RDA2,label=species), 
          colour = "green2", 
          # fontface = "bold",  
          size = 5) +  
  geom_text(data = en_coord_cont[c(9),], aes(x = RDA1, y = RDA2,label=species), 
                                 colour = "green2", 
                                 # fontface = "bold",  
                                 size = 5) +
  geom_text(data = en_coord_cont[c(10),], aes(x = RDA1, y = RDA2,label=species), 
            colour = "green2", 
            # fontface = "bold",  
            size = 5) +
  geom_text(data = en_coord_cont[c(11),], aes(x = RDA1, y = RDA2,label=species), 
            colour = "green2", 
            # fontface = "bold",  
            size = 5) +
  geom_text(data = en_coord_cont[c(12),], aes(x = RDA1, y = RDA2,label=species), 
            colour = "green2", 
            # fontface = "bold",  
            size = 5,
            vjust=0.3, hjust=-0.1) 

