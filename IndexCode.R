
library("vegan")
data(BCI)
View(BCI)
diversity(BCI, index = "shannon")
fisher.alpha(BCI)
spa <- specaccum(BCI) #species richness
plot(spa)
mod <- decorana(BCI) #ordination
plot(mod)

setwd("C:/Users/Usuario/Documents/R/Moths_QuantReas")

# Calculating diversity by night (N=120) - DOES NOT WORK

G_matrix <- read.csv('Geo_full_matrix.csv')
G_fisher <- fisher.alpha(G_matrix) #Error: returns really high numbers for some rows
G_fisher <- as.data.frame(G_fisher)

A_matrix <- read.csv('Arc_full_matrix.csv')
A_fisher <- fisher.alpha(A_matrix) #Error: doesn't run even thought its the same code as for geometrids
A_fisher <- as.data.frame(A_fisher)

# Geo_Div <- table(Geom_fisher)
# View(Geo_Div)

covar <- read.csv('covariables.csv')

#Make 6 copies per row of covariable data, to extend dataframe from 20 to 120 rows.
covar_long <- covar
for (i in 1:5){
  covar_long <- rbind(covar_long, covar)
}

# covar_long$G_div <- Geom_fisher$V1

# t1 <- merge(Geom_fisher,nlcd08,by=c("COUNTY","LANDCOVER"),all.x=TRUE)

#export data to csv
write.table(covar_long, file="covar_long.csv",sep=",",row.names=F)
write.table(G_fisher, file="G_div.csv",sep=",",row.names=F)

# Calculating moth diversity of each site (N=20)

G_matrix <- read.csv('Geo_site_matrix.csv')
G_fisher <- fisher.alpha(G_matrix) 
G_fisher <- as.data.frame(G_fisher)

A_matrix <- read.csv('Arc_site_matrix.csv')
A_fisher <- fisher.alpha(A_matrix) #Error: doesn't run even thought its the same code as for geometrids
A_fisher <- as.data.frame(A_fisher)

#export data to csv
write.table(G_fisher, file="G_div.csv",sep=",",row.names=F)
write.table(A_fisher, file="A_div.csv",sep=",",row.names=F)

#NMDS for plants
plants <- read.csv("Plant_matrix.csv")
sites <- read.csv("SiteCode.csv")
habitat <- read.csv("Habitat.csv")

install.packages("MASS")
library("MASS")

PlantsOrd <- metaMDS(plants,distance = "bray", k = 3,trymax=100)
PlantsOrd
plot(PlantsOrd)

cols <- c("red", "blue", "pink", "green")
plot(PlantsOrd, type = "n")
points(PlantsOrd, col = cols[habitat$Habitat], pch = 16)
legend("topright", legend=levels(habitat$Habitat), col=cols, pch = 16)

cols <- c("red", "blue", "pink", "green")
plot(PlantsOrd, type = "n")
points(PlantsOrd, col = cols[sites$Site.Code], pch = 16)
legend("topright", legend=levels(sites$Site.Code), col=cols, pch = 16)

stressplot(PlantsOrd)
################## ggplot ###################

install.packages("ggplot2")
install.packages("Rcpp")
library("ggplot2")
require("ggplot2")

set.seed(201)
PlantsOrd1 <- metaMDS(plants)
plot(PlantsOrd1$points, col = sites$Site.Code)
ordiellipse(PlantsOrd1, sites$Site.Code, display = "sites", kind = "sd", label = T)
NMDS = data.frame(MDS1 = PlantsOrd1$points[,1], MDS2 = PlantsOrd1$points[,2],group=sites$Site.Code)

#Extract NMDS axis scores
nms_axis <- scores(PlantsOrd, choices=c(1,2))
write.table(nms_axis, file="Plant_nms_axis.csv",sep=",",row.names=F)

###
covar_moth
plot(Code~Geometrid_FisherIndex~VegDiversity, data = covar_moth)
abline(lm(Geometrid_FisherIndex~CanopyCover, data = covar_moth))

####### testing for spatial autocorrelation ##########
#method taken from: https://stats.idre.ucla.edu/r/faq/how-do-i-fit-a-variogram-model-to-my-spatial-data-in-r-using-regression-commands/

install.packages("nlme")
library(nlme)
require(nlme)

covar_moth <- read.csv("covar_moth.csv")
dummy <- rep(1, 20)
spdata <- cbind(covar_moth, dummy)
null.model <- lme(fixed = Arctiine_FisherIndex ~ 1, data = covar_moth, random = ~ 1 | dummy)
summary(null.model)

cs1Exp <- corExp(1, form = ~ Lat + Long)
cs1Exp <- Initialize(cs1Exp, covar_moth)
corMatrix(cs1Exp)

exp.sp <- update(null.model, correlation = corExp(1, form = ~ Lat + Long), method = "ML")
summary(exp.sp)



##############################################################################################

install.packages("lme4")
library(lme4)
  
##Arctiid Diversity (Fisher's alpha) Structural Model
Arctiid.structural <- as.formula(Arctiine_FisherIndex ~ UnderComplex + CanopyCover + VerticalComplex + (1|Moonlight) + (1|Habitat))
Arctiid.structural.model <- lmer(Arctiid.structural, data=covar_moth)
summary(Arctiid.structural.model)

##Geometrid Diversity (Fisher's alpha) Structural Model
Geometrid.structural <- as.formula(Geometrid_FisherIndex ~ UnderComplex + CanopyCover + VerticalComplex + (1|Moonlight) + (1|Habitat))
Geometrid.structural.model <- lmer(Geometrid.structural, data=covar_moth)
summary(Geometrid.structural.model)

##Arctiid Diversity (Fisher's alpha) Floristic Model
Arctiid.floristic <- as.formula(Arctiine_FisherIndex ~ VegDiversity + VegDensity + NMDS1 + NMDS2 + (1|Moonlight) + (1|Habitat))
Arctiid.floristic.model <- lmer(Arctiid.floristic, data=covar_moth)
summary(Arctiid.floristic.model)

##Geometrid Diversity (Fisher's alpha) Floristic Model
Geometrid.floristic <- as.formula(Geometrid_FisherIndex ~ VegDiversity + VegDensity + NMDS1 + NMDS2 + (1|Moonlight) + (1|Habitat))
Geometrid.floristic.model <- lmer(Geometrid.floristic, data=covar_moth)
summary(Geometrid.floristic.model)


##Arctiid Diversity (Fisher's alpha) Structural Model
Arctiid.structural <- as.formula(Arctiine_FisherIndex ~ UnderComplex + CanopyCover + VerticalComplex + (1|Moonlight) + (1|Habitat))
Arctiid.structural.model <- lmer(Arctiid.structural, data=covar_moth)
summary(Arctiid.structural.model)


f1 <- lmer(Arctiine_FisherIndex ~ UnderComplex + CanopyCover + VerticalComplex + (1|Moonlight) + (1|Habitat), data = covar_moth); summary(f1)

f2 <- glmer(Geometrid_FisherIndex ~ UnderComplex + CanopyCover + VerticalComplex + (1|Moonlight) + (1|Habitat), data = covar_moth, family=Gamma(link=log)); summary(f2)

f3 <- lmer(Arctiine_FisherIndex ~ VegDiversity + VegDensity + NMDS1 + NMDS2 + (1|Moonlight) + (1|Habitat), data = covar_moth); summary(f3)

f4 <- glmer(Geometrid_FisherIndex ~ VegDiversity + VegDensity + NMDS1 + NMDS2 + (1|Moonlight) + (1|Habitat), data = covar_moth, family=Gamma(link=log)); summary(f4)

fnull <- glmer(Geometrid_FisherIndex ~ 1 + (1|Moonlight) + (1|Habitat), data = covar_moth, family=Gamma(link=log)); summary(fnull)


plot(log(Geometrid_FisherIndex)~VegDiversity, data=covar_moth)

G.VDiv <- ggplot(covar_moth, aes(x=VegDiversity, y=Geometrid_FisherIndex)) +
  geom_point(size=3, shape=19)+
  geom_point()+
  scale_colour_grey()+
  # geom_text(aes(label=ifelse(rlw>30,as.character(zcta5),'')),hjust=0,vjust=0)+
  theme(legend.position = "top")+
  xlab("Vegetation Diversity") + ylab("Geometrid Diversity")

G.NMDS1 <- ggplot(covar_moth, aes(x=NMDS1, y=Geometrid_FisherIndex)) +
  geom_point(size=2, shape=19)+
  geom_point(aes(colour = factor(Habitat)))+
  scale_colour_grey()+
  # geom_text(aes(label=ifelse(rlw>30,as.character(zcta5),'')),hjust=0,vjust=0)+
  theme(legend.position = "none")+
  xlab("Vegetation Composition NMDS1") + ylab("Geometrid Diversity")

install.packages("cowplot")
library(cowplot)
plot_grid(G.VDiv, G.NMDS1, ncol=2, nrow=1)

####  for R Markdown  ####

shapiro.test(covar_moth$Arctiine_FisherIndex)
shapiro.test(covar_moth$Geometrid_FisherIndex)

install.packages("fitdistrplus")
library(fitdistrplus)

descdist(covar_moth$Geometrid_FisherIndex)    
descdist(covar_moth$Arctiine_FisherIndex)

install.packages("lmerTest")
library(lmerTest)

##Geometrid Diversity (Fisher's alpha) Structural Model
Geometrid.structural <- glmer(Geometrid_FisherIndex ~ UnderComplex + CanopyCover + VerticalComplex + (1|Moonlight) + (1|Habitat), data = covar_moth, family=Gamma(link=log))
summary(Geometrid.structural)

##Arctiid Diversity (Fisher's alpha) Structural Model
Arctiid.structural <- lmer(Arctiine_FisherIndex ~ UnderComplex + CanopyCover + VerticalComplex + (1|Moonlight) + (1|Habitat), data = covar_moth)
summary(Arctiid.structural)

##Geometrid Diversity (Fisher's alpha) Floristic Model
Geometrid.floristic <- glmer(Geometrid_FisherIndex ~ VegDiversity + VegDensity + NMDS1 + NMDS2 + (1|Moonlight) + (1|Habitat), data = covar_moth, family=Gamma(link=log))
summary(Geometrid.floristic)

##Arctiid Diversity (Fisher's alpha) Floristic Model
Arctiid.floristic <- lmer(Arctiine_FisherIndex ~ VegDiversity + VegDensity + NMDS1 + NMDS2 + (1|Moonlight) + (1|Habitat), data = covar_moth)
summary(Arctiid.floristic)


install.packages("cowplot")
library(cowplot)

G.UCom <- ggplot(covar_moth, aes(x=UnderComplex, y=log(Geometrid_FisherIndex))) +
  geom_point(size=2, shape=19)+
  geom_smooth(method = "lm", se=FALSE) + scale_colour_discrete()+
  xlab("Understory Complexity") + ylab("Geometrid alpha Diversity") + theme_

G.CC <- ggplot(covar_moth, aes(x=CanopyCover, y=log(Geometrid_FisherIndex))) +
  geom_point(size=2, shape=19)+
  geom_smooth(method = "lm", se=FALSE) + scale_colour_discrete()+
  xlab("Canopy Cover (%)") + ylab("")

G.VCom <- ggplot(covar_moth, aes(x=VerticalComplex, y=log(Geometrid_FisherIndex))) +
  geom_point(size=2, shape=19)+
  geom_smooth(method = "lm", se=FALSE) + scale_colour_discrete()+
  xlab("Vertical Complexity") + ylab("")

plot_grid(G.UCom, G.CC, G.VCom, ncol=3, nrow=1)

A.UCom <- ggplot(covar_moth, aes(x=UnderComplex, y=Arctiine_FisherIndex)) +
  geom_point(size=2, shape=19)+
  geom_smooth(method = "lm", se=FALSE) + scale_colour_discrete()+
  xlab("Understory Complexity") + ylab("Arctiine alpha Diversity") 

A.CC <- ggplot(covar_moth, aes(x=CanopyCover, y=Arctiine_FisherIndex)) +
  geom_point(size=2, shape=19)+
  geom_smooth(method = "lm", se=FALSE) + scale_colour_discrete()+
  xlab("Canopy Cover (%)") + ylab("")

A.VCom <- ggplot(covar_moth, aes(x=VerticalComplex, y=Arctiine_FisherIndex)) +
  geom_point(size=2, shape=19)+
  geom_smooth(method = "lm", se=FALSE) + scale_colour_discrete()+
  xlab("Vertical Complexity") + ylab("")

plot_grid(A.UCom, A.CC, A.VCom, ncol=3, nrow=1)

G.VDiv <- ggplot(covar_moth, aes(x=VegDiversity, y=log(Geometrid_FisherIndex))) +
  geom_point(size=2, shape=19)+
  geom_smooth(method = "lm", se=FALSE) + scale_colour_discrete()+
  xlab("Plant alpha Diversity") + ylab("Geometrid alpha Diversity") 

G.VDen <- ggplot(covar_moth, aes(x=VegDensity, y=log(Geometrid_FisherIndex))) +
  geom_point(size=2, shape=19)+
  geom_smooth(method = "lm", se=FALSE) + scale_colour_discrete()+
  xlab("Plant Density") + ylab("")

G.NMDS1 <- ggplot(covar_moth, aes(x=NMDS1, y=log(Geometrid_FisherIndex))) +
  geom_point(size=2, shape=19)+
  geom_smooth(method = "lm", se=FALSE) + scale_colour_discrete()+
  xlab("Plant Composition NMDS1") + ylab("Geometrid alpha Diversity")

G.NMDS2 <- ggplot(covar_moth, aes(x=NMDS2, y=log(Geometrid_FisherIndex))) +
  geom_point(size=2, shape=19)+
  geom_smooth(method = "lm", se=FALSE) + scale_colour_discrete()+
  xlab("Plant Composition NMDS2") + ylab("")

plot_grid(G.VDiv, G.VDen, G.NMDS1, G.NMDS2, ncol=2, nrow=2)

A.VDiv <- ggplot(covar_moth, aes(x=VegDiversity, y=Arctiine_FisherIndex)) +
  geom_point(size=2, shape=19)+
  geom_smooth(method = "lm", se=FALSE) + scale_colour_discrete()+
  xlab("Plant alpha Diversity") + ylab("Arctiine alpha Diversity")

A.VDen <- ggplot(covar_moth, aes(x=VegDensity, y=Arctiine_FisherIndex)) +
  geom_point(size=2, shape=19)+
  geom_smooth(method = "lm", se=FALSE) + scale_colour_discrete()+
  xlab("Plant Density") + ylab("")

A.NMDS1 <- ggplot(covar_moth, aes(x=NMDS1, y=Arctiine_FisherIndex)) +
  geom_point(size=2, shape=19)+
  geom_smooth(method = "lm", se=FALSE) + scale_colour_discrete()+
  xlab("Plant Composition NMDS1") + ylab("Arctiine alpha Diversity")

A.NMDS2 <- ggplot(covar_moth, aes(x=NMDS2, y=Arctiine_FisherIndex)) +
  geom_point(size=2, shape=19)+
  geom_smooth(method = "lm", se=FALSE) + scale_colour_discrete()+
  xlab("Plant Composition NMDS2") + ylab("")

plot_grid(A.VDiv, A.VDen, A.NMDS1, A.NMDS2, ncol=2, nrow=2)


