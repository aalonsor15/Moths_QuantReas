
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




