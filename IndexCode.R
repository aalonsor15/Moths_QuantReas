
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

G_matrix <- read.csv('Geo_full_matrix.csv')
G_fisher <- fisher.alpha(G_matrix) #Error: returns really high numbers for some rows
G_fisher <- as.data.frame(G_fisher)

A_matrix <- read.csv('Arc_full_matrix.csv')
A_fisher <- fisher.alpha(A_matrix) #Error: doesn't run for some reason
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

