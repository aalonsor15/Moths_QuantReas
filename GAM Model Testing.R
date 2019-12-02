install.packages('mgcv')
require(mgcv)
library(dplyr)

#Arctiid Diversity as a function of Tree Diversity, Vegetation density, Understory complexity, Canopy Cover, Vertical Complexity
Arctiidae.allvariables <- as.formula(Fisher.s.alpha~ s(Fisher.alpha, bs="cr", k=3) + s(Vegetation.density._.2cm.DBH..N.314m_., bs="cr", k=3) + s(Understory.complexity..Shannon., bs="cr", k=3) + s(Canopy.cover...., bs="cr", k=3) + s(Vertical.complexity..Shannon., bs="cr", k=3))
Arctiid_model <- gam(Arctiidae.allvariables, data=All_data, na.action=na.exclude)

#Geometrid Diversity as a function of Tree Diversity, Vegetation density, Understory complexity, Canopy Cover, Vertical Complexity
Geometridae.allvariables <- as.formula(Fisher.s.alpha.1~ s(Fisher.alpha, bs="cr", k=3) + s(Vegetation.density._.2cm.DBH..N.314m_., bs="cr", k=3) + s(Understory.complexity..Shannon., bs="cr", k=3) + s(Canopy.cover...., bs="cr", k=3) + s(Vertical.complexity..Shannon., bs="cr", k=3))
Geometrid_model <-gam(Geometridae.allvariables, data=All_data, na.action=na.exclude)

#Arctiid Diversity as a function of Tree Diversity alone
Arctiidae.treediversity <- as.formula(Fisher.s.alpha~ s(Fisher.alpha))
Arctiidtreemodel <- gam(Arctiidae.treediversity, data=All_data)

dput(All_data)
model

