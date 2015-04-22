library(arm)
library(pryr)

pc.seiz <- Balance_features_1[,-97]
seiz.cor <- cor(pc.seiz)
corrplot(seiz.cor,abs=T,color = T)

source("C:/R Programming/Data/Bartlett.sphericity.test.R")
source("C:/R Programming/Data/KMO_Test.R")

Bartlett.sphericity.test(pc.seiz)# corelation or not?
kmo(pc.seiz)# Sample adequacy test 0.7> good sample

library(nFactors)

# get eigenvalues
eigenvalues <- eigen(seiz.cor)
aproximation <- parallel(subject=nrow(pc.seiz), var=ncol(pc.seiz), rep=100, cent=.05)

# to get distribution of eigen value and to use $qevpea
# to find number of factors

nooffactors <- nScree(x=eigenvalues$values, aparallel=aproximation$eigen$qevpea) 

plotnScree(nooffactors)

#According to parallel analysis:  A factor or component is retained if the associated eigenvalue is bigger than the 95th of the distribution of eigenvalues derived from the random data.

nooffactors$Components$nparallel

#number of factors: 6
# from parallel analysis

chosen.component <-1:6

# normalising the dataset for PCA

pc.seiz.norm <- data.frame(scale(pc.seiz,center = TRUE)) 

library(psych)

data.scale.pca <- principal(pc.seiz.norm,nfactors=6)

feature.vector <- data.scale.pca$rotation[, chosen.component] 

# 67% of the data explained by 6 PCA : found through scree plot and selected through parallel analysis

newData_1 <- data.frame(t((t(feature.vector) %*% t(pc.seiz.norm))))

# Matrix multiplication is performed.Here we get principal component scores

#Component Summaries or renaming the component is difficult because the electrodes span from one region to many regions in brain

write.csv(newData_1,"PCA_train_feature1.csv")
