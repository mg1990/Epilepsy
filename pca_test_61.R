
pca_test <- New_features[,-97]

chosen.component <- 1:6

# normalising the dataset for PCA

pc.seiz.norm <- data.frame(scale(pca_test,center = TRUE)) 

data.scale.pca <- prcomp(pc.seiz.norm)

feature.vector <- data.scale.pca$rotation[, chosen.component] 

# 67% of the data explained by 6 PCA : found through scree plot and selected through parallel analysis

newData_1 <- data.frame(t((t(feature.vector) %*% t(pc.seiz.norm))))
write.csv(newData_1,"PCA_test_feature1.csv")
