# Libraries Required :

require(R.matlab) || install.packages("R.matlab")
require(stringi) || install.packages("stringi")
require(data.table) || install.packages("data.table")
require(e1071) || install.packages("e1071")
require(randomForest) || install.packages("randomForest")
require(RWeka) || install.packages("RWeka")
require(rpart) || install.packages("rpart")
require(rpart.plot) || install.packages("rpart.plot")


library(data.table)
library(R.matlab)
library(stringi)

####################################################################3

# Mat to CSV conversion function :


getAllMatFile <- function() {
  return(list.files(pattern = "\\.(mat)$"))
}

# Path :

setwd("D:/R/Epilepsy/Dog_1")

toCSV <- function() {
  allMatFiles <- sort(getAllMatFile())
  
    for (i in 1:length(allMatFiles)) {
      matData <- readMat(allMatFiles[i])
      data.df <- data.frame(t(matData[[1]][[1]]))
      colnames(data.df) <- paste('V', 1:(length(colnames(data.df))), sep = "")
      write.csv(data.df, paste(stri_sub(allMatFiles[i], 1,-4), "csv", sep = ""))
      print(paste("Converted", allMatFiles[i]))
    }  
}

# calling function

toCSV()

#------------------------------------------------------------------------------------------#

#-------fft_abs conversion function :

GetAllcsvfiles <- function(){
  return(list.files(pattern = "\\.(csv)$"))
}

fft_abs <- function(){
allcsvfiles <- sort(list.files(pattern="*.csv"))
for (i in 1 : length(allcsvfiles)){
  data <- data.frame(fread(allcsvfiles[i]))
  data.df <- data[,-1]
  fft.df <- data.frame(lapply(lapply(data.df, fft), abs))
  write.csv(fft.df, paste("fft_",paste(stri_sub(allcsvfiles[i], 
                                                1,-5),"fft"), ".csv", sep = ""))
  print(paste("Bhaiya Done with", allcsvfiles[i]))
} 
}

# Calling function 

fft_abs() 

#---------------------------- Feature Extraction ---------------------------------------#

########################## Feature 1 ####################################

# Read fft.csv files :

GetFFTcsvfiles <- function(){
  return(list.files(pattern = "\\fft.(csv)$"))
}

v_alpha=vector()
v_beta=vector()
v_lowgama=vector()
v_highgamma=vector()
v_delta=vector()
v_theta=vector()

AllFFT <- function(){

  allFFTfiles <- sort(list.files(pattern="*fft.csv"))
  for (k in 1:length(allFFTfiles)){
    data.fft <- data.frame(fread(allFFTfiles[k]))
    data.fft.df <- data.fft[,-1]
    for(i in 1 : ncol(data.fft.df)){
      #feature() 
      #8-12 & 12-30
      v_alpha[i] <- sum(data.fft.df[which(data.fft.df[,i]<12 & data.fft.df[,i]>8),i])
      v_beta[i] <- sum(data.fft.df[which(data.fft.df[,i]<30 & data.fft.df[,i]>12),i])
      #30-70 & 70 to 180
      v_lowgama[i] <- sum(data.fft.df[which(data.fft.df[,i]<70 & data.fft.df[,i]>30),i])
      v_highgamma[i] <- sum(data.fft.df[which(data.fft.df[,i]<180 & data.fft.df[,i]>70),i])
      #0.1-4 & 4-8  
      v_delta[i] <- sum(data.fft.df[which(data.fft.df[,i]<4 & data.fft.df[,i]>0.1),i])
      v_theta[i] <- sum(data.fft.df[which(data.fft.df[,i]<8 & data.fft.df[,i]>4),i])
    }
    feature_raw <- cbind(v_alpha,v_beta,v_delta,v_highgamma,v_lowgama,v_theta)
    
    Add_Feature <- list()
    
    for(j in 1:ncol(feature_raw)){
      Add_Feature[[j]]  <- as.data.frame(t(feature_raw[,j]))      
    }
    feature_raw_1  <- data.frame(cbind(Add_Feature[[1]],Add_Feature[[2]],Add_Feature[[3]],Add_Feature[[4]],Add_Feature[[5]],Add_Feature[[6]]))
    write.csv(feature_raw_1,paste("feature_", stri_sub(allFFTfiles[k],11,-9),".csv", sep = ""))
    print(paste("Bhaiya Done with", allFFTfiles[k]))
  }
  
}

AllFFT()

# Final Features CSV :

setwd("D:/R/Epilepsy/Features")

GetAllcsvfiles <- function(){
  return(list.files(pattern = "\\.(csv)$"))
}

f <- GetAllcsvfiles()
g <- lapply(f, read.csv)
t <- vector()
y <- vector()
for (u in 1:length(g)){
  t <- g[[u]]
  y <- rbind(y,t)
  print("done with all the features go to sleep.")
}
Final_features <- data.frame(y[-1])
View(Final_features)

write.csv(Final_features,"Final_Features.csv",row.names=T)


# Appending paramenter for Interictal or preictal :

a <- GetAllcsvfiles()
#b <- lapply(f, read.csv)
c <- vector()

for (z in 1:length(a)){
   if(substr(a[z],9,18)== 'interictal'){
    c[z] <- '0'
  }
  else{
    c[z] <- '1'
  }
  
  print("done with all the parameters go to sleep.")
}

View(c)

New_features <- cbind(Final_features,c)

write.csv(New_features,"New_features.csv")

#########################  Feature 2  ##############################

# Features of correlations :

# Read fft.csv files :

setwd("D:/R/Epilepsy/Dog_1")

GetFFTcsvfiles <- function(){
  return(list.files(pattern = "\\fft.(csv)$"))
}

allFFTfiles <- sort(list.files(pattern="*fft.csv"))
for (k in 1:length(allFFTfiles)){ 
  data.fft <- data.frame(fread(allFFTfiles[k]))
  data.fft.df <- data.fft[,-1]
  
  fft_1_cor <- cor(data.fft.df)
  
  value = NULL
  value1 = NULL
  
  for(i in 1 : nrow(fft_1_cor))
  {
    for(j in 2 : ncol(fft_1_cor))
    {
      if(j>i)
      {
        
        value<-fft_1_cor[i,j]
        value1<-rbind.data.frame(value1,value)
        rm(value)
      }
      
    }
  }
  
  write.csv(value1,paste("New_feature_", stri_sub(allFFTfiles[k],11,-9),".csv", sep = ""))
  print(paste("Bhaiya Done with", allFFTfiles[k]))
}

# read all new features csv to make it one final new feature csv :

setwd("D:/R/Epilepsy/Fratures_1")

GetAllcsvfiles <- function(){
  return(list.files(pattern = "\\.(csv)$"))
}

f <- GetAllcsvfiles()
g <- lapply(f, read.csv)
t <- vector()
y <- rep(NA,120)
for (u in 1:length(g)){
  l <- g[[u]] 
  t <- l[-1]
  y <- cbind(y,t)
  print("done with all the new_features go to home.")
}

new_file <- t(y[-1])

View(new_file)

# Appending paramenter for Interictal or preictal :

a <- GetAllcsvfiles()

c <- vector()

for (z in 1:length(a)){
  if(substr(a[z],13,22)== 'interictal'){
    c[z] <- '0'
  }
  else{
    c[z] <- '1'
  }
  
  print("done with all the parameters go to sleep.")
}

View(c)

New_features_2 <- cbind(new_file,c)

write.csv(New_features_2,"New_final_features_2.csv")


#--------------------- balancing of features file using SMOTE function -----------------------------#

require(DMwR) || install.packages("DMwR")
library(DMwR)

# Fearure 1 :

New_features$type <- factor(New_features$type) 

Balance_features_1 <- SMOTE(type~.,New_features,perc.over = 1200,perc.under = 194)
table(Balance_features_1$type)

# Feature 2 :

New_features_2$Type <- factor(New_features_2$Type) 

Balance_features_2 <- SMOTE(Type~.,New_features_2,perc.over = 1200,perc.under = 194)
table(Balance_features_2$Type)

write.csv(Balance_features_2,"Balanced_Features_2.csv")


#------------------------------ Applying PCA for dimentionality reduction ---------------------------------#

# Feature 1 on Balance data for training data :

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

Train_feature1 <- data.frame(t((t(feature.vector) %*% t(pc.seiz.norm))))

# Matrix multiplication is performed.Here we get principal component scores

#Component Summaries or renaming the component is difficult because the electrodes span from one region to many regions in brain

write.csv(Train_feature1,"PCA_train_feature1.csv")


# Feature 1 on unbalance data for testing data :

pca_test <- New_features[,-97]

chosen.component <- 1:6

# normalising the dataset for PCA

pc.seiz.norm <- data.frame(scale(pca_test,center = TRUE)) 

data.scale.pca <- prcomp(pc.seiz.norm)

feature.vector <- data.scale.pca$rotation[, chosen.component] 

# 67% of the data explained by 6 PCA : found through scree plot and selected through parallel analysis

Test_feature1 <- data.frame(t((t(feature.vector) %*% t(pc.seiz.norm))))

write.csv(Test_feature1,"PCA_test_feature1.csv")


## Feature 2 on Balance data for training data :

New_smote <- Balance_features_2[,-121]

chosen.component_tr <-1:33
New_smote.norm <- data.frame(scale(New_smote,center = T))
data.scale.pca_tr <- prcomp(New_smote.norm)
summary(data.scale.pca_tr)
feature.vector_tr <- data.scale.pca_tr$rotation[, chosen.component_tr]
Train_feature2 <- data.frame(t((t(feature.vector_tr) %*% t(New_smote.norm))))
View(Train_feature2)
#data.scale <- log(data[,-121]) # Log will remove skewness from data
# Since skewness and the magnitude of the variables influence the resulting PCs,
# it is good practice to apply skewness transformation,
# center and scale the variables prior to the application of PCA
write.csv(Train_feature2,"PCA_train_feature2.csv")


## Feature 2 on unbalance data for testing data :

New_unsmote <- New_features_2[,-121]
New_unsmote_cor <- cor(New_unsmote)

library(arm)
library(nFactors)
library(psych)

corrplot(New_unsmote_cor, abs= T,color=T)

Bartlett.sphericity.test(New_unsmote)

eigenvalues <- eigen(New_unsmote_cor)

aproximation <- parallel(subject=nrow(New_unsmote), var=ncol(New_unsmote), rep=100, cent=.05)

nooffactors <- nScree(x=eigenvalues$values, aparallel=aproximation$eigen$qevpea)
plot(nooffactors)

chosen.component1 <-1:33

New_unsmote.norm <- data.frame(scale(New_unsmote1,center = T))

data.scale.pca1 <- prcomp(New_unsmote.norm)

summary(data.scale.pca1)

feature.vector1 <- data.scale.pca1$rotation[, chosen.component1]

Test_feature2 <- data.frame(t((t(feature.vector1) %*% t(New_unsmote.norm))))

View(Test_feature2)
#data.scale <- log(data[,-121]) # Log will remove skewness from data
# Since skewness and the magnitude of the variables influence the resulting PCs,
# it is good practice to apply skewness transformation,
# center and scale the variables prior to the application of PCA
write.csv(Test_feature2,"PCA_test_feature2.csv") 


#------------------------------Applying Models-----------------------------------------#

Outcomes <- function(conf){
  
  total <- sum(conf[1,1],conf[1,2],conf[2,1],conf[2,2])
  
  accuracy <- (sum(diag(conf))/total)*100
  recall  <- (conf[2,2]/sum(conf[2,1],conf[2,2]))*100
  specificity <- (conf[1,2]/sum(conf[1,1],conf[1,2]))*100
  precision <-  (conf[2,2]/sum(conf[1,2],conf[2,2]))*100
  
  print(paste("Accuracy of model is ",accuracy,sep=" "))
  print(paste("Recall of model is ",recall,sep=" "))
  print(paste("Specificity of model is ",specificity,sep=" "))
  print(paste("Precision of model is ",precision,sep=" "))
  
}

ApplyModels <- function(X,Y){
  
  # Dividing data into train and test :
  
  smp_size <- floor(0.7 * nrow(X))
  
  ## set the seed to make your partition reproductible
  
  set.seed(123)
  train_index <- sample(seq_len(nrow(X)), size = smp_size)
  
  train_data <- X[train_index, ]
  test_data <-  X[-train_index, ]
  
  valid_data <- Y
  
  View(train_data)
  View(test_data)
  View(valid_data)
  
  ##############################################################
  
  # Applying Models :
  
  # Decision tree :
  
  #require(rpart) || install.packages("rpart")
  library(rpart)
  
  #require(rpart.plot) || install.packages("rpart.plot")
  library(rpart.plot)
  
  # Feature 1 :
  
  dtfit_1 <- rpart(type~PC1+PC2+PC4+PC5+PC6, data = train_data, method="class")
  plot(dtfit_1, uniform = TRUE, compress = TRUE)
  text(dtfit_1, use.n = TRUE, all = TRUE, cex = .7)
  printcp(dtfit_1)
  plotcp(dtfit_1)
  
  #prune the tree0
  
  pfit<- prune(dtfit_1, cp=   dtfit_1$cptable[which.min(dtfit_1$cptable[,"xerror"]),"CP"])
  prp(pfit,type=2,extra=2, uniform=T,cex = .6, main="Pruned Classification Tree for training model ")
  
  # Predict the test on decision table :
  
  dtpredict_1 <- predict(dtfit_1,test_data,type="class")
  
  dtpredict_1_valid <- predict(dtfit_1,valid_data,type="class")
  
  # Cross Tabulation across true value :
  
  dtconfmat_1 <- table(true = test_data[,6], pred = dtpredict_1)
  
  dtconfmat_1
  
  dtconfmat_1_valid <- table(true = valid_data[,6], pred = dtpredict_1_valid)
  
  dtconfmat_1_valid
  
  print("Outcomes of Decision tree")
  print("For test data")
  
  View(dtconfmat_1)
  Outcomes(dtconfmat_1)
  
  print("For valid data")
  
  print(dtconfmat_1_valid)
  
  Outcomes(dtconfmat_1_valid)
  #---------------------------------------------------------------------------------
  
  # KNN algorithm :
  
  # Feature 1 :
  
  #require(RWeka) || install.packages("RWeka")
  library(RWeka)
  
  knnmodel_1 <- IBk(type~., data = train_data)
  
  # Predict the test set :
  
  knnpredict_1 <- predict(knnmodel_1,test_data[,-6])
  
  knnpredict_1_valid <- predict(knnmodel_1,valid_data[,-6])
  
  # Cross Tabulation of result :
  
  knnconfmat_1 <- table(true = test_data[,6], pred = knnpredict_1)
  
  knnconfmat_1
  
  knnconfmat_1_valid <- table(true = valid_data[,6], pred = knnpredict_1_valid)
  
  knnconfmat_1_valid
  
  print("Outcomes of KNN Model")
  print("For test data")
  print(knnconfmat_1)
  Outcomes(knnconfmat_1)
  
  print("For valid data")
  print(knnconfmat_1_valid)
  Outcomes(knnconfmat_1_valid)
  
  #----------------------------------------------------------------------------------
  
  # Random Forest :
  
  # Feature 1 :
  
  #require(randomForest) || install.packages("randomForest")
  library(randomForest)
  
  train_data$type <- as.factor(train_data$type)
  test_data$type <- as.factor(test_data$type)
  valid_data$type <- as.factor(valid_data$type)
  
  rffit_1 <- randomForest(type~., data = train_data,do.trace = 100, importance = T, ntree = 900, mtry = 3)
  print(rffit_1)
  
  # Predict the test set :
  
  rfpredict_1 <- predict(rffit_1,test_data[,-6])
  
  rfpredict_1_valid <- predict(rffit_1,valid_data[,-6])
  
  # Cross Validation :
  
  rfconfmat_1 <- table(true = test_data[,6], pred = rfpredict_1)
  
  rfconfmat_1
  
  rfconfmat_1_valid <- table(true = valid_data[,6], pred = rfpredict_1_valid)
  
  rfconfmat_1_valid
  
  
  print("Outcomes of Random Forest")
  print("for test data")
  print(rfconfmat_1)
  Outcomes(rfconfmat_1)
  
  print("for valid data")
  print(rfconfmat_1_valid)
  Outcomes(rfconfmat_1_valid)
  
  #---------------------------------------------------------------------------------
  
  # SVM :
  
  # Feature 1 :
  
  #require(e1071) || install.packages("e1071")
  library(e1071)
  
  svmmodel_1 <- svm(type~., data = train_data,type = "C-classification",kernel = "linear")
  
  # Predict the test set :
  
  svmpredict_1 <- predict(svmmodel_1,test_data[,-6])
  
  svmpredict_1_valid <- predict(svmmodel_1,valid_data[,-6])
  # Cross Tabulation :
  
  svmconfmat_1 <- table(true = test_data[,6], pred = svmpredict_1)
  
  svmconfmat_1
  
  svmconfmat_1_valid <- table(true = valid_data[,6], pred = svmpredict_1_valid)
  
  svmconfmat_1_valid
  
  print("Outcomes of SVM Model")
  print("for test data")
  print(svmconfmat_1)
  Outcomes(svmconfmat_1)
  
  print("for valid data")
  print(svmconfmat_1_valid)
  Outcomes(svmconfmat_1_valid)
  
}

print("Outcomes for feature 1")

ApplyModels(Train_feature1,Test_feature1)

print("Outcomes for feature 2")

ApplyModels(Train_feature2,Test_feature2)

#########################  End of code  ###########################################
