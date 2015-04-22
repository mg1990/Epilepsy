balance <- read.csv(file.choose(),header = T)
unbalance <- read.csv(file.choose(),header = T)

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
  
  require(rpart) || install.packages("rpart")
  library(rpart)
  
  require(rpart.plot) || install.packages("rpart.plot")
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
  
  require(RWeka) || install.packages("RWeka")
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
  
  require(randomForest) || install.packages("randomForest")
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
  
 # require(e1071) || install.packages("e1071")
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

ApplyModels(balance,unbalance)

