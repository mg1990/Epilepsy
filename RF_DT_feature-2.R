pca_final_unsmote_4 <- read.csv('pca_final_unsmote_4.csv',header =T)
pca_fea_smote2 <- read.csv('pca_final_smote_4.csv')

#-----------feature-2 Random forest,KNN,Decision tree---------
library(randomForest)
random_for <- read.csv('pca_final_smote_4.csv')

random_for$Type <- as.factor(random_for$Type)

pca_fea_smote2$Type <- as.factor(pca_fea_smote2$Type)
pca_final_unsmote_4$type <- as.factor(pca_final_unsmote_4$type)

rffit <- randomForest(Type~. , data= random_for, importance = T, mtry = 12,ntree = 900 ,do.trace = 100)
print(rffit)
rfpredicit <- predict(rffit,pca_final_unsmote_4[,-34])
rfconfmat <- table(true = pca_final_unsmote_4$type, pred = rfpredicit)
rfconfmat
#--------Random forest is giving 94.51% accuracy--------

#------Decision tree---------
library(rpart)
dtfit <- rpart(Type~. ,data=pca_fea_smote2, method= "class")
plot(dtfit, uniform=TRUE, compress=TRUE)
text(dtfit, use.n=TRUE, all=TRUE, cex=.7)
dtpredict <- predict(dtfit,pca_final_unsmote_4,type="class")
dtconfmat <- table(true = pca_final_unsmote_4$type, pred = dtpredict)
dtconfmat
accuracy.test <- sum(diag(dtconfmat))/504
#----------accuracy is 78%-------------
#--------Support vector machine--------
library(e1071)
svmfit =svm(Type∼., data=pca_fea_smote2,type = "C-classification",kernel= "linear" ,scale =F )
summary(svmfit)
ypred=predict (svmfit ,pca_final_unsmote_4[,-34] )
svmtable <- table(predict =ypred , truth= pca_final_unsmote_4[,34])
accuracy.test.svm <- sum(diag(svmtable))/504
#-----59.3% accuracy-------
#--------KNN------
library(RWeka)
knnmodel <- IBk(Type~., data = pca_fea_smote2)
knnpredict <- predict(knnmodel, pca_final_unsmote_4[,-34])
knnconfmat <- table(true = pca_final_unsmote_4[,34], pred = knnpredict)
knnconfmat
accuracy.test.knn <- sum(diag(knnconfmat))/504
#------63.09% accuracy-----------
#-----Adaboost------------
library(ada)
control <- rpart.control(cp = -1, maxdepth = 14,maxcompete = 1,xval = 0)
adamodel <- ada(Type~., data = pca_fea_smote2,type = "gentle",control = control, iter = 70)
adapred <- predict(adamodel,pca_final_unsmote_4[,-34])
adaconfmat <- table(true=pca_final_unsmote_4[,34],pred=adapred)
adaconfmat
accuracy.test.ada <- sum(diag(adaconfmat))/504
