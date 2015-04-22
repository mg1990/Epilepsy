data.pca.test <- read.csv(file.choose())

data.pca.test$type<-factor(data.pca.test$type)

seizure.lm <- readRDS("seizure_lm")

summary(seizure.lm)

glm_pred.test<- predict(seizure.lm,data.pca.test)

glm.pred.test=rep ("0" ,504)

glm.pred.test[glm_pred.test>.5]="1"

confusion_matrix.test<- table(glm.pred.test,data.pca.test$type)

accuracy.test <- sum(diag(confusion_matrix.test))/504
#Accuracy of training model on test data 89%

###----PCA61-----##
data.pca.test.61 <- read.csv(file.choose())

data.pca.test.61$type<-factor(data.pca.test.61$type)

seizure.lm.61 <- readRDS("seizure_lm.61")

glm_pred.test.61<- predict(seizure.lm.61,data.pca.test.61)

glm.pred.test.61=rep ("0" ,504)

glm.pred.test.61[glm_pred.test.61>.5]="1"

confusion_matrix.test.61<- table(glm.pred.test.61,data.pca.test.61$type)

accuracy.test <- sum(diag(confusion_matrix.test.61))/504


