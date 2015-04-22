data.pca.5 <- read.csv(file.choose())

#glm.fit_1.5<- glm(type~.,data=data.pca.5,family= binomial)

#summary(glm.fit)
#PC2 has a low p-value, hence we are dropping PC2 variable

data.pca.5$type<-factor(data.pca.5$type)

glm.fit_3<- glm(type~.,data=data.pca.5,family= binomial)

summary(glm.fit_3)

#All variables are having p-value less than 0.05
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

