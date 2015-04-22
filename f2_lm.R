library(car)

f2.data <- read.csv(file.choose())

f2.data <- f2.data[,-35]

f2.data$Type<-factor(f2.data$Type)


f2.lm <- glm(Type~.-PC6-PC11-PC13-PC14-PC16-PC23-PC24-PC25-PC27-PC28-PC4-PC5-PC22-PC31-PC33,data=f2.data,family= binomial)

summary(f2.lm)

vif(f2.lm)
#VIF is 1 for all variables

saveRDS(f2.lm,"feature2_lm")


###---------Model Accuracy on test data--------#########
f2_lm.test <- readRDS("feature2_lm")

f2.test <- read.csv(file.choose())

attach(f2.test)

f2.test$type<-factor(f2.test$type)

f2.predict<- predict(f2_lm.test,f2.test[,-which(names(f2.test) %in% c("PC6","PC11","PC13","PC14","PC16","PC23","PC24","PC25","PC27","PC28","PC4","PC5","PC22","PC31","PC33"))])

contrasts(f2.test$type)

f2_predict=rep ("0" ,504)

f2_predict[f2.predict>.5]="1"

confusion_matrix<- table(f2_predict,f2.test$type)

accuracy<- sum(diag(confusion_matrix))/504
#Accuracy 68.8% on test data using training model



