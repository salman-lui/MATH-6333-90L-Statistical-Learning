
"
Author: Md Salman Rahman
Course: MATH 6333 Statistical Learning
Course Instructor: Dr. Tamer Oraby
Final Project
"
setwd("/Users/salman/OneDrive - The University of Texas-Rio Grande Valley/Course_video/Statistical Learning/Homework_salman/Final_project")

library(ggplot2)
library(e1071)
library(ROCR)
library(irr)
library(caret)
#loading data set

hepa_data <-  read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/hepatitis/hepatitis.data",header = FALSE)

colnames(hepa_data) <- c('Class','AGE','SEX','STEROID',
            'ANTIVIRALS','FATIGUE','MALAISE','ANOREXIA','LIVER BIG','LIVER FIRM','SPLEEN PALPABLE','SPIDERS','ASCITES',
            'VARICES','BILIRUBIN','ALK PHOSPHATE','SGOT','ALBUMIN','PROTIME','HISTOLOGY')

str(hepa_data)

# histogram 
hist(hepa_df$Class)
ggplot(data=hepa_df, aes(hepa_data$Class)) + 
  geom_histogram()

plot(hepa_df$Class)
# checking NA value
table(is.na(hepa_data))

summary(hepa_data)

hepa_data[hepa_data == '?'] <- NA

hepa <- function(a){
  a <-as.numeric(as.character(a))
  a[is.na(a)] = median(a,na.rm = TRUE)
  a
}

hepa_data <- data.frame(apply(hepa_data,2,hepa))

summary(hepa_data)
str(hepa_data)

# Correlation plot
pairs(hepa_df[,])

library(corrplot)
# correlation matrix 
corrplot(cor(hepa_data[,]),
         method = "number",
         type = "upper" # show only upper side
)

#identifying outlier 
boxplot(hepa_data$AGE~hepa_data$Class, main="Age",ylab="",xlab="")
boxplot(hepa_data$ALK.PHOSPHATE~hepa_data$Class, main="Alk_Phosphate",ylab="",xlab="")


norm <- function(p){
  return((p-min(p))/max(p)-min(p))
}

hepa_df <- as.data.frame(lapply(hepa_data, norm))

summary(hepa_df)
str(hepa_df)


hepa_df$Class <- ifelse(hepa_df$Class == -0.5, 'Live','Die')
hepa_df$Class <- as.factor(hepa_df$Class)
hepa.df.final <- hepa_df
str(hepa.df.final)

# training and testing data
set.seed(123)
ind = sample(2,nrow(hepa_df),replace=TRUE,prob=c(0.65,0.35))
training = hepa_df[ind==1,]
testing = hepa_df[ind==2,]
train <- as.data.frame(training)
test <- as.data.frame(testing)



# K nearest neighbor 

library(class)
library(MASS)
Choose_K<-function(K,M=1000){
  MSE<-0
  for(i in 1:M){
    train_knn<-sample(155,155-31) # for 5-fold CV
    KNN<-knn(hepa_df[train_knn,2:20], hepa_df[-train_knn,2:20], hepa_df[train_knn,1],
             k=K,prob=TRUE,use.all=FALSE)
    MSE<-MSE+mean(KNN != hepa_df[-train_knn,1])
  }
  CVK<-MSE/M
  return(CVK)
}


library("purrr")

LK<-1:35 
vCVK<-LK %>% map(function(K) Choose_K(K))

vCVK<-unlist(vCVK)
vCVK<-as.data.frame(vCVK)
colnames(vCVK)<-c("CVK")

ggplot()+
  geom_point(data=vCVK,aes(x=LK,y=CVK))+
  xlab("K")+
  ylab("CV")

LK[which.min(vCVK$CVK)]

library(class)
KNN <- knn(train[,2:20],train[,2:20], train[,1],
           k=9, prob=TRUE,use.all=TRUE) # with appropriate k = 9


# prediction using test data 

library(class)
knn_test <- knn(train[,2:20],test[,2:20],train[,1],
                k=9, prob=TRUE,use.all = TRUE)

# error 

error <- mean(knn_test != test[,1])
error

# confusion matrix
library(caret)
knn.conf <- confusionMatrix(data=as.factor(knn_test),reference = test$Class)
knn.conf$byClass


# ROC curve

library(pROC)
roc.knn <- plot.roc(test$Class, as.numeric(knn_test))

#AUC
roc.1 <- roc(test$Class, as.numeric(knn_test))
auc(roc.1)


# logistic regression

glm.fit.case <- glm(Class ~., data=train, family = "binomial")
summary(glm.fit.case)
coef(glm.fit.case)
# prediction
logis.pred <- predict(glm.fit.case, newdata=test, type = "response")

logis.pred.1 <-ifelse(logis.pred>=0.5,"Live","Die")

# confusion matrix
library(caret)
logis.confu <- confusionMatrix(data=as.factor(logis.pred.1),reference = test$Class)
logis.confu$byClass
# ROC curve

library(pROC)
roc.logis <- plot.roc(test$Class, as.numeric(logis.pred))

#AUC
roc.1 <- roc(test$Class, as.numeric(logis.pred))
auc(roc.1)

# error 

error <- mean(logis.pred.1 != test[,1])
error



# Linear Discriminant Analysis

library(MASS)

lda_fit <- lda(Class ~., data=train)
lda_fit

plot(lda_fit, col = as.integer(train$Class))

library(klaR)

partimat(Class ~ ., data=train, method="lda")

# prediction 

lda_predict <- predict(lda_fit,test)$class
error_lda <- mean(lda_predict != test[,1])

# confusion matrix

library(caret)
expected <- factor(test[,1])
predicted <- factor(lda_predict)
results <- confusionMatrix(data=predicted, reference=expected)
print(results)
results$byClass

# ROC curve
library(pROC)
roc.lda <- plot.roc(test$Class, as.numeric(lda_predict))


#AUC
roc.1 <- roc(test$Class, as.numeric(lda_predict))
auc(roc.1)


# Quadratic Discriminant Analysis 

qda_fit <- qda(Class ~., data=train)
qda_fit 

# prediction 

qda_predict <- predict(qda_fit,test)$class
error_qda <- mean(qda_predict != test[,1])




# Naive Bayes Classification
library(e1071)

nb_fit <- naiveBayes(Class ~., data=train)

nb_fit

# prediction 

naive_predict <- predict(nb_fit,test)
error_nb <- mean(naive_predict != test[,1])

# confusion matrix 

table(naive_predict,test[,1])
library(caret)

expected <- factor(test[,1])
predicted <- factor(naive_predict)
results <- confusionMatrix(data=predicted, reference=expected)
print(results)

results$byClass

# ROC curve
library(pROC)
roc.nb <- plot.roc(test$Class, as.numeric(naive_predict))


#AUC
roc.1 <- roc(test$Class, as.numeric(naive_predict))
auc(roc.1)

# Support Vector Machine

set.seed(101)
library(e1071)

#support vector machine 
svm.fit <- svm(Class ~ ., data=train, kernel = "radial") 

svm.pred <- predict(svm.fit, newdata=test, outcome="test")

error_svm <- mean(svm.pred != test[,1])


# confusion matrix
library(caret)
svm.confu <- confusionMatrix(data=as.factor(svm.pred),reference = test$Class)
svm.confu$byClass

# ROC curve

library(pROC)
plot.roc(test$Class, as.numeric(svm.pred))

#AUC
roc.curve <- roc(test$Class, as.numeric(svm.pred))
auc(roc.curve)



# Random Forest
library(randomForest)
set.seed(101)

set.seed(101)
oob_error <- function(tree = 10000, mtr){
  mar.rf<- randomForest(Class ~., data=train, ntree=tree, mtry=mtr)
  error <- tail(mar.rf$err.rate,1)[,1]
  return(error)
}
x =seq(1:15)
y=c()

for (i in x) {
  p <- oob_error(tree = 10000, mtr =i)
  y=c(y,p)
  
}
lowest.x <- y[x]==min(y)

# mar 8 gives the lowest oob error 
mar_rf.8 <- randomForest(Class ~ ., data = train, ntree=10000, mtry=8)
importance <- mar_rf.8$importance
plot(importance)

rf_pred.8 <- predict(mar_rf.8, newdata=test)
error <- mean(rf_pred.8 != test[,1])

# confusion matrix
library(caret)
rf.confu <- confusionMatrix(data=as.factor(rf_pred.8),reference = test[,1])
rf.confu$byClass

# ROC curve 
library(pROC)
plot.roc(test$Class, as.numeric(rf_pred.8))

#AUC
roc.2 <- roc(test$Class, as.numeric(rf_pred.8))
auc(roc.2)


# Neural Network

require(neuralnet)

nn <- neuralnet(Class ~ ., data=train, hidden = 16, act.fct = "logistic", linear.output =FALSE)

plot(nn)

# prediction 
nn.predict <- predict(nn, test,type = "class")
nn.pred <-ifelse(nn.predict>=0.5,"Live","Die")
error <- mean(nn.pred[,2] != test[,1])




# confusion matrix
library(caret)
nn.confu <- confusionMatrix(data=as.factor(nn.pred[,2]),reference = test$Class)
nn.confu$byClass

# ROC curve 
library(pROC)
plot.roc(test$Class, as.numeric(nn.predict[,2]))

#AUC
roc.2 <- roc(test$Class, as.numeric(nn.predict[,2]))
auc(roc.2)



# classification tree 

library(tree)
tree.fit <- tree(Class ~., data=train)

summary(tree.fit)

# prediction

tree.pred <- predict(tree.fit, test, type = "class")
error.tree <- mean(tree.pred != test[,1])

# confusion matrix
library(caret)
tree.confu <- confusionMatrix(data=as.factor(tree.pred),reference = test$Class)
tree.confu$byClass

# ROC curve 
library(pROC)
plot.roc(test$Class, as.numeric(tree.pred))

#AUC
roc.2 <- roc(test$Class, as.numeric(tree.pred))
auc(roc.2)

