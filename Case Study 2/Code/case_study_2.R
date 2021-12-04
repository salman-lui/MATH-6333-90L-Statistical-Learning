
"
Author: Md Salman Rahman
Course: MATH 6333 Statistical Learning
Course Instructor: Dr. Tamer Oraby
Case Study 2
Goal: The main goal of the case study is to gets hands on experience in applying 
the logistic regression, random forest, and Support Vector Machines to solve a classification 
problem
"

# Solution 

setwd("/Users/salman/OneDrive - The University of Texas-Rio Grande Valley/Course_video/Statistical Learning/Homework_salman/Case Study 2")

## ### reading the data set

library("readxl")
library(ggplot2)
mar_data <- load("CaseStudy2.RData")
# checking the dimension of the data
dim(train)
dim(valid)

# checking missing value

table(is.na(train))
table(is.na(valid))


############  
# exploring the data set by performing some descriptive analysis 
#get means
sapply(train, mean, na.rm=TRUE)

#summary 
summary(train)


# histogram 
hist(train$TOT_HI_CRDT_CRDT_LMT)
hist(train$N_OF_SATISFY_FNC_REV_ACTS)


# pie chart 
mytable<-table(train$PURCHASE) # 1 is Male and 2 is Female
pie(mytable)

# correlation analysis 
library(corrplot)


# correlation matrixs
corrplot(cor(train[,1:69]),
         method = "number",
         type = "upper" # show only upper side
)

# scatterplot of several variable 
pairs(train[,3:14])



### Step 2 Information Value #############
library(Information)
options(scipen=10)

########## Killing the weakest variables with IV
IV <- create_infotables(data=train, y="PURCHASE", ncore=2)
View(IV$Summary)

# only keeping IV value with > 0.5
train_new <- train[,c(subset(IV$Summary,IV>0.05)$Variable,"PURCHASE")]
dim(train_new)

valid_new <- valid[,c(subset(IV$Summary,IV>0.05)$Variable,"PURCHASE")]
dim(valid_new)





## Step 3: Multicollinearity check 

library(ClustOfVar)
library(reshape2)
library(plyr)

tree <- hclustvar(train_new[,!(names(train_new)=="PURCHASE")])
nvars <- 20
part_init <- cutreevar(tree,nvars)$cluster
kmeans<-kmeansvar(X.quanti=train_new[,!(names(train_new)=="PURCHASE")],init=part_init)
clusters <- cbind.data.frame(melt(kmeans$cluster), row.names(melt(kmeans$cluster)))
names(clusters) <- c("Cluster", "Variable")
clusters <- join(clusters, IV$Summary, by="Variable", type="left")
clusters <- clusters[order(clusters$Cluster),]
clusters$Rank <- ave(-clusters$IV, clusters$Cluster, FUN=rank)
View(clusters)
variables <- as.character(subset(clusters, Rank==1)$Variable)
variables



## Step 4: Adding a new response variable called Newpurchase

train_new$Newpurchase <- ifelse(train_new$PURCHASE==1,1,-1)

valid_new$Newpurchase <- ifelse(valid_new$PURCHASE==1,1,-1)


train_new$Newpurchase <- as.factor(train_new$Newpurchase)
valid_new$Newpurchase <- as.factor(valid_new$Newpurchase)


## Step 5: logistic regression model
glm.fit.case <- glm(Newpurchase ~., data=train_new[,c("Newpurchase", variables)], family = "binomial")
summary(glm.fit.case)
coef(glm.fit.case)

# prediction
logis.pred <- predict(glm.fit.case, newdata=valid_new[,c("Newpurchase", variables)], type = "response")

logis.pred.1 <-ifelse(logis.pred>=0.5,1,-1)

# confusion matrix
library(caret)
logis.confu <- confusionMatrix(data=as.factor(logis.pred.1),reference = valid_new$Newpurchase)
# ROC curve

library(pROC)
roc.logis <- plot.roc(valid_new$Newpurchase, as.numeric(logis.pred.1))

#AUC
roc.1 <- roc(valid_new$Newpurchase, as.numeric(logis.pred.1))
auc(roc.1)

#ggsave("ROC_RF.png",roc.logis)



## Step 6: Random forest model
train_new$Newpurchase <- as.factor(train_new$Newpurchase)
valid_new$Newpurchase <- as.factor(valid_new$Newpurchase)

library(randomForest)
set.seed(101)
#with mtry = 1
mar_rf.1 <- randomForest(Newpurchase ~., data=train_new[,c("Newpurchase", variables)], ntree=10001, mtry=1, seed=2015)
mar_rf.1$importance

rf_pred.1 <- predict(mar_rf.1, newdata=valid_new, outcome="train")

library(ROCR)
paste0("RF: ", AUC(valid_new$Newpurchase, rf_pred.1$predicted[,2])[1])

#with mtry = 2
mar_rf.2 <- randomForest(Newpurchase ~., data=train_new[,c("Newpurchase", variables)], ntree=10001, mtry=2, seed=2015)
mar_rf.2$importance

rf_pred.2 <- predict(mar_rf.2, newdata=valid_new, outcome="train")


#with mtry = 3
mar_rf.3 <- randomForest(Newpurchase ~., data=train_new[,c("Newpurchase", variables)], ntree=10001, mtry=3, seed=2015)
mar_rf.3$importance

rf_pred.3 <- predict(mar_rf.3, newdata=valid_new, outcome="train")

#with mtry = 4
mar_rf.4 <- randomForest(Newpurchase ~., data=train_new[,c("Newpurchase", variables)], ntree=10001, mtry=4, seed=2015)
mar_rf.4$importance

rf_pred.4 <- predict(mar_rf.4, newdata=valid_new[,c("Newpurchase", variables)], outcome="test")
library(pROC)
plot.roc(valid_new[,c("Newpurchase", variables)]$Newpurchase, as.numeric(rf_pred.4))


# #with mtry = 5
# mar_rf.5 <- randomForest(Newpurchase ~., data=train_new[,c("Newpurchase", variables)], ntree=10001, mtry=5, seed=2015)
# mar_rf.5$importance
# 
# rf_pred.5 <- predict(mar_rf, newdata=valid_new, outcome="train")
# 
# #with mtry = 6
# mar_rf.6 <- randomForest(Newpurchase ~., data=train_new[,c("Newpurchase", variables)], ntree=10001, mtry=6, seed=2015)
# mar_rf$importance
# 
# rf_pred.6 <- predict(mar_rf.6, newdata=valid_new, outcome="train")
# 
# #with mtry = 7
# mar_rf.7 <- randomForest(Newpurchase ~., data=train_new[,c("Newpurchase", variables)], ntree=10001, mtry=7, seed=2015)
# mar_rf.7$importance
# 
# rf_pred.7 <- predict(mar_rf.7, newdata=valid_new, outcome="train")
# 
# #with mtry = 8
# mar_rf.1 <- randomForest(Newpurchase ~., data=train_new[,c("Newpurchase", variables)], ntree=10001, mtry=8, seed=2015)
# mar_rf.8$importance
# 
# rf_pred.8 <- predict(mar_rf.8, newdata=valid_new, outcome="train")
# 
# 
# #with mtry = 9
# mar_rf.9 <- randomForest(Newpurchase ~., data=train_new[,c("Newpurchase", variables)], ntree=10001, mtry=9, seed=2015)
# mar_rf.9$importance
# 
# rf_pred.9 <- predict(mar_rf.9, newdata=valid_new, outcome="train")
# 
# #with mtry = 10
# mar_rf.10 <- randomForest(Newpurchase ~., data=train_new[,c("Newpurchase", variables)], ntree=10001, mtry=10, seed=2015)
# mar_rf.10$importance
# 
# rf_pred.10 <- predict(mar_rf.10, newdata=valid_new, outcome="train")
# 
# #with mtry = 11
# mar_rf.11 <- randomForest(Newpurchase ~., data=train_new[,c("Newpurchase", variables)], ntree=10001, mtry=11, seed=2015)
# mar_rf.11$importance
# 
# rf_pred.11 <- predict(mar_rf.11, newdata=valid_new, outcome="train")
# 
# #with mtry = 12
# mar_rf.12 <- randomForest(Newpurchase ~., data=train_new[,c("Newpurchase", variables)], ntree=10001, mtry=12, seed=2015)
# mar_rf.12$importance
# 
# rf_pred.12 <- predict(mar_rf.12, newdata=valid_new, outcome="train")
# 
# #with mtry = 13
# mar_rf.13 <- randomForest(Newpurchase ~., data=train_new[,c("Newpurchase", variables)], ntree=10001, mtry=13, seed=2015)
# mar_rf.13$importance
# 
# rf_pred.13 <- predict(mar_rf.13, newdata=valid_new, outcome="train")



set.seed(101)
oob_error <- function(tree =10001, mtr){
  mar.rf<- randomForest(Newpurchase ~., data=train_new[,c("Newpurchase", variables)], ntree=tree, mtry=mtr)
  error <- tail(mar.rf$err.rate,1)[,1]
  return(error)
}
x =seq(1:13)
y=c()

for (i in x) {
  p <- oob_error(tree = 10001, mtr =i)
  y=c(y,p)
  
}
lowest.x <- y[x]==min(y)


#with mtry = 9
mar_rf.9 <- randomForest(Newpurchase ~., data=train_new[,c("Newpurchase", variables)], ntree=10001, mtry=9, seed=2015)
importance <- mar_rf.9$importance
plot(importance)

rf_pred.9 <- predict(mar_rf.9, newdata=valid_new[,c("Newpurchase", variables)], outcome="test")


# confusion matrix
library(caret)
rf.confu <- confusionMatrix(data=as.factor(rf_pred.9),reference = valid_new$Newpurchase)


# ROC curve 
library(pROC)
plot.roc(valid_new$Newpurchase, as.numeric(rf_pred.9))

#AUC
roc.2 <- roc(valid_new$Newpurchase, as.numeric(rf_pred.9))
auc(roc.2)

## Step 7: SVM classification models

set.seed(101)
library(e1071)

#support vector classifier 
svm.fit.1 <- svm(Newpurchase ~ ., data=train_new[,c("Newpurchase", variables)], kernel = "polynomial", degree = 3, probability = TRUE) 

svm.pred <- predict(svm.fit.1, newdata=valid_new[,c("Newpurchase", variables)], outcome="test")

#support vector machine 
svm.fit.2 <- svm(Newpurchase ~ ., data=train_new[,c("Newpurchase", variables)], kernel = "radial", gamma = 0.000001, cost = 0.01) 

svm.pred.2 <- predict(svm.fit.2, newdata=valid_new[,c("Newpurchase", variables)], outcome="test")

#testing error(all four method) 
df.case <- valid_new[,c("Newpurchase", variables)]

error.2 <- mean(logis.pred.1 != df.case$Newpurchase) #logistic
error.3 <- mean(rf_pred.9 != df.case$Newpurchase) # rf
error.4 <- mean(svm.pred != df.case$Newpurchase) #SVM 1
error.5 <- mean(svm.pred.2 != df.case$Newpurchase) #SVM 2



# confusion matrix
library(caret)
svm.confu.1 <- confusionMatrix(data=as.factor(svm.pred),reference = valid_new$Newpurchase)
svm.confu.2 <- confusionMatrix(data=as.factor(svm.pred.2),reference = valid_new$Newpurchase)
# ROC curve

library(ROCR)
plot.roc(valid_new$Newpurchase, as.numeric(svm.pred))
plot.roc(valid_new$Newpurchase, as.numeric(svm.pred.2))

#AUC -model 1
roc.3 <- roc(valid_new$Newpurchase, as.numeric(svm.pred))
auc(roc.3)

#AUC-model 2
roc.4 <- roc(valid_new$Newpurchase, as.numeric(svm.pred.2))
auc(roc.2)



