"
Author: Md Salman Rahman
Course: MATH 6333 Statistical Learning
Course Instructor: Dr. Tamer Oraby
Mid Term
Problem 3
"

# Solution 

###################### Classification ###############################

## reading the data set

library("readxl")
library(ggplot2)

data <- iris  # Iris data is built in R


# summary 

summary(data)
dim(data)
pairs(data)
str(data)
cor(data[,-5])

ggplot(data,aes(x=Petal.Length,y=Sepal.Length))+
  geom_point(aes(color=Species))

# training and testing data
set.seed(123)
sample_size <- floor(0.80 * nrow(data)) 
train_ind <- sample(seq_len(nrow(data)), size = sample_size)
training <- data[train_ind, ]
testing <- data[-train_ind, ]
train <- as.data.frame(training)
test <- as.data.frame(testing)

attach(data)

# K nearest neighbor 

library(class)
Choose_K<-function(K,M=1000){
  MSE<-0
  for(i in 1:M){
    train_knn<-sample(150,150-30) # for 5-fold CV
    KNN<-knn(data[train_knn,1:4], data[-train_knn,1:4], data[train_knn,5],
             k=K,prob=TRUE,use.all=FALSE)
    MSE<-MSE+mean(KNN != data[-train_knn,5])
  }
  CVK<-MSE/M
  return(CVK)
}


library("purrr")

LK<-1:35 
vCVK<-LK %>%
  map(function(K) Choose_K(K))

vCVK<-unlist(vCVK)
vCVK<-as.data.frame(vCVK)
colnames(vCVK)<-c("CVK")

ggplot()+
  geom_point(data=vCVK,aes(x=LK,y=CVK))+
  xlab("K")+
  ylab("CV")

LK[which.min(vCVK$CVK)]

library(class)
KNN <- knn(training[,1:4],training[,1:4], training[,5],
           k=13, prob=TRUE,use.all=TRUE) 
           # with appropriate k = 13


# prediction using test data 

library(class)
knn_test <- knn(training[,1:4],testing[,1:4],training[,5],
                k=13, prob=TRUE,use.all = TRUE)

# error 

error <- mean(knn_test != test[,5])
error

#confusion matrix

a <- table(knn_test,test[,5])

library(caret)

expected <- factor(test[,5])
predicted <- factor(knn_test)
results <- confusionMatrix(data=predicted, reference=expected)
print(results)







# Multinomial regression

# firstly making virginica as baseline

library(dplyr)
train <- train %>% mutate(Species=relevel(Species,ref="virginica"))

library(nnet)
multino_model <- multinom(Species ~ 
Sepal.Length + Sepal.Width +
Petal.Length + Petal.Width, data=train)

summary(multino_model)

#coefficient 
coe <- summary(multino_model)
$coefficients/summary(multino_model)$standard.errors

p = (1 - pnorm(abs(coe), 0, 1)) * 2


# checking the interaction

interact_model <- multinom(Species ~ 
Sepal.Length + Sepal.Width +
                     Petal.Length + Petal.Width + Sepal.Length * Sepal.Width, data=train)

anova(multino_model,interact_model,test="Chisq")


# prediction and accuracy 

library(data.table)
pred <- predict(multino_model, newdata = test, "class")
value = data.table(pred)

error_multi <- mean((pred!=test[,5]))

#confusion matrix
table(pred,test[,5])

library(caret)

expected <- factor(test[,5])
predicted <- factor(pred)
results <- confusionMatrix(data=predicted, reference=expected)
print(results)





# L1-regularized multinomial regression

library(glmnet)

x <- model.matrix(Species ~ ., data = data) [,-1]

y <- data$Species

scale.x <- scale(x)

train_glm <- model.matrix(Species ~ ., data = train) [,-1]
train_scale_x <- scale(train_glm)
train_y <-train$Species

#optimal value of lambda
set.seed(123)
l1_fit_cv <- cv.glmnet(x,y,family= "multinomial",type.multinomial="ungrouped")
plot(l1_fit_cv)



#train
l1_multi_model <- glmnet(train_scale_x, train_y, family = "multinomial", type.multinomial = "ungrouped")

coef_l1_multi <- predict(l1_multi_model,s=l1_fit_cv$lambda.min , exact = F, type ="coefficients", train_scale_x, train_y)


# test 


l1_test <- predict(l1_multi_model, s= l1_fit_cv$lambda.min, 
newx=
scale(model.matrix(Species~.,data=test)[,-1]), type = "class")

#misclassification error 

mean(l1_test != test[,5])


# confusion matrix 

table(l1_test,test[,5])

library(AICcmodavg)

library(caret)

expected <- factor(test[,5])
predicted <- factor(l1_test)
results <- confusionMatrix(data=predicted, reference=expected)
print(results)



# Elastic-net multinomial regression

#glmnet

scale_x_elas <- scale(train[,1:4])
test_scale_x <- scale(test[,1:4])
q <- as.matrix(train[,5])

elastic_fit <- glmnet(alpha=0.5,scale_x_elas[,1:4],train[,5],
family="multinomial")
elas_cv_fit <- cv.glmnet(alpha =0.5, scale_x_elas[,1:4],q,family="multinomial",
type.multinomial="ungrouped")



# test 

elas_test <- predict(elas_cv_fit,
test_scale_x,s="lambda.min",type="class")

error_elas <- mean(elas_test !=test[,5] )


#confusion matrix 
table(elas_test,test[,5])

library(caret)

expected <- factor(test[,5])
predicted <- factor(elas_test)
results <- confusionMatrix(data=predicted, reference=expected)
print(results)






# Linear Discriminant Analysis

library(MASS)

lda_fit <- lda(Species ~ Sepal.Length + Sepal.Width 
               + Petal.Length + Petal.Width, data=train)
lda_fit

plot(lda_fit, col = as.integer(train$Species))

library(klaR)

partimat(Species ~ ., data=train, method="lda")

# prediction 

lda_predict <- predict(lda_fit,test)$class
error_lda <- mean(lda_predict != test[,5])

# confusion matrix 

table(lda_predict,test[,5])

library(caret)

expected <- factor(test[,5])
predicted <- factor(lda_predict)
results <- confusionMatrix(data=predicted, reference=expected)
print(results)

### train lda(another way)


lda_train <- predict(lda_fit)
train$lda <- lda_train$class
table(train$lda,train$Species)


## test lda(another way)

lda_test <- predict(lda_fit,test)
test$lda <- lda_test$class
table(test$lda,test$Species)


# Quadratic Discriminant Analysis 

qda_fit <- qda(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, train)
qda_fit 

# prediction 

qda_predict <- predict(qda_fit,test)$class
error_qda <- mean(qda_predict != test[,5])

# confusion matrix 

table(qda_predict,test[,5])

library(caret)

expected <- factor(test[,5])
predicted <- factor(qda_predict)
results <- confusionMatrix(data=predicted, reference=expected)
print(results)

#train (another way)
qda_train <- predict(qda_fit)
train$qda <- qda_train$class
table(train$qda,train$Species)

# test (another way)
qda_test <- predict(qda_fit,test)
test$qda <- qda_test$class
table(test$qda,test$Species)

# Naive Bayes Classification with Gaussian Conditional Distribution of Inputs 
library(e1071)

nb_fit <- naiveBayes(Species ~ . ,  data=train)

nb_fit

# prediction 

naive_predict <- predict(nb_fit,test)
error_nb <- mean(naive_predict != test[,5])

# confusion matrix 

table(naive_predict,test[,5])
library(caret)

expected <- factor(test[,5])
predicted <- factor(naive_predict)
results <- confusionMatrix(data=predicted, reference=expected)
print(results)








