
"
Author: Md Salman Rahman
Course: MATH 6333 Statistical Learning
Course Instructor: Dr. Tamer Oraby
Case Study 1
"

# Solution 

###################### Multiple Regression Analysis ###############################

## reading the data set

library("readxl")
library(ggplot2)
tinnitus_data<- read_excel("C:/Users/User/OneDrive - The University of Texas-Rio Grande Valley/Course_video/Statistical Learning/MATH 6333 90L - SL - F21/Homework_salman/Case Study 1/CaseStudy1.xlsx")
tinni_data_frame<-as.data.frame(tinnitus_data)




### Step 1:  
# exploring the data set by performing some descriptive analysis 

#get means excluding the missing value 
sapply(tinni_data_frame, mean, na.rm=TRUE)

#summary 
summary(tinni_data_frame)



# histogram 
hist(tinni_data_frame$HHI_Score)
hist(tinni_data_frame$GAD)
hist(tinni_data_frame$PHQ)
hist(tinni_data_frame$ISI)
hist(tinni_data_frame$SWLS)
hist(tinni_data_frame$Hyperacusis)
hist(tinni_data_frame$CFQ)
hist(tinni_data_frame$Gender)
hist(tinni_data_frame$Age)
hist(tinni_data_frame$`Duration_of_tinnitus(years)`)
hist(tinni_data_frame$Pre_TFI_Score)
hist(tinni_data_frame$Post_TFI_Score)


# pie chart 
mytable<-table(tinni_data_frame$Gender) # 1 is Male and 2 is Female
pie(mytable)

# correlation analysis 
library(corrplot)


# correlation matrix 
corrplot(cor(tinni_data_frame[,3:14]),
         method = "number",
         type = "upper" # show only upper side
)

# scatterplot of several variable 
pairs(tinni_data_frame[,3:14])

# correlation tests for whole data set
library(Hmisc)
res <- rcorr(as.matrix(tinni_data_frame[,3:14])) # rcorr() accepts matrices only


# correlation test
library(correlation)

correlation::correlation(tinni_data_frame[,3:14],
                         include_factors = TRUE, method = "auto"
)


### Step 2 and 3: 
#checking the missing value in the Pre and Post TFI scores

table(is.na(tinni_data_frame))


# if there is missing value use data imputation with mean corresponding to the Pre or Post TFI score


# If there are any numerical measurement with missing values, we will use mean to impute the data
# and if there are any missing values for any categorical measurement, we will use mode to impute the data.


tinni_data_frame$Post_TFI_Score[is.na(tinni_data_frame$Post_TFI_Score)] <- mean(tinni_data_frame$Post_TFI_Score, na.rm = TRUE)




# after the data imputation please create a new variable called TFI_Reduction by subtracting 
# "Post_ TFI_Score" by "Pre_TFI_Score". 
# we will use the TFI_Reduction as the response in our multiple linear regression model 

tinni_data_frame$TFI_Reduction <- (tinni_data_frame$Post_TFI_Score - tinni_data_frame$Pre_TFI_Score)



#removing pre and post TFI score and subject ID

tinni_data_frame = subset(tinni_data_frame, select = -c(Subject_ID, Pre_TFI_Score,Post_TFI_Score) )


##baseline 



# replacing male and female with 1 and 0 respectively 

tinni_data_frame$Group[tinni_data_frame$Group == "Treatment"] <- 1
tinni_data_frame$Group[tinni_data_frame$Group == "Control"] <- 0

tinni_data_frame$Gender[tinni_data_frame$Gender == 1] <- 1
tinni_data_frame$Gender[tinni_data_frame$Gender == 2] <- 0

tinni_data_frame$Group <- as.numeric(tinni_data_frame$Group)

View(tinni_data_frame)
 
# converting the group and Gender into factor as they are categorical 
#tinni_data_frame$Gender <- as.factor(tinni_data_frame$Gender)
#contrasts(tinni_data_frame$Gender)

#Multicollinearity check

library(lattice)
splom(tinni_data_frame[,],pscales=0)
round(cor(tinni_data_frame[c(-1,-9)]),2)


## Step 4: 
# partitioning the data set (obtained at step 2) into training (80% of the data ) and a test set
# Use set.seed(123) and sample() in base package or createDatapartition() in caret package 

set.seed(123) # to make your partition reproducible

sample_size <- floor(0.80 * nrow(tinni_data_frame)) 

train_ind <- sample(seq_len(nrow(tinni_data_frame)), size = sample_size)

training <- tinni_data_frame[train_ind, ]
testing <- tinni_data_frame[-train_ind, ]

training <- as.data.frame(training)



### Step 5: 
# Performing the multiple regression analysis on the data using the TFI_Reduction of each subject
# as the response. There will be some comment on finding. 
# Tips: we can use best subset selection/forward/backward selection methods to select the best multiple 
# linear regression model with lm() in R


full_ls <- lm(TFI_Reduction ~ ., data=tinni_data_frame)
summary(full_ls)
plot(full_ls)

#removing outlier

tinni_d <-tinni_data_frame[-c(3,32,57,58),]
full_l <- lm(TFI_Reduction ~ ., data=tinni_data_frame)
summary(full_l)

# new model
full_l_2 <- lm(TFI_Reduction ~ Group + HHI_Score + ISI + SWLS, data=tinni_data_frame)
summary(full_l_2)

# new model
full_l_3 <- lm(TFI_Reduction ~ HHI_Score + ISI + SWLS +Gender , data=tinni_d)
summary(full_l_3)


# anova test of this two model 
anova(full_l,full_l_2)
fitted(full_l_2)
resid(full_l_2)

# anova for test 3
anova(full_l,full_l_3)


# model with some interaction 
full_l_4 <- lm(TFI_Reduction ~ HHI_Score*ISI*SWLS, data=tinni_d)
summary(full_l_4)



# poly interaction 
full_l_5 <- lm(TFI_Reduction ~ HHI_Score + ISI + SWLS + I(ISI^2)+ I(HHI_Score^2)+ ISI*HHI_Score, data=tinni_d)
summary(full_l_5)





#### subset selection

l_model_R <- lm(TFI_Reduction ~ ., data=training)
summary(l_model_R)

l_model_test <- lm(TFI_Reduction ~ ., data=testing)

mse_full <- mean((l_model_test$fitted.values-testing$TFI_Reduction)^2)

l_model_step_AIC <- step(l_model_R,direction="both",k=2) # for AIC

summary(l_model_step_AIC)

l_model_step_BIC <- step(l_model_R,direction="both",k=log(nrow(training))) # for BIC

summary(l_model_step_BIC)


#anova for AIC and BIC
l_model_step_AIC$anova
l_model_step_BIC$anova






# lars give k fold cross validation 

library(lars)

l_model_cv<-cv.lars(x=as.matrix(training[,-12]) , y = as.matrix(training[,12]),
               K= 10, plot.it=TRUE,se=TRUE,type="stepwise")


summary(l_model_cv)





### Step 6: 
# Once we select the best model with high prediction power (using multiple metrics like 
# Adjusted R2, AIC and BIC to select the best model), 
#perform the model diagnostics. If we see any 
# violations in the model assumptions, we will take 
#the appropriate actions to correct them. (For example
#, if we see a U shape pattern in the residual plot, try including the quadratic term, if you see any 
# potential influential point, create two regression models, both with and without that data point 
# to evaluate how the regression estimates and their standard errors get impacted). 
# There will be comment on what we saw and on the actions that we took to justify our approach.






#############
library(leaps)
#a <- regsubsets (TFI_Reduction~.,data=training, intercept=TRUE, method = "forward")
a <- regsubsets(x=training[,-12] , y = training[,12],
                 intercept = TRUE, method = "forward")
summ_a <- summary(a)
as.data.frame(summ_a$outmat)

nad <- which.max(summ_a$adjr2)
nrss<-which.min(summ_a$rss)

ncp<-which.min(summ_a$cp)

nbic<-which.min(summ_a$bic)


# getting the model 
adjr2 <- summ_a$which[nad,]

rss <- summ_a$which[nrss,]

cp <- summ_a$which[ncp,]

bic <- summ_a$which[nbic,]
cbind(adjr2,rss,cp,bic)


# coefficient of the object 
coef(a,ncp) # coefficient of best model 
vcov(a,ncp) 

plot(a,scale="Cp")

plot(a,scale="bic")

plot(a,scale="adjr2")

plot(a,scale="r2")





### Step 7: 
# After we clarify that there is no issue with the model assumptions, 
#we will use the model to find 
# out the factors which highly influence the reduction in TFI score. 
#There will be comment on our 
# findings. 


# final model 
#final_m <- lm(training$TFI_Reduction ~ training$HHI_Score+ training$ISI + training$PHQ + training$SWLS, data=training)
#summary(final_m)

# final model 
fin <- lm(training$TFI_Reduction ~ training$HHI_Score+ training$ISI  + training$SWLS, data=training)
summary(fin)
plot(fin)

#plot(final_m)
#coef(a,ncp) 


### Step 8:
# Now we will make prediction on the test data set. 
#There will be comment on mean square error on the
# testing data set. 

fin_test <- lm(testing$TFI_Reduction ~ testing$HHI_Score+ testing$ISI  + testing$SWLS, data=testing)
#pred <- predict(fin_test, testing[,1:11])

mse_l <- mean((fin_test$fitted.values-testing$TFI_Reduction)^2)



### Step 9:
# Now we will perform ridge regression, 
#lasso regression, principal component regression, and partial least 
# squares regression to do the same previous steps to decide 
#which multiple regression has the highest 
# predictive power via the smallest testing error 



# Ridge regression  (experiment)

library(glmnet)

x <- model.matrix(TFI_Reduction ~ ., data = training) [,-1]

y <- training$TFI_Reduction

mean(y)

list.lambda <- 10^seq(1,-2,length=100)

ridge.model <- glmnet(x, y, alpha=0, lambda=list.lambda, standardize = TRUE) 
#alpha =0 for ridge regression

ridge.model$lambda[10]

coef(ridge.model)[,10]  # coefficient respective to lambda [10]
dim(coef(ridge.model)[-1,])
penalty<-colSums(coef(ridge.model)[-1,])^2
plot(penalty)

predict(ridge.model,s=200, exact=T,type="coefficients",x=scale(x),y=y) # for lambda=200

predict(ridge.model,s=ridge.model$lambda[10], exact=T,type="coefficients",x=scale(x),y=y) # for lambda=200


predict(ridge.model,s=0, exact=T,type="coefficients",x=x,y=y) # same as original
lm(TFI_Reduction~.,data=training)$coef

## Scaling 
scale.x <-scale(x)
round(colMeans(scale.x))
apply(scale.x,2,sd)


lambda<-ridge.model$lambda[10]



#### effective degree of freedom function

df<-function(lambda,matrix=x){
  svd.x<-svd(matrix)
  D<-svd.x$d
  vdfL<-c()
  for (i in 1:length(lambda)){
    dfL<-D^2/(D^2+lambda[i])
    dfL<-sum(dfL)
    vdfL<-c(vdfL,dfL)
  }
  return(vdfL)
}


df(ridge.model$lambda[10],scale.x)

plot(list.lambda,df(list.lambda))




# training ridge (main part)


 # define earlier 

#train_in<-nrow(tinni_data_frame) %>% sample(.,0.8*.)

#train_in <- sample(nrow(tinni_data_frame), .8*nrow(tinni_data_frame))

#data_test <- tinni_data_frame[-train_in,] 

#rownames(data_test) = c()



train_in <- train_ind
data_test <- testing 
x <- model.matrix(TFI_Reduction ~ ., data = tinni_data_frame) [,-1]

y <- tinni_data_frame$TFI_Reduction


list.lambda <- 10^seq(1,-2,length=100)

#ridge.model<-glmnet(scale(x)[train_in,], y[train_in],alpha=0,lambda=list.lambda)

ridge.model<-glmnet(scale(x), y,alpha=0)

#optimal value of lambda
set.seed(123)
ridge.cv <- cv.glmnet(scale(x)[train_in,],y[train_in],alpha =0)
optimal_lam <- ridge.cv$lambda.min
optimal_second <- ridge.cv$lambda.1se

plot(ridge.cv)


#degree of freedom

df(ridge.cv$lambda.min,scale(x)[train_in,])
df(ridge.cv$lambda.1se,scale(x)[train_in,])


# prediction
#ridge.pred <-ridge.model %>% predict(s=optimal_lam, newx = x[-train_in,])

ridge.pred <-  predict(ridge.model, s=optimal_lam,
                       newx = scale(x)[-train_in,])

mse_ridge<- mean((ridge.pred - data_test$TFI_Reduction)^2)






# lasso 


# only difference will alpha =1


lasso.model<-glmnet(scale(x), y,alpha=1)

#optimal value of lambda
set.seed(123)
lasso.cv <- cv.glmnet(scale(x)[train_in,],y[train_in],alpha =1)
optimal_lam <- lasso.cv$lambda.min
optimal_second <- lasso.cv$lambda.1se

plot(lasso.cv)


#degree of freedom

df(lasso.cv$lambda.min,scale(x)[train_in,])
df(lasso.cv$lambda.1se,scale(x)[train_in,])


# prediction
#ridge.pred <-ridge.model %>% predict(s=optimal_lam, newx = x[-train_in,])

lasso.pred <-  predict(lasso.model, s=optimal_lam,
                       newx = scale(x)[-train_in,])

mse_lasso<- mean((lasso.pred - data_test$TFI_Reduction)^2)




# Principal component regression 

library(pls)

pcr.model <- pcr(TFI_Reduction ~ ., data=tinni_data_frame, 
                 scale =TRUE, validation ="CV")  # using 10 fold
summary(pcr.model)
validationplot(pcr.model,val.type = "MSEP")




## Train data
train <- sample(1:142,80)
pcr.model <- pcr(TFI_Reduction ~ ., data= tinni_data_frame, scale =TRUE, 
                 validation = "CV", subset = train)
validationplot(pcr.model, val.type = "MSEP")



# ## 


pcr.model <- pcr(TFI_Reduction ~ ., data=tinni_data_frame, 
                 scale =TRUE, ncomp=4)  # using 10 fold
summary(pcr.model)

pcr.model$projection
pcr.model$coefficients
pcr.model$loadings


pred.pcr<-predict(pcr.model,x[-train,],ncomp = 4)
mean((pred.pcr-y[-train])^2)

##################### PLS


pls.model<-plsr(TFI_Reduction ~ . , data = tinni_data_frame, 
                scale= TRUE,valiadtion = "CV", subset = train) # 10-fold
validationplot(pls.model,val.type = "MSEP")
summary(pls.model)

pred.pls<-predict(pls.model,x[-train,],ncomp = 2)
mean((pred.pls-y[-train])^2)
















######## K Means Regression #####################################



### Step 10: 
# Now we will use K means regression to train 
#several regression models after selecting the best k value, 
# using Monte-Carlo cross validation. 

#train_in<-nrow(tinni_data_frame) %>% sample(.,0.8*.)

#data_train <- tinni_data_frame[train_in,]

#data_test <- tinni_data_frame[-train_in,] 

#rownames(data_test) = c()

train_in <- train_ind
data_test <- testing
data_train <-training


library(class)
library(MASS)
library(FNN)
Choose_K<-function(K,M=800){
  MSE<-0
  for(i in 1:M){
    train<-sample(113,113-23) # for 5 fold
    KNN<-knn.reg(data_train[train,1:11], data_train[-train,1:11], data_train[train,12], k=K)
    MSE <- MSE + mean((KNN$pred - data_train[-train, 12])^2)
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



### Step 11: 
# We will make some prediction on the testing data set and obtain the mean square error. 

library(class)
KNN_t <- knn.reg(data_train[,1:11],data_test[,1:11],data_train[,12],
                 k=22) # with appropriate k =29

mse_knn<- mean((KNN_t$pred - data_test$TFI_Reduction)^2)







