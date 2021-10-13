
"
Problem 2
Author: Md Salman Rahman
Course: MATH 6333 Statistical Learning
Course Instructor: Dr. Tamer Oraby
Homework: This problem is based on your understanding of the work done in Lab 1(Three parts) -
R.R and similar data to that was generated there. You will produce four files of R codes,
each of which is saved after the name of the role played, e.g., contestant 1.R.
In this problem, you will play four different roles: contestant 1, contestant 2, contestant 3
and the judge. Do the following:
1) Import the attached training_data.xlsx into R.
2) Take the role of contestant 1 who trains the linear regression using that data.
3) Take the role of contestant 2 who trains the K-nearest neighbor using that data
and chooses the appropriate K using Monte-Carlo cross-validation.
4) Take the role of contestant 3 who trains the one-nearest neighbor using that
data.
5) Import the attached testing_data.xlsx into R.
6) Take the role of the judge and test the three supervised learning methods for
classification using the testing error function defined via the 0-1 Loss.
7) Who does win the contest?
"

# Solution 


# Task: Import the attached training_data.xlsx into R
library("readxl")
library(ggplot2)
data_training<- read_excel("C:/Users/User/OneDrive - The University of Texas-Rio Grande Valley/Course_video/Statistical Learning/MATH 6333 90L - SL - F21/Homework_salman/HW 2/training_data.xlsx")
data_train<-as.data.frame(data_training)



# Task: Taking the role of contestant 2 who trains the K-nearest neighbor using that data
#         and chooses the appropriate K using Monte-Carlo cross-validation


# choosing the appropriate k using monte carlo cross validation
Choose_K<-function(K,M=800){
  MSE<-0
  for(i in 1:M){
    train<-sample(200,200-40) # for 5-fold CV
    KNN<-knn(data_train[train,1:2], data_train[-train,1:2], data_train[train,3],
             k=K,prob=TRUE,use.all=FALSE)
    MSE<-MSE+mean(KNN != data_train[-train,3])
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
KNN <- knn(data_train[,1:2],data_train[,1:2],data_train[,3],
           k=7, prob=TRUE,use.all=TRUE) # with appropriate k =7


