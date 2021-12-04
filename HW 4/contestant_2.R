
"
Problem 2
Author: Md Salman Rahman
Course: MATH 6333 Statistical Learning
Contestant 2

"

# Solution 


# Task: Import the attached training_data.xlsx into R
setwd("/Users/salman/OneDrive - The University of Texas-Rio Grande Valley/Course_video/Statistical Learning/Homework_salman/HW 4")
# Task 1: Import the attached training_data.xlsx into R
library("readxl")
library(ggplot2)
data_training<- read_excel("training_data.xlsx")
data_train<-as.data.frame(data_training)

# Task: Taking the role of contestant 2 who trains the K-nearest neighbor using that data
#         and chooses the appropriate K using Monte-Carlo cross-validation


# choosing the appropriate k using monte carlo cross validation

library(class)
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


source("/Users/salman/OneDrive - The University of Texas-Rio Grande Valley/Course_video/Statistical Learning/Homework_salman/HW 4/contestant_1.R")

KNN2 <- knn(data_train[,1:2],DF_M,data_train[,3],
           k=7, prob=TRUE,use.all=TRUE) 

new_M_KNN<-cbind.data.frame(DF_M,KNN2)

p6=p3+geom_point(data=new_M_KNN[,1:2],aes(x=cx,y=cy),
                 colour= ifelse( new_M_KNN[ , 3]==1,"orange","blue"), alpha=.01)

p6

pr1<-attr(KNN2,"prob")
pr2<-ifelse(KNN2=="1",1-pr1,pr1)
DF_M_KNN<-cbind(DF_M,pr2)

p7=p6+
  geom_contour(data=DF_M_KNN,aes(x=cx,y=cy,z=pr2),
               binwidth=0.51,lwd=.5,colour="black")+
  ggtitle("Decision Boundary for K-Nearest Neighbor")
p7


#ggsave("p7.png",plot=p7)

library(png)
img<-readPNG("p7.png")
grid::grid.raster(img)