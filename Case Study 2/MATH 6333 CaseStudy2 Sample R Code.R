##This is just a sample R Codes to Perform the Step 1,2 and 3.
##Please modify this as your preference.
#Load any other necessary packages needed for the rest of the case study.

########## Clear the session
rm(list=ls())

########## Load workspace with train and valid data 
setwd("C:/Users/18133/OneDrive - The University of Texas-Rio Grande Valley (2)/Fall 2021/MATH 6333/MATH 6392 Materials/Case Studies/Case Study 2/gampost-master")
load("CaseStudy2.RData")


########## Load packages

install.packages("Information")
library(Information)


options(scipen=10)




######### Kill the weakest variables with IV
IV <- create_infotables(data=train, y="PURCHASE", ncore=2)
View(IV$Summary)

train_new <- train[,c(subset(IV$Summary, IV>0.05)$Variable, "PURCHASE")]
dim(train)

valid_new <- valid[,c(subset(IV$Summary, IV>0.05)$Variable, "PURCHASE")]
dim(valid)

######## Variable clustering
install.packages("ClustOfVar")
install.packages("reshape2")
install.packages("plyr")

library(ClustOfVar)
library(reshape2)
library(plyr)

tree <- hclustvar(train_new[,!(names(train_new)=="PURCHASE")])
nvars <- 20
part_init<-cutreevar(tree,nvars)$cluster
kmeans<-kmeansvar(X.quanti=train_new[,!(names(train_new)=="PURCHASE")],init=part_init)
clusters <- cbind.data.frame(melt(kmeans$cluster), row.names(melt(kmeans$cluster)))
names(clusters) <- c("Cluster", "Variable")
clusters <- join(clusters, IV$Summary, by="Variable", type="left")
clusters <- clusters[order(clusters$Cluster),]
clusters$Rank <- ave(-clusters$IV, clusters$Cluster, FUN=rank)
View(clusters)
variables <- as.character(subset(clusters, Rank==1)$Variable)
variables

mytraindata=train_new[,c("NEWPurchase", variables)] #This will help you to create a new train dataset with selected columns and the new response variable that you created in Step 4.

########## Random Forest
train$CPURCHASE <- ifelse(train$PURCHASE==1, 1, -1)
valid$CPURCHASE <- ifelse(valid$PURCHASE==1, 1, -1)
train$CPURCHASE <- as.factor(train$CPURCHASE)
valid$CPURCHASE <- as.factor(valid$CPURCHASE)

library(randomForest)
system.time(
  rf.grow <- randomForest(CPURCHASE ~ ., data=train[,c("CPURCHASE", variables)], ntree=100, seed=2015)
)
system.time(
  rf.pred <- predict(rf.grow, newdata=valid, outcome="train")
)
paste0("RF: ", AUC(valid$PURCHASE, rf.pred$predicted[,2])[1])

rf.grow$importance
