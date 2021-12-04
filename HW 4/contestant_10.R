
"
Problem 1
Contestant 8: Neural Network
Author: Md Salman Rahman
Course: MATH 6333 Statistical Learning
Course Instructor: Dr. Tamer Oraby
"

# Solution 

setwd("/Users/salman/OneDrive - The University of Texas-Rio Grande Valley/Course_video/Statistical Learning/Homework_salman/HW 4")


# Task: Import the attached training_data.xlsx into R
library("readxl")
library(ggplot2)
data_training<- read_excel("training_data.xlsx")
data_train<-as.data.frame(data_training)
data_train$Y = as.factor(data_train$Y)

# Task: Taking the role of contestant 4 who trains the Neural Network using that data.



# Method: vanilla model (using keras)
# library(keras)
# x <- scale(model.matrix(Y~cx+cy, data=data_train))
# nn.fit <- keras_model_sequential() %>% layer_dense(units = 3, activation = "relu", 
#                                                    input_shape = ncol(x)) %>% layer_dropout(rate =0.4) %>% layer_dense(units = 1)

# Method using neuralnet
source("/Users/salman/OneDrive - The University of Texas-Rio Grande Valley/Course_video/Statistical Learning/Homework_salman/HW 4/contestant_1.R")

require(neuralnet)

nn <- neuralnet(Y~cx+cy, data =data_train, hidden =5, act.fct = "logistic", linear.output =FALSE)
 
#plot(nn)


pred.nn <- ifelse(compute(nn,DF_M)$net.result>0.5,1,0)
new_M.nn<-cbind.data.frame(DF_M,pred.nn)
pred.num <- new_M.nn[,3]

p15=p3+
  geom_point(data=new_M.nn,aes(x= cx, y=cy ),colour=ifelse(new_M.nn[,3]==1,"orange","blue"),shape=23, alpha =0.10)
p15

p16 <- p15 + geom_contour(data=new_M.nn,aes(x=cx,y=cy,z=as.numeric(pred.num)),
                          breaks = c(0.8), lwd = 0.5, colour ="red")+
  ggtitle("Neural Network Decision Boundary with 5 Hidden Neuron")

p16
