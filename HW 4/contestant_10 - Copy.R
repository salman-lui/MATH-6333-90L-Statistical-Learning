
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
data_training<- read_excel("C:/Users/User/OneDrive - The University of Texas-Rio Grande Valley/Course_video/Statistical Learning/Homework_salman/HW 4/training_data.xlsx")
data_train<-as.data.frame(data_training)
data_train$Y = as.factor(data_train$Y)

# Task: Taking the role of contestant 4 who trains the Neural Network using that data.



# Method: vanilla model
library(keras)
x <- scale(model.matrix(Y~cx+cy, data=data_train))
nn.fit <- keras_model_sequential() %>% layer_dense(units = 3, activation = "relu", 
                                                   input_shape = ncol(x)) %>% layer_dropout(rate =0.4) %>% layer_dense(units = 1)

nn.fit
summary(nn.fit)