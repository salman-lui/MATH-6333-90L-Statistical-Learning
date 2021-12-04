
"
Problem 1
Contestant 6: Quadratic discriminant analysis (QDA)
Author: Md Salman Rahman
Course: MATH 6333 Statistical Learning
Course Instructor: Dr. Tamer Oraby
"

# Solution 

setwd("/Users/salman/OneDrive - The University of Texas-Rio Grande Valley/Course_video/Statistical Learning/Homework_salman/HW 4")


# Task: Import the attached training_data.xlsx into R
library("readxl")
library(ggplot2)
library("plyr")
library("dplyr")
library("grid")
library("gridExtra")
library("caret")
library(raster)
library(rgdal)
library(classInt)
library(RColorBrewer)
data_training<- read_excel("training_data.xlsx")
data_train<-as.data.frame(data_training)


# Task: Taking the role of contestant 4 who trains the Quadratic discriminant analysis (QDA) using that data.

library(MASS)

# Method: Quadratic discriminant analysis (QDA)
attach(data_train)
qda.fit <- qda(Y ~ cx + cy, data = data_train)

qda.fit


library("grid")
library("gridExtra")
library("caret")
attach(data_train)
data_train$Y = as.factor(data_train$Y)

color <- brewer.pal(3,'Set1')[1:2]

ggplot(data=data_train, aes(x = cx, y=cy))+
  geom_point(aes(color=Y), size =4, alpha = 0.8)+
  scale_color_manual(name='Y',values = color)

data.ml.fit <- mutate(data_train, qda_prob = predict(qda.fit, data=data_train)$posterior[,1], 
                      qda_pred=predict(qda.fit, data=data_train)$posterior[,2])
# decision region

grid.point <- 250
x1 <- seq(min(data_train$cx), max(data_train$cx),length = grid.point)
x2 <- seq(min(data_train$cy), max(data_train$cy),length = grid.point)
grid <- expand.grid(cx =x1, cy=x2)
region = ggplot(data = data.ml.fit, aes(x =cx, y=cy, color =Y))+
  geom_tile(data = cbind(grid, Y = predict(qda.fit,grid)$class), aes (fill =Y))+
  scale_fill_manual(name = 'Y', values = color) +
  ggtitle("Decision region") + theme(legend.text = element_text(size = 10)) +
  scale_colour_manual(name = 'Y', values = color)

show(region)


boundary.qda = ggplot(data = data_train, aes(x = cx, y = cy,
                                         color = Y)) +
  geom_contour(data = cbind(grid, Y = predict(qda.fit,grid)$class),
               aes(z = as.numeric(Y)), color = "red", breaks = c(1.5)) +
  geom_point(size = 4, alpha = .5) +
  ggtitle("Decision boundary") +
  theme(legend.text = element_text(size = 10)) +
  scale_colour_manual(name = 'Y', values = color)
show(boundary.qda)


grid.arrange(region, boundary.qda, ncol =2, top = textGrob("Quadratic discriminant analysis (QDA)", 
                                                       gp = gpar(fontsize =20)))








