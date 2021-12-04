
"
Problem 1
Contestant 5: Linear discriminant analysis (LDA)
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
data_training<- read_excel("training_data.xlsx")
data_train<-as.data.frame(data_training)


# Task: Taking the role of contestant 4 who trains the Linear discriminant analysis (LDA) using that data.

# Method Linear discriminant analysis (LDA)

library(MASS)
attach(data_train)
lda.fit <- lda(Y ~ cx + cy, data = data_train)
summary(lda.fit)
plot(lda.fit, col = Y)

library("plyr")
library("dplyr")
# decision boundary 
library("grid")
library("gridExtra")
library("caret")
library(raster)
library(rgdal)
library(classInt)
library(RColorBrewer)
attach(data_train)
data_train$Y = as.factor(data_train$Y)

color <- brewer.pal(3,'Set1')[1:2]

ggplot(data=data_train, aes(x = cx, y=cy))+
  geom_point(aes(color=Y), size =4, alpha = 0.8)+
  scale_color_manual(name='Y',values = color)

data.ml.fit <- mutate(data_train, lda_prob = predict(lda.fit, data=data_train)$posterior[,1], 
                      lda_pred=predict(lda.fit, data=data_train)$posterior[,2])
# decision region

grid.point <- 250
x1 <- seq(min(data_train$cx), max(data_train$cx),length = grid.point)
x2 <- seq(min(data_train$cy), max(data_train$cy),length = grid.point)
grid <- expand.grid(cx =x1, cy=x2)
c.plot = function(grid,fit,title){
region = ggplot(data = data.ml.fit, aes(x =cx, y=cy, color =Y))+
  geom_tile(data = cbind(grid, Y = predict(fit,grid)$class), aes (fill =Y))+
  scale_fill_manual(name = 'Y', values = color) +
  ggtitle("Decision region") + theme(legend.text = element_text(size = 10)) +
  scale_colour_manual(name = 'Y', values = color)

show(region)


boundary = ggplot(data = data_train, aes(x = cx, y = cy,
                                       color = Y)) +
  geom_contour(data = cbind(grid, Y = predict(fit,grid)$class),
               aes(z = as.numeric(Y)), color = "red", breaks = c(1.5)) +
  geom_point(size = 4, alpha = .5) +
  ggtitle("Decision boundary") +
  theme(legend.text = element_text(size = 10)) +
  scale_colour_manual(name = 'Y', values = color)
show(boundary)


grid.arrange(region, boundary, ncol =2, top = textGrob(title, 
                                                       gp = gpar(fontsize =20)))
}


c.plot(grid,lda.fit,"Linear discriminant analysis (LDA)")


library("caret")
Errs = data.frame()
Method = "lda"
Name = "LDA"
Formula = "nb"
Model = train(as.formula(Formula), data = data_train, method = Method, trControl = TrControl, metric = "ROC")
c.plot(grid, Model ,Name)


Errs <- rbind(Errs,cbind(Model$resample,rep(Name,40)))
Method = "knn"
Name = "kNN"
Formula = "classes~."
Model = train(as.formula(Formula), data = twoClass, method = Method, trControl = TrControl, metric = "ROC")
create_plots(Grid, Model ,Name)


