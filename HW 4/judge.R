"
Problem 1
Author: Md Salman Rahman
Course: MATH 6333 Statistical Learning
Course Instructor: Dr. Tamer Oraby
Judge Role
"
# Solution 

setwd("/Users/salman/OneDrive - The University of Texas-Rio Grande Valley/Course_video/Statistical Learning/Homework_salman/HW 4")

# contestant 1
source("contestant_1.R")

# contestant 2

source("contestant_2.R")

#contestant 3

source("contestant_3.R")



#contestant 4

source("contestant_4.R")


#contestant 5

source("contestant_5.R")


#contestant 6

source("contestant_6.R")


#contestant 7

source("contestant_7.R")



#contestant 8

source("contestant_8.R")



#contestant 9

source("contestant_9.R")



#contestant 10

source("contestant_10.R")




#Judge 

# Import the attached testing_data.xlsx into R.

library("readxl")
data_testing<- read_excel("testing_data.xlsx")

data_test<-as.data.frame(data_testing)


# Judge role

#testing contestant 1

cont_1_predi <- predict(l_regression , data_test[,1:2])

error_con_1 <- mean((cont_1_predi>0.5) != data_test[,3])
error_con_1

#testing contestant 2

library(class)
cont_2_knn <- knn(data_train[,1:2],data_test[,1:2],data_train[,3],
           k=7, prob=TRUE)

error_con_2 <- mean(cont_2_knn != data_test[,3])

error_con_2

#testing contestant 3

cont_3_knn <- knn(data_train[,1:2],data_test[,1:2],data_train[,3],
                k=1, prob=TRUE)

error_con_3 <- mean(cont_3_knn != data_test[,3])

error_con_3

#testing contestant 4
cont_4_predi <- predict(glm.fit , data_test[,1:2])

logis.4.pred <-ifelse(cont_4_predi>=0.5,1,0)

error_con_4 <- mean(logis.4.pred != data_test[,3])
error_con_4

#testing contestant 5
lda.predict <- predict(lda.fit, data_test)$class

error_con_5 <- mean(lda.predict != data_test[,3])


#testing contestant 6
qda.predict <- predict(qda.fit, data_test)$class
error_con_6 <- mean(qda.predict != data_test[,3])

#testing contestant 7

naive.predict <- predict(naive.fit, data_test)

error_con_7 <- mean(naive.predict != data_test[,3])

#testing contestant 8

svm.predict <- predict(svm.fit, data_test)

error_con_8 <- mean(svm.predict != data_test[,3])

#testing contestant 9
rf.predict <- predict(ran.for.fit, data_test)

error_con_9 <- mean(rf.predict != data_test[,3])

#testing contestant 10
nn.predict <- predict(nn, data_test)
nn.pred <-ifelse(nn.predict>=0.5,1,0)
error_con_10 <- mean(nn.pred != data_test[,3])

#judge comment 


library(ggplot2)

com <- data.frame(
  contestant=c("C1","C2","C3", "C4", "C5", "C6", "C7", "C8", "C9", "C10") ,  
  error=c(error_con_1,error_con_2,error_con_3,error_con_4, error_con_5, error_con_6,error_con_7,error_con_8,error_con_9, error_con_10)
)

ggplot(com, aes(x=contestant, y=error)) + 
  geom_bar(stat = "identity",width=0.1)




