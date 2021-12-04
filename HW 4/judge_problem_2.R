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

# contestant 1
source("C:/Users/User/OneDrive - The University of Texas-Rio Grande Valley/Course_video/Statistical Learning/MATH 6333 90L - SL - F21/Homework_salman/HW 2/contestant 1_problem_2.R")

# contestant 2

source("C:/Users/User/OneDrive - The University of Texas-Rio Grande Valley/Course_video/Statistical Learning/MATH 6333 90L - SL - F21/Homework_salman/HW 2/contestant 2_problem_2.R")

#contestant 3

source("C:/Users/User/OneDrive - The University of Texas-Rio Grande Valley/Course_video/Statistical Learning/MATH 6333 90L - SL - F21/Homework_salman/HW 2/contestant 3_problem_3.R")


#Judge 

# Import the attached testing_data.xlsx into R.

library("readxl")
data_testing<- read_excel("C:/Users/User/OneDrive - The University of Texas-Rio Grande Valley/Course_video/Statistical Learning/MATH 6333 90L - SL - F21/Homework_salman/HW 2/testing_data.xlsx")

data_test<-as.data.frame(data_testing)

mse <- (((data_test)-testing[,12])^2)/12
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


#judge comment 


library(ggplot2)

com<-data.frame(
  name=c("Contestant1","Contestant2","Contestant3") ,  
  error=c(error_con_1,error_con_2,error_con_3)
)

ggplot(com, aes(x=name, y=error)) + 
  geom_bar(stat = "identity",width=0.2)


# Task 7, from the ggplot figure we can see that there is tie between linear regression
# and one nearest neighbor having the lowest error (0.1), I will declare both contestant 1 and
# contestant 3 as the winner. Anyway, from my personal point of view I prefer linear regression 
# than one nearest neighbor as linear regression is more general, versatile, and less of a black box(high interpretability) 
# than one nearest neighbor. 



