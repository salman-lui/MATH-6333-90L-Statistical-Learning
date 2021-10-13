
"
Problem 3
Author: Md Salman Rahman
Course: MATH 6333 Statistical Learning
Course Instructor: Dr. Tamer Oraby
Homework: Problem 13 of ISL book 
In this exercise you will create some simulated data and will fit simple
linear regression models to it. Make sure to use set.seed(1) prior to
starting part (a) to ensure consistent results.
"


#### (a) Using the rnorm() function, create a vector, x, containing 100
#        observations drawn from a N(0, 1) distribution. This represents
#        a feature, X ###

# Solution of (a)

set.seed(1) 
x <- rnorm(100)


#### (b) Using the rnorm() function, create a vector, eps, containing 100
#        observations drawn from a N(0, 0.25) distribution-a normal
#        distribution with mean zero and variance 0.25

# Solution of (b)

eps <- rnorm(n=100, mean = 0, sqrt(0.25))




#### (c) Using x and eps, generate a vector y according to the model
#        Y = -1 + 0.5X + epsilon
#        What is the length of the vector y? What are the values of beta_0 and beta_1 in this linear model?

# Solution of (c)

vec_y <- -1+0.5*x+eps 
length(vec_y)
#Answer: Length of vector y is 100; and beta_0=-1 & beta_1=0.5



#### (d) Create a scatter plot displaying the relationship between x and
#       y. Comment on what you observe

# Solution of (d)

#plot(x,vec_y) [Method -1]
cor(x,vec_y)

library(ggplot2)
p1 = ggplot() + aes (x=x,y=vec_y) +geom_point()+xlab("x")+ylab("y") #Using ggplot[Method-2]
p1

#Comment: x and y are 68.4% correlated. From the figure we find that the slope of y is positive 




#### (e) Fit a least squares linear model to predict y using x. Comment
# on the model obtained. How do beta_0(hat) and beta_1(hat) compare to beta_0 and beta_1?

# Solution of (e)

fit<- lm(vec_y~x)
summary(fit)

#Comment: summary function provide beta_0(hat-1)= -1.01885 and beta_1(hat)= 0.49947 and earlier we find 
#         beta_0=-1 & beta_1=0.5. We saw that beta_0(hat) and beta_1(hat) is almost equal to
#         beta_0 and beta_1 Here from summary function we saw that p value is very very low so that we
#         can reject the null hypothesis. Also, the F statistics is large enough to accept the value. 



#### (f) Display the least squares line on the scatter plot obtained in (d).
#        Draw the population regression line on the plot, in a different
#        color. Use the legend() command to create an appropriate legend

# Solution of (f)
plot(x,vec_y)
abline(fit,col="blue") # include the least squares line 
abline(a=-1,b=0.5,col="red")  # include population regression line 
legend(x=-1.5,y=NULL, legend=c("least square line", "population regression line"))

#Note: I did not use ggplot here because the question ask to use legend() command.


#### (g) Now fit a polynomial regression model that predicts y using x and x2. 
#Is there evidence that the quadratic term improves the model fit? Explain your answer

# Solution of (g)

poly_reg<- lm(formula = vec_y ~ x + I(x^2))

summary(poly_reg)

# Answer: summary function shows that p value of I(x^2) is = 0.164, it is clear that the null hypothesis
#         cannot possible to reject. The test is not statistically significant. So there is not enough
#         evidence to conclude quadratic term improves the model fit. 
#         



#### (h) Repeat (a)-(f) after modifying the data generation process in
#        such a way that there is less noise in the data. The model (3.39) should remain the same. 
#       You can do this by decreasing the variance of the normal distribution used to generate 
#        the error term epsilon in (b). Describe your results.

# Solution of (h)

#subscript h used for part h


set.seed(1) 
x_h <- rnorm(100)    #(a)
eps_h <- rnorm(n=100, mean = 0, sd= sqrt(0.10))  # (b) and considering variance = 0.10 
vec_y_h <- -1+0.5*x_h+eps_h   #(c)
plot(x_h,vec_y_h)    #(d)
fit_h<- lm(vec_y_h~x_h)    #(e)
summary(fit_h)      #(e)
plot(x_h,vec_y_h)    #(f)
abline(fit_h,col="blue") # (f)   include the least squares line 
abline(a=-1,b=0.5,col="red")  # (f)   include population regression line 
legend(x=-1.5,y=NULL, legend=c("least square line", "population regression line"))   #  (f)

# Answer: When we change the variance to 0.10, the R^2 increased from the previous, this means the model 
#         fit better than the previous one. Comparing the figure of two plot, we can say that the data-
#         point fit better in second case.



#### (i) Repeat (a)-(f) after modifying the data generation process in
#        such a way that there is more noise in the data. The model
#        (3.39) should remain the same. You can do this by increasing
#        the variance of the normal distribution used to generate the
#        error term epsilon in (b). Describe your results.

# Solution of (i)

#subscript i used for part i


set.seed(1) 
x_i <- rnorm(100)    #(a)
eps_i <- rnorm(n=100, mean = 0, sd= sqrt(0.6))  # (b) and considering variance = 0.6
vec_y_i <- -1+0.5*x_i+eps_i   #(c)
plot(x_i,vec_y_i)    #(d)
fit_i<- lm(vec_y_i~x_i)    #(e)
summary(fit_i)      #(e)
plot(x_i,vec_y_i)    #(f)
abline(fit_i,col="blue") # (f)   include the least squares line 
abline(a=-1,b=0.5,col="red")  # (f)   include population regression line 
legend(x=-1.5,y=NULL, legend=c("least square line", "population regression line"))   #  (f)

# Answer: When we change the variance to 0.60, the R^2 decreased drastically from the previous two, 
#         this means that previous two model
#         fit  better than this one. Comparing the figure of three plot, we can say that the data-
#         point are comparatively much scatter in this case.


#### (j) What are the confidence intervals for beta_0 and beta_1 based on the
#        original data set, the noisier data set, and the less noisy data
#        set? Comment on your results.

# Solution of (j)

# Confidence Intervals for Model Parameters

confint(fit) # for the original data set 

confint(fit_h) # for the less noisier data set find in part h

confint(fit_i) # for the noisier data set find in part i



# Answer:  The lower and upper confidence limits for original data set is: 
#           (Intercept) -1.1150804(2.5%) -0.9226122(97.5%)
#             slope      0.3925794(2.5%)  0.6063602(97.5%)

#The lower and upper confidence limits for less noisier data set is: 
  #           (Intercept) -1.0514655(2.5%) -0.9653911(97.5%)
  #             slope      0.4519601(2.5%)  0.5475657(97.5%)
  
  
# The lower and upper confidence limits for noisier data set is: 
  #           (Intercept) -1.1627482(2.5%) -0.8905572(97.5%)
  #             slope       0.3480843(2.5%)   0.6504160 (97.5%)

# From the above data , it is clear that for most noisiest data (3rd one) the interval are wider followed
# by original and less noisier data. It is because the lower variability will result in a tighter 
# confidence interval with a smaller margin of error. And for the 2nd case the variance is small compare 
# to other to which results in narrower confidence interval. 