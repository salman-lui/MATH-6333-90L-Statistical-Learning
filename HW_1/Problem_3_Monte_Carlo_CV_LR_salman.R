
"
Problem 3
Author: Md Salman Rahman
Course: MATH 6333 Statistical Learning
Course Instructor: Dr. Tamer Oraby
Homework: The code in the file Monte_Carlo_CV_LR.R performs 20%-80% . Monte-Carlo CrossValidation of the 
linear regression model presented in Lab 1-R.R and discussed in
problem 1 for Linear Regression/classification. But unfortunately, it was done using a
for-loop and that would be a slow type of code. Re-write lines 5 through 14 to calculate
the output CV in just one line using the function replicate and the mean function. Run
the one-line code along with the file Lab 1 - R.R
"


#### Lab 1 Intro to Supervised Learning in R ###########

#set.seed(5)
library("MASS")
library("ggplot2")

mean_blue<-mvrnorm(n=10,c(1,0),diag(2))
is.matrix(mean_blue)
nrow(mean_blue)
DF_mean_blue<-as.data.frame(mean_blue)
colnames(DF_mean_blue)<-c('cx','cy')

p1=ggplot(DF_mean_blue,aes(x= cx, y=cy ))+
  geom_point(colour="blue",shape=23)+
  xlab('x')+
  ylab('y')
p1


mean_orange<-mvrnorm(n=10,c(0,1),diag(2))
DF_mean_orange<-as.data.frame(mean_orange)
colnames(DF_mean_orange)<-c('cx','cy')

p2=ggplot(DF_mean_orange,aes(x= cx, y=cy ))+
  geom_point(colour="orange",shape=23)+
  xlab('x')+
  ylab('y')
p2

p3=p1+
  geom_point(data=DF_mean_orange,aes(x= cx, y=cy ),colour="orange",shape=23)
p3

N=200

data_blue<-replicate(n=N/2 , c(mvrnorm(n=1,mean_blue[sample(nrow(mean_blue),1), ],diag(2)/5),0))
data_blue<-t(data_blue)

data_orange<-replicate(n=N/2 , c(mvrnorm(n=1,mean_orange[sample(nrow(mean_orange),1), ],diag(2)/5),1))
data_orange<-t(data_orange)

mix_data<-rbind(data_blue,data_orange)

DF_mix_data<-as.data.frame(mix_data)
colnames(DF_mix_data)<-c('cx','cy','Y')
rownames(DF_mix_data)<-c()

DF_mix_data<-DF_mix_data[sample(nrow(DF_mix_data)), ]
rownames(DF_mix_data)<-c()

p4=ggplot(DF_mix_data[ ,1:2],aes(x=cx,y=cy))+
  geom_point(colour= ifelse( DF_mix_data[,3]==1, "orange", "blue"))+
  theme_bw()+xlim(-3,4)+ylim(-3,3)
p4

rm(data_blue,data_orange,DF_mean_blue,DF_mean_orange,mean_blue,mean_orange,mix_data)

## Linear regression for classification

LR<-lm(Y~cx+cy,data=DF_mix_data)
summary(LR)
sum((predict(LR)>.5)+0 == DF_mix_data$Y)/N

(predict(LR,data.frame(cx=c(1.3,1.4),cy=c(2,4)))>.5)+0

beta=coef(LR)

meshpixel=100
x_pixel=seq(-3,4,length=meshpixel)
y_pixel=seq(-3,3,length=meshpixel)
M<-expand.grid(x_pixel,y_pixel)
DF_M<-as.data.frame(M)
colnames(DF_M)<-c('cx','cy')
color_M<-(predict(LR,DF_M)>.5)+0
new_M_LR<-cbind.data.frame(DF_M,color_M)

p5=p4+geom_point(data=new_M_LR[ , 1:2],aes(x=cx,y=cy),
                 colour=ifelse( new_M_LR[ , 3]==1,"orange","blue"),alpha=.01)+
  geom_abline(aes(intercept= (.5-beta[1])/beta[3], slope=-beta[2]/beta[3] ))
p5
ggsave("LRlearn.png",p5)


## Monte-Carlo 20%-80% Cross-Validation for Linear Regression/classification 
## Re-write lines 5 through 14 to calculate CV in just one line 
##using the function "replicate" and the mean function.




set.seed(2000)
CV<-mean(replicate(1000,mean((DF_mix_data[-(training_sample<-sample(200,200-40)),3]-predict(lm(Y~cx+cy,
                data=DF_mix_data,subset=training_sample),DF_mix_data[-training_sample,1:2]))^2)))
CV


set.seed(2000)
attach(DF_mix_data)
MSE<-0
M<-1000
for (i in 1:M){
  K=40
  train<-sample(200,200-K)
  LR_CV <- lm(Y ~ cx+cy , data = DF_mix_data, subset = train) 
  SDiff<-(DF_mix_data[-train,3]-predict(LR_CV,DF_mix_data[-train,1:2]))^2
  MSE<-MSE+mean(SDiff)
}
MSE/M










