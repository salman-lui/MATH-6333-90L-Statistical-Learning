
"
Author: Md Salman Rahman
Course: MATH 6333 Statistical Learning
Course Instructor: Dr. Tamer Oraby
Final Project
"
setwd("/Users/salman/OneDrive - The University of Texas-Rio Grande Valley/Course_video/Statistical Learning/Homework_salman/Final_project")

library(deSolve)
library(deBInfer)
library(ggplot2)

source("dataadj.R")
disease_date_vector = c()
new_cases_vector = c()
total_death_vector = c()
power_dist_vector = c()
indivi_vector = c()
masculi_vector = c()
uncer_avoid_vector = c()
indul_vector = c()
gdp_vector = c()

for (i in 1) {
  disease_date_vector = c(disease_date_vector, DB[[i]][["disease"]][["date"]])
  new_cases_vector = c(new_cases_vector, DB[[i]][["disease"]][["new_cases"]])
  total_death_vector = c(total_death_vector , DB[[i]][["disease"]][["total_deaths"]])
  gdp_vector = c(gdp_vector, DB[[i]][["GP"]][["GDP"]])
  power_dist_vector = c(power_dist_vector, DB[[i]][["Hofs"]][["Power Distance"]])
  indivi_vector = c(indivi_vector , DB[[i]][["Hofs"]][["Individualism"]])
  masculi_vector = c(masculi_vector, DB[[i]][["Hofs"]][["Masculinity"]])
  uncer_avoid_vector = c(uncer_avoid_vector, DB[[i]][["Hofs"]][["Uncertainty Avoidance"]])
  indul_vector = c(indul_vector , DB[[i]][["Hofs"]][["Indulgence"]])
}

data_disease <- data.frame(disease_date_vector, new_cases_vector, total_death_vector)

# time series of covid disease data
## function to standardize (z-score)
# libaray(zoo)
# stand <- function(x) {
#   y <- (x - mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)
#   return(y)
# }
# 
# # Create zoo object for satandardised anomalies:
# NH1 <- zoo(stand(data_disease$total_death_vector),order.by=dates)
# plot(NH1,main="",ylab="Standardized Temp (Z)",xlab="Year")

# Spectral Analysis
library(zoo)
meas_dis <- zoo(data_disease$new_cases_vector, as.Date(data_disease$disease_date_vector))

plot(meas_dis, xlab="Date", ylab="New Cases")

t_death <- zoo(data_disease$total_death_vector, as.Date(data_disease$disease_date_vector))
plot(t_death, xlab="Date", ylab="Total deaths")
q <- diff(t_death)
plot(q)


#power spectrum 
dspect <- spectrum(data_disease$new_cases_vector, log="no", spans=c(5,5), plot=FALSE)
delta <- 1/365
specx <- dspect$freq/delta
specy <- 2*dspect$spec
plot(specx[1:100], specy[1:100], xlab="Period (years)", ylab="Spectral Density", type="l")

#Coherence of new cases and total death 
ccf(data_disease$new_cases_vector,data_disease$total_death_vector, main="")


# wavelet coherence of the two series 
library(biwavelet)
## biwavelet requires input of nx2 matrices
len <- length(data_disease$total_death_vector)
t1 <- cbind(data_disease$disease_date_vector,data_disease$total_death_vector)
t2 <- cbind(data_disease$disease_date_vector,data_disease$new_cases_vector)
## don't want to actually do this because it takes a long time!
d1d2.wtc <- wtc(t1,t2)
plot(d1d2.wtc)





library(neuralnet)

library(keras)


#deep learning algorithm 
#autoencoder
#



