library(readxl)
popdata <- read_excel("/Users/salman/OneDrive - The University of Texas-Rio Grande Valley/The 6 Power/Human_COVID/Fall 2021/GDP_inf_new/gdp_population_data_frame 2007 to 2017.xlsx")

attach(popdata)
newdata=list()

len<-c(which(duplicated(location)==FALSE),length(location)+1)

for (i in 2:length(len)){
  x=len[i-1]
  y=len[i]
  newdata[[location[x]]]=c(countryname=location[x],
                           T=y-x,
                           GP=list(popdata[x:(y-1),2:4]))
  }
 ###################################################
disdata <- read_excel("/Users/salman/OneDrive - The University of Texas-Rio Grande Valley/The 6 Power/Human_COVID/Fall 2021/GDP_inf_new/final_covid.xlsx")
attach(disdata)
newdata2=list()

len2<-c(which(duplicated(location)==FALSE),length(location)+1)


for (i in 2:length(len2)){
  x=len2[i-1]
  y=len2[i]
  newdata2[[location[x]]]=c(countryname=location[x],
                           days=y-x,
                           stringency_index=list(disdata[x:(y-1),c(2,5)]),
                           disease=list(disdata[x:(y-1),c(2,3:4)]),
                           google=list(disdata[x:(y-1),c(2,12:17)]),
                           Hofs=list(disdata[x,6:11]))
}



DB=Map(c,newdata2,newdata)

rm(x,y,i,len,len2,popdata,disdata,newdata,newdata2)
