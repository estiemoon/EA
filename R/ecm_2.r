library(readxl)
library(stargazer)
library(lmtest)
library(foreign)
library(ggplot2)
library(gridExtra)
library(psych)

setwd('Library/Mobile Documents/com~apple~CloudDocs/ecm_da/R')
data <- read_xlsx("ecm_result6.3.xlsx")

data <- ecm_result6_3_2_


data$Q1 <- ifelse(
  data$통합필드 %in% c(
    "부산", "대구", 
    "나주", 
    "울산", "원주", 
    "음성", "진천", "전주", "완주", "김천",
    "진주", "제주", "세종","경주","청주","태안","보령","아산"
  ),
  1,  # 조건을 만족하면 1
  0   # 조건을 만족하지 않으면 0
)


data$TS <- ifelse(
  data$통합필드 %in% c(
    "부산","대구", 
    "제주"
    
  ), 2012,
  ifelse(
    data$통합필드 %in% c(
      "나주",
      "울산", "원주",
      "전주", "완주", "김천", "음성",
      "진주","세종","태안"
    ), 2013,
    ifelse(
      data$통합필드 %in% c(
        "진천", "청주"
      ), 2014,
      ifelse(
        data$통합필드 %in% c(
          "보령"
        ), 2015,
        ifelse(
          data$통합필드 %in% c(
          "경주", "아산"
          ), 2016, NA
        )
      )  
    )
  )
)


data <- data[!data$통합필드 %in% c( "서울", "경기","인천" ), ]



#data$R <- ifelse(data$year>=data$TS,1,0)

data$K <- data$year-data$TS

#data$K[is.na(data$K)] <- 0
#data$s1[is.na(data$s1)] <- 0

#data$K[data$K == 1000] <- NA



data$a <-data$Q1*(data$K<=4)*(data$K>=-4)
data$b <-(1-data$Q1)*(data$year<=2020)*(data$year>=2008)
data$a[is.na(data$a)] <- 0

data$sample <- as.numeric(data$a+data$b==1)
#data <- data[data$sample == 1, ]


data$k0 <- as.numeric(data$K==0) 
data$k1 <- as.numeric(data$K==1) 
data$k2 <- as.numeric(data$K==2) 
data$k3 <- as.numeric(data$K==3) 
data$k4 <- as.numeric(data$K==4) 

data$k_1 <- as.numeric(data$K==-1) 
data$k_2 <- as.numeric(data$K==-2) 
data$k_3 <- as.numeric(data$K==-3) 
data$k_4 <- as.numeric(data$K==-4) 


data$yr0 <- as.numeric(data$year==2008) 
data$yr1 <- as.numeric(data$year==2009)
data$yr2 <- as.numeric(data$year==2010) 
data$yr3 <- as.numeric(data$year==2011)
data$yr4 <- as.numeric(data$year==2012) 
data$yr5 <- as.numeric(data$year==2013)
data$yr6 <- as.numeric(data$year==2014) 
data$yr7 <- as.numeric(data$year==2015)
data$yr8 <- as.numeric(data$year==2016) 
data$yr9 <- as.numeric(data$year==2017)
data$yr10 <- as.numeric(data$year==2018)
data$yr11 <- as.numeric(data$year==2019)
data$yr12 <- as.numeric(data$year==2020) 


data$a_ <- data$k_4*data$Q1
data$b_ <- data$k_3*data$Q1
data$c_ <- data$k_2*data$Q1
data$d <- data$k0*data$Q1
data$e<-data$k1*data$Q1
data$f<-data$k2*data$Q1
data$g <- data$k3*data$Q1
data$h<-data$k4*data$Q1

abc<- lm(tfer~Q1+yr1+yr2+yr3+yr4+yr5+yr6+yr7+yr8+yr9+yr10+yr11+yr12+a_+b_+c_+d+e+f+g+h+cf+log(elementary)+log(tp)+log(grdp)+stres+log(doctors)+seoul_dist+ktx,data=data, subset=sample==1)
summary(abc)


abc<- lm(qlifea~Q1+yr1+yr2+yr3+yr4+yr5+yr6+yr7+yr8+yr9+yr10+yr11+yr12+a_+b_+c_+d+e+f+g+h+cf+ur+log(elementary)+log(tp)+log(doctors)+pd+log(grdp)+ktx+log(kin)+stres,data=data, subset=sample==1)
summary(abc)

abc<- lm(value~Q1+yr1+yr2+yr3+yr4+yr5+yr6+yr7+yr8+yr9+yr10+yr11+yr12+a_+b_+c_+d+e+f+g+h+log(first_ind)+log(manuf)+log(commerce)+log(tp)+er,data=data, subset=sample==1)
summary(abc)

names(data)
