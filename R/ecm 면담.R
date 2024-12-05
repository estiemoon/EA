library(readxl)
library(stargazer)
library(lmtest)
library(foreign)
library(ggplot2)
library(gridExtra)
library(psych)

setwd('Library/Mobile Documents/com~apple~CloudDocs/ecm_da/R')
data <- read_xlsx("ecm_result4.4.xlsx")

data$Q1 <- ifelse(
  data$"시군구" %in% c(
    "부산광역시", "대구광역시", "울산광역시", "제주특별자치도", "세종특별자치시", "진천군", "음성군", 
    "진주시", "원주시", "나주시", "전주시", "완주군", "김천", "경주시"
  ),
  1, 
  0  
)


summary(data)

#cf, kin, elementary, doctors, popgr, grdp ,tfer

data$TS <- ifelse(
  data$"시군구" %in% c(
  '부산광역시', '대구광역시', '나주시', '제주특별자치도', '세종특별자치시'),
  2012,
  ifelse(
    data$"시군구" %in% c('울산광역시', '원주시' ,'음성군',  '전주시','완주군' ,'진주시', '김천시')
    , 2013,
    ifelse(
      data$'시군구' %in% c('진천군'), 
      2014, NA
    )
  )
)

data <- data[!data$'시군구' %in% c('서울특별시', '경기도','인천광역시','김해시', '양산시', '경산시', 
                               '영천시', '군위군', '칠곡군', '성주군', '고령군', '청도군', 
                               '광주광역시', '담양군', '화순군', '함평군', '장성군', '경주시', 
                               '밀양시', '양산시', '횡성군', '괴산군', '증평군', '구미시', '상주시', 
                               '칠곡군', '사천시', '남해군', '하동군', '공주시', '대전광역시', 
                               '천안시', '청주시'),]

data$R <- ifelse(data$year>=data$TS,1,0)
data$K <- data$year-data$TS


data$a <-data$Q1*(data$K<=4)*(data$K>=-4)
data$b <-(1-data$Q1)*(data$year<=2017)*(data$year>=2008)
data$sample <- as.numeric(data$a+data$b==1)


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


data$a <- data$k_4*data$Q1 
data$b <- data$k_3*data$Q1 
data$c <- data$k_2*data$Q1 
data$d <- data$k0*data$Q1
data$e <- data$k1*data$Q1
data$f <- data$k2*data$Q1
data$g <- data$k3*data$Q1
data$h <- data$k4*data$Q1



abc<- lm(log(grdp)~Q1+yr1+yr2+yr3+yr4+yr5+yr6+yr7+yr8+yr9+a+b+c+d+e+f+g+h+cf+upr+eri,data=data, subset=sample==1)
summary(abc)
