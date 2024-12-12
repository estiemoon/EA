library(readxl)
library(stargazer)
library(lmtest)
library(foreign)
library(ggplot2)
library(gridExtra)
library(psych)

setwd('Library/Mobile Documents/com~apple~CloudDocs/ecm_da/R')
data <- read_xlsx("ecm_result6.3.2.xlsx")


data$Q1 <- ifelse(
  data$"통합 필드" %in% c(
    "부산", "대구", 
    "나주", 
    "울산", "원주", 
    "음성", "진천", "전주", "완주", "김천",
    "진주", "제주", "세종"
  ),
  1,  # 조건을 만족하면 1
  0   # 조건을 만족하지 않으면 0
)


data$TS <- ifelse(
  data$"통합 필드" %in% c(
    "부산","대구","제주"
  ), 2012,
  ifelse(
    data$"통합 필드" %in% c(
      "나주",
      "울산", "원주",
      "전주", "완주", "김천", "음성",
      "진주","세종","태안"
    ), 2013,
    ifelse(
      data$"통합 필드" %in% c(
        "진천"
        ),2014,NA
      )  
    )
  )


data <- data[!data$"시도" %in% c("경기도"), ]
data <- data[!data$"통합 필드" %in% c("서울", "인천"), ]

data$tp2 <- data$tp * data$tp
data$"metro" <- ifelse(grepl("광역시", data$"시군구"), 1, 0)

data$K <- data$year-data$TS


data$a <-data$Q1*(data$K<=4)*(data$K>=-4)
data$b <-(1-data$Q1)*(data$year<=2018)*(data$year>=2008)
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


data$a_ <- data$k_4*data$Q1
data$b_ <- data$k_3*data$Q1
data$c_ <- data$k_2*data$Q1
data$d <- data$k0*data$Q1*data$s2
data$e <- data$k1*data$Q1*data$s2
data$f <- data$k2*data$Q1*data$s2
data$g <- data$k3*data$Q1*data$s2
data$h <- data$k4*data$Q1*data$s2
data$base_k <- data$k_1*data$Q1


data$K[is.na(data$K)] <- 0
data$k0[is.na(data$k0)] <- 0
data$k1[is.na(data$k1)] <- 0
data$k2[is.na(data$k2)] <- 0
data$k3[is.na(data$k3)] <- 0
data$k4[is.na(data$k4)] <- 0
data$k_1[is.na(data$k_1)] <- 0
data$k_2[is.na(data$k_2)] <- 0
data$k_3[is.na(data$k_3)] <- 0
data$k_4[is.na(data$k_4)] <- 0

data$a_[is.na(data$a_)] <- 0
data$b_[is.na(data$b_)] <- 0
data$c_[is.na(data$c_)] <- 0
data$d[is.na(data$d)] <- 0
data$e[is.na(data$e)] <- 0
data$f[is.na(data$f)] <- 0
data$g[is.na(data$g)] <- 0
data$h[is.na(data$h)] <- 0
data$base_k[is.na(data$base_k)] <- 0

check_sample <- subset(data, sample==1)

tfer<- lm(tfer~Q1+yr1+yr2+yr3+yr4+yr5+yr6+yr7+yr8+yr9+yr10
         +a_+b_+c_+d+e+f+g+h
         +log(elementary)+stres+log(doctors)+seoul_dist+metro+youth+sr
         ,data=check_sample)
summary(tfer)

grdp<- lm(log(grdp)~Q1+yr1+yr2+yr3+yr4+yr5+yr6+yr7+yr8+yr9+yr10
          +a_+b_+c_+d+e+f+g+h
          +er+sr+youth+tp
          ,data=check_sample)
summary(grdp)


check_sample$first_ind_rate <- check_sample$first_ind/check_sample$total_ind
check_sample$manuf_rate <- check_sample$manuf/check_sample$total_ind


grdp2<- lm(log(grdp)~Q1+yr1+yr2+yr3+yr4+yr5+yr6+yr7+yr8+yr9+yr10
          +a_+b_+c_+d+e+f+g+h
          +er + manuf_rate + first_ind_rate
          +sr+youth+log(tp)
          +ktx
          ,data=check_sample)
summary(grdp2)



bb<-grdp2$coef
vv<-vcov(grdp2) 
names(bb)
bb[14]

check_k <- check_sample$k_4
sum(check_k)

check_sample$eff <- bb[13]*check_sample$k_4+ bb[14]*check_sample$k_3+ bb[15]*check_sample$k_2+ bb[16]*check_sample$k0+ bb[17]*check_sample$k1+ bb[18]*check_sample$k2+ bb[19]*check_sample$k3+ bb[20]*check_sample$k4

check_sample$eff_CI_u <- (bb[13]+1.64*sqrt(vv[13,13]))*check_sample$k_4+(bb[14]+1.64*sqrt(vv[14,14]))*check_sample$k_3+(bb[15]+1.64*sqrt(vv[15,15]))*check_sample$k_2+(0   +1.64*sqrt(vv[2,2]))*check_sample$k_1+(bb[16]+1.64*sqrt(vv[16,16]))*check_sample$k0+(bb[17]+1.64*sqrt(vv[17,17]))*check_sample$k1+(bb[18]+1.64*sqrt(vv[18,18]))*check_sample$k2+(bb[19]+1.64*sqrt(vv[19,19]))*check_sample$k3+(bb[20]+1.64*sqrt(vv[20,20]))*check_sample$k4

check_sample$eff_CI_l <- (bb[13]-1.64*sqrt(vv[13,13]))*check_sample$k_4+(bb[14]-1.64*sqrt(vv[14,14]))*check_sample$k_3+(bb[15]-1.64*sqrt(vv[15,15]))*check_sample$k_2
+(0     -1.64*sqrt(vv[2,2]))*check_sample$k_1+(bb[16]-1.64*sqrt(vv[16,16]))*check_sample$k0+(bb[17]-1.64*sqrt(vv[17,17]))*check_sample$k1+(bb[18]-1.64*sqrt(vv[18,18]))*check_sample$k2+(bb[19]-1.64*sqrt(vv[19,19]))*check_sample$k3+(bb[20]-1.64*sqrt(vv[20,20]))*check_sample$k4

ggplot(check_sample, aes(x = K, y = eff)) + geom_point() +  geom_hline(yintercept = 0, linetype=1) +
  geom_errorbar(aes(ymin = eff_CI_l, ymax = eff_CI_u), width = 0.1) +
  ggtitle("Test")

