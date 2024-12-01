library(readxl)
library(stargazer)
library(lmtest)
library(foreign)
library(ggplot2)
library(gridExtra)
library(psych)

setwd('/Users/munseung-eun/Library/Mobile Documents/com~apple~CloudDocs/ecm_da')

data_f <- read_xlsx("ecm_results4.1 (1).xlsx")

data_f$Q1 <- ifelse(
  data_f$시군구 %in% c(
    "부산광역시", "김해시", "양산시", "대구광역시", "경산시", "영천시", "군위군",
    "칠곡군", "성주군", "고령군", "청도군", "나주시", "광주광역시", "담양군", "화순군",
    "함평군", "장성군", "울산광역시", "경주시", "밀양시", "원주시", "횡성군",
    "음성군", "증평군", "괴산군", "진천군", "전주시", "완주군", "김천시", "구미시",
    "상주시", "진주시", "사천시", "남해군", "하동군", "제주특별자치도", 
    "세종특별자치시", "공주시", "대전광역시", "천안시", "청주시"
  ),
  1,  # 조건을 만족하면 1
  0   # 조건을 만족하지 않으면 0
)



summary(data_f)

#cf, kin, elementary, doctors, popgr, grdp ,tfer

data_f$TS <- ifelse(
  data_f$시군구 %in% c(
    "부산광역시", "김해시", "양산시", "대구광역시", "경산시", "영천시", "군위군", 
    "칠곡군", "성주군", "고령군", "청도군", "제주특별자치도", "세종특별자치시", 
    "공주시", "대전광역시", "천안시", "청주시"
  ), 2012,
  ifelse(
    data_f$시군구 %in% c(
      "나주시", "광주광역시", "담양군", "화순군", "함평군", "장성군", 
      "울산광역시", "경주시", "밀양시", "양산시", "원주시", "횡성군", 
      "음성군", "전주시", "완주군", "김천시", "구미시", "상주시", 
      "진주시", "사천시", "남해군", "하동군"
    ), 2013,
    ifelse(
      data_f$시군구 %in% c("진천군", "증평군"), 2014, NA
    )
  )
)

data_f$R <- ifelse(data_f$year>=data_f$TS,1,0)

data_f$K <- data_f$year-data_f$TS



reg <- lm(grdp ~ )





