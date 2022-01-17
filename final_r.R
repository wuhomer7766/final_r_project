
library(readr)
library(dplyr)
library(ggplot2)
dataset <- list()
dataset <- readr::read_csv("https://github.com/PEIZHIHE/110-1-r4ds-main/blob/147f189ece0ae4ca676905d605111cf3f7fa2002/dataset.csv?raw=true", show_col_types = FALSE)

##在檢查資料時發現教育程度(年)有的值為96(年)，明顯為無效的資料，因此先進行篩選並砍掉這些資料
pick <- dataset$a13 == 96
which(pick)
dataset <- dataset[-which(pick),]


#以下是依照問卷調查的結果，針對教育程度(年)和月收入(台幣)的關係
#使用ggplot來畫散佈圖及迴歸線，來觀察兩者的相關程度
#先把教育程度(最高上限為25年) 做unlist方便作圖，因為部分問卷填答結果有誤，
#導致獨立變數和反應變數最終的有效資料筆數不同
#因此只抽取前500份的有效資料來進行分析

eduYear <- unlist(dataset$a13)
neweduYear <- dataset$a13[which(dataset$a13<=25)]
educationYear <- neweduYear[1:500]


#把月收入(最高上限為300000)的問卷答案(1~24) 改成真實新台幣數目
incomeRaw <- unlist(dataset$h26)
newmoney = c(0,10000,15000,25000,35000,45000,55000,65000,75000,85000,95000,105000,115000,125000,135000,145000,155000,165000,175000,185000,195000,250000,300000,0)
index = 1*(incomeRaw==1) + 2*(incomeRaw==2) + 3*(incomeRaw==3) + 4*(incomeRaw==4) + 5*(incomeRaw==5) + 6*(incomeRaw==6) +  7*(incomeRaw==7) + 8*(incomeRaw==8) + 9*(incomeRaw==9) + 10*(incomeRaw==10) +  11*(incomeRaw==11) + 12*(incomeRaw==12) + 13*(incomeRaw==13) + 14*(incomeRaw==14) + 15*(incomeRaw==15) + 16*(incomeRaw==16) +  17*(incomeRaw==17) +  18*(incomeRaw==18) +  19*(incomeRaw==19) +  20*(incomeRaw==20) +  21*(incomeRaw==21) +  22*(incomeRaw==22) + 23*(incomeRaw==23) + 24*(incomeRaw==24) 
incomeRaw = newmoney[index]
#
filter2<-filter(incomeRaw,incomeRaw>=0 & incomeRaw<=300000)
incomeMonthly <- incomeRaw[1:500]

#把月收入和教育程度單獨設一個新的df
NewDataFrame <- data.frame(EducationYear=educationYear,
                           IncomeMonthly=incomeMonthly)

##跑下一行code以防圖片出現自然數影響閱讀
options(scipen=999)


#我們以簡單迴歸分析，來觀察x(教育程度)和y(月收入)是否具有相關性

  
library(ggpmisc)

my.formula = y ~ x ##設定此公式亦即假設y=ax+b,

ggplot(data = NewDataFrame, aes(x = educationYear , y = incomeMonthly)) +
  geom_point() +
  geom_smooth(formula = y ~ x, method = lm , se = T , level = 0.95) +
  stat_poly_eq(formula = my.formula,aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse=TRUE,size = 5 ,colour = "blue")

##由程式結果可知，在95%信賴水準下的區間估計為圖中的灰色面積，藍色
##截距 a = 2.62*10^4  , 迴歸係數 b = 568，相關係數r^2<0.01
##因此在前五百名受試者當中x(教育程度)和y(月收入)無線性相關







