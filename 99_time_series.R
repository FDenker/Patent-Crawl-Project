library(xts)
library(TSA)
library(forecast)
library(tseries)
library(ggplot2)
library(ftplottools)
library(rugarch)
library(stargazer)
library(gridExtra)
library(grid)
library(tidyverse)
library(lubridate)
library(DT)


complete_join_no_na <- read_and_join_data()
names(complete_join_no_na)
group_by_app_date <- complete_join_no_na %>%  group_by(.$uspto_app_date) %>% 
  summarise(count=n()) %>% as.data.frame() 
View(group_by_app_date)
z <- read.zoo(group_by_app_date)
autoplot(z)

adf_1 <- adf.test(z)
adf_1
lgx<- ts(z)
ggAcf(lgx)
Box.test(lgx, type="Ljung-Box")
fit1 <- arma(lgx, c(0,  1 )) #MA(1)

fit2 <- arma(lgx, c(1,  0 )) #AR(1)

fit3 <- arma(lgx, c(1,  1 )) #ARMA(1,1)

fit4 <- arma(lgx, c(0,  2 ))

fit5 <- arma(lgx, c(2,  0 ))

fit6 <- arma(lgx, c(1,  2 )) #ARMA(2,2)

fit7 <- arma(lgx, c(2,  1 )) #ARMA(2,2)

fit8 <- arma(lgx, c(2,  2 )) #ARMA(2,2)

model_names <- c("ARMA(0,1)","ARMA(1,0)", "ARMA(1,1)", "ARMA(0,2)", "ARMA(2,0)","ARMA(1,2)", "ARMA(2,1)", "ARMA(2,2)")
aic_values <- c(round(summary(fit1)$aic,1), round(summary(fit2)$aic,1), round(summary(fit3)$aic,1), round(summary(fit4)$aic,1),
                round(summary(fit5)$aic,1), round(summary(fit6)$aic,1), round(summary(fit7)$aic,1), round(summary(fit8)$aic,1) )
aic_table <- cbind(model_names, aic_values)

