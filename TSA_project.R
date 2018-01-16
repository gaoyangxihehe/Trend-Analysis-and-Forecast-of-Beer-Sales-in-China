library("bsts")
library("xlsx")
library("xts")
library("lubridate")
library("ggplot2")
library("forecast")
# 导出数据
beer <- read.xlsx("beer.xlsx", 1, header = T, stringsAsFactors = F, 
                  encoding = "UTF-8")
a <- c(0,diff(beer[,2]))
a[seq(1,72,4)] <- beer[,2][seq(1,72,4)]
beer$销售量 <- a
# 绘制时间趋势图
ggplot(beer, aes(x = 时间, y = 销售量)) + 
    geom_line(colour = "green") + 
    geom_area(colour = 'grey', alpha = 0.2) +
    labs(x = "时间", y = "销售量（万千升）", title = "1998-2015年啤酒销售趋势图")
beer$年 <- year(beer$时间)
ggplot(beer, aes(x = factor(年), y = 销售量, fill = factor(年))) +
    geom_boxplot( ) + 
    labs(x = '年份', y = '销售量（万千升）', title = "1998-2015年啤酒销售箱线图") +
    scale_fill_discrete(name = "年份")
ggAcf(beer$销售量, lag.max = 60) +
    labs(title = "啤酒销售量自相关系数图")
ggPacf(beer$销售量) +
    labs(title = "啤酒销售量偏自相关系数图")
# 一阶差分后
beer$差分 <- c(diff(beer[,2]), 0)
ggplot(beer, aes(x = 时间, y = 差分)) + 
    geom_line(colour = "green") + 
    geom_area(colour = 'grey', alpha = 0.2) +
    labs(y = "一阶差分值", title = "啤酒销售量一阶差分图")
ggAcf(beer$差分) +
    labs(title = "啤酒销售量一阶差分后自相关系数图")
ggPacf(beer$差分) +
    labs(title = "啤酒销售量一阶差分后偏自相关系数图")
### 序列差异(1 - L)(1 - L^4)Y
y <- vector()
for (i in 6:length(beer$销售量)) {
    y[i] <- (beer$销售量[i] - beer$销售量[i - 1] - beer$销售量[i - 4] + beer$销售量[i - 5])
}
beer$季节差分 <- y
ggplot(beer, aes(x = 时间, y = 季节差分)) + 
    geom_line(colour = "green") + 
    geom_area(colour = 'grey', alpha = 0.2) +
    labs(y = "季节差分值", title = "啤酒销售量季节差分图")
ggAcf(beer$季节差分) +
    labs(title = "啤酒销售量季节差分后自相关系数图")
ggPacf(beer$季节差分) +
    labs(title = "啤酒销售量季节差分后偏自相关系数图")
###################################################################
dat<-ts(beer[, 2], start=c(1998,1),frequency=4)
Y.train <- window(dat, end = c(2014, 3))
Y.test <- window(dat, start = c(2014, 4))
################################################### ETS Analysis and Forecasts
ets <- ets(Y.train, "ZZZ")
plot(ets)
a <- as.data.frame(ets$residuals)
ggplot(a, aes(sample=x)) + 
    stat_qq() +
    labs(title = "残差QQ图")
Box.test(ets$residuals,type="Ljung-Box")
# h-step ahead forecasts
for1 <- forecast(ets, h = 5)
for1
plot(for1)
lines(Y.test, col = "red")
accuracy(for1,Y.test)
#####################
# 构造
dat <- data.frame(time(dat), dat)
colnames(dat) <- c("date", "price")
dat$fitted <- c(for1$fitted, for1$mean)
dat$hi95 <- c(rep(NA, 67), for1$upper[, 2])
dat$lo95 <- c(rep(NA, 67), for1$lower[, 2])

ggplot(data=dat,aes(x=date,y=price)) + 
    geom_line(col='red') +
    geom_line(aes(y=fitted),col='blue') +
    geom_ribbon(aes(ymin=lo95,ymax=hi95),alpha=.25) +
    labs(x = "时间", y = "销售量（万千升）", title = "啤酒销售量预测图")
###########################################################
# ARIMA Analysis and Forecasts
arma <- auto.arima(Y.train)
arma
b <- as.data.frame(arma$residuals)
ggplot(b, aes(sample=x)) + 
    stat_qq() +
    labs(title = "残差QQ图")
Box.test(arma$residuals,type="Ljung-Box")
# h-step ahead forecasts
for2 <- forecast(arma, h = 5)
for2
plot(for2)
lines(Y.test)
accuracy(for2, Y.test)
# 诊断图
library("ggfortify")
ggtsdiag(auto.arima(Y.train))
##################
# 构造
dat$fitted <- c(for2$fitted, for2$mean)
dat$hi95 <- c(rep(NA, 67), for2$upper[, 2])
dat$lo95 <- c(rep(NA, 67), for2$lower[, 2])

ggplot(data=dat,aes(x=date,y=price)) + 
    geom_line(col='red') +
    geom_line(aes(y=fitted),col='blue') +
    geom_ribbon(aes(ymin=lo95,ymax=hi95),alpha=.25) +
    labs(x = "时间", y = "销售量（万千升）", title = "啤酒价格销售量预测图")
#####################################################################
# Using bsts package
ss <- AddLocalLinearTrend(list(), Y.train)
ss <- AddSeasonal(ss, Y.train, nseasons = 4)

# model estimation
model <- bsts(Y.train, state.specification = ss, niter = 1000)
names(model)
dim(model$state.contributions)

plot(model)
plot(model, "components", same.scale = F)
plot(model, "residuals")
plot(model, "seasonal")

# prediction
pred <- predict(model, horizon = 5, burn = 100)
pred
plot(pred, plot.original = 36)

##
dat$fitted <- c(rep(NA, 67), pred$mean)
dat$hi95 <- c(rep(NA, 67), pred$interval[2, ])
dat$lo95 <- c(rep(NA, 67), pred$interval[1, ])
dev.off()
ggplot(data=dat,aes(x=date,y=price)) + 
    geom_line(col='red') +
    geom_line(aes(y=fitted),col='blue') +
    geom_ribbon(aes(ymin=lo95,ymax=hi95),alpha=.25) +
    labs(x = "时间", y = "销售量（万千升）", title = "啤酒价格销售量预测图")

# redefine the ts of pred for evaluation
for3 <- ts(pred$mean, start = c(2014, 4), frequency = 4)
for3
accuracy(for3, Y.test)
