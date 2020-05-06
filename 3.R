pacman::p_load(tidyverse, zoo, lubridate, e1071, parallel, quantmod)
Unemployment <- read_csv("Unemployment.csv")

dat <- Unemployment

dat %>% 
  ggplot(aes(x = Label, y = Unemployment,  group = Race))+
  geom_line()+
  theme_classic()+
  facet_wrap(~Race)+
  geom_vline(aes(xintercept = 2000),  lty = "dashed", size = 1)+
  geom_vline(aes(xintercept = 2008), lty = "dashed", size = 1)+
  geom_vline(aes(xintercept = 2001),  lty = "dashed", size = 1)+
  geom_vline(aes(xintercept = 2011), lty = "dashed", size = 1)+
  geom_vline(aes(xintercept = 2019.4), lty = "dashed", size = .2)+
  geom_rect(aes(xmin=2000, xmax=2001, ymin=-Inf, ymax=Inf), alpha = .01)+
  geom_rect(aes(xmin=2008, xmax=2011, ymin=-Inf, ymax=Inf), alpha = .01)

black.time <- dat %>%
  filter(Race == "Black") %>% 
  select(Unemployment) %>% 
  ts(Unemployment, start=c(2000, 1), end=c(2020, 1), frequency=4)

white.time <- dat %>%
  filter(Race == "White") %>% 
  select(Unemployment) %>% 
  ts(Unemployment, start=c(2000, 1), end=c(2020, 1), frequency=4)

asian.time <- dat %>%
  filter(Race == "Asian") %>% 
  select(Unemployment) %>% 
  ts(Unemployment, start=c(2000, 1), end=c(2020, 1), frequency=4)

hispanic.time <- dat %>%
  filter(Race == "Hispanic") %>% 
  select(Unemployment) %>% 
  ts(Unemployment, start=c(2000, 1), end=c(2020, 1), frequency=4)

unemployment <- ts.intersect(hispanic.time, white.time, black.time, asian.time)

Just_GDP <- read_csv("Just_GDP.csv")
gdp <- Just_GDP
gdp.time <- ts(gdp$GDP_raw, start=c(1947, 1), end=c(2020, 1), frequency=4)
library(tsbox)
GDP.21 <-  ts_tbl(gdp.time) %>% 
  mutate(year = as.numeric(year(time))) %>% 
  filter(year >= 2000) %>% 
  select(value) %>% 
  ts(value, start=c(2000, 1), end=c(2020, 1), frequency=4)


library(ggfortify)
library(forecast)
library(patchwork)
autoplot(GDP.21)

d.arima <- auto.arima(GDP.21)

e <- forecast(ets(GDP.21), level = c(95), h = 5)
d.forecast <- forecast(d.arima, level = c(95), h = 5)
g1 <- autoplot(d.forecast)+
  labs(title = "ARIMA Forecast", y = "GDP")+
  theme_classic()

g2 <- autoplot(e)+
  labs(title = "ETS Forecast", y = "GDP")+
  theme_classic()

ggsave("GDP Forecast.jpeg")

autoplot(GDP.21)

d.arima <- auto.arima(black.time)
d.forecast <- forecast(d.arima, level = c(95), h = 5)
p1 <-autoplot(d.forecast)+
  labs(title = "Black")+
  theme_classic()

d.arima <- auto.arima(white.time)
d.forecast <- forecast(d.arima, level = c(95), h = 5)
p2 <- autoplot(d.forecast)+
  labs(title = "White")+
  theme_classic()


d.arima <- auto.arima(asian.time)
d.forecast <- forecast(d.arima, level = c(95), h = 5)
p3 <-autoplot(d.forecast)+
  labs(title = "Asian")+
  theme_classic()


d.arima <- auto.arima(hispanic.time)
d.forecast <- forecast(d.arima, level = c(95), h = 5)
p4 <-autoplot(d.forecast)+
  labs(title = "Hispanic")+
  theme_classic()



p1 + p2+ p3 +p4
ggsave("Unemployment_forecast.jpeg") 



d.arima <- auto.arima()
d.forecast <- forecast(d.arima, level = c(95), h = 5)
autoplot(d.forecast)+
  labs(title = "Hispanic")+
  theme_classic()



## NN
time_nn <- function(data){
fit <- nnetar(data, lambda=0.5)

sim <- ts(matrix(0, nrow=20, ncol=5), start=end(data)[1]+1)
for(i in seq(5)){
  sim[,i] <- simulate(fit, nsim=20)}

autoplot(data) + forecast::autolayer(sim)

fcast <- forecast(fit, PI=TRUE, h=5)
autoplot(fcast)}

g3 <- time_nn(GDP.21)+
  labs(title = "NN Forecast", y = "GDP")+
  theme_classic()

g1 + g2 +g3

ggsave("gdp.jpeg", width = 11, height = 7)

n1 <- time_nn(black.time)+
  labs(title = "Black")+
  theme_classic()
n2 <- time_nn(white.time)+
  labs(title = "White")+
  theme_classic()
n3 <- time_nn(asian.time)+
  labs(title = "Asian")+
  theme_classic()
n4 <- time_nn(hispanic.time)+
  labs(title = "Hispanic")+
  theme_classic()

n1+n2+n3+n4
ggsave("nn_forecast_unemployment.jpeg")

data <- unemployment


fit <- nnetar(data, subset = c(black.time, white.time, asian.time, hispanic.time), lambda=0.5, center = )

sim <- ts(matrix(0, nrow=20, ncol=5), start=end(data)[1]+1)
for(i in seq(5)){
  sim[,i] <- simulate(fit, nsim=20)}

autoplot(data) + forecast::autolayer(sim)

fcast <- forecast(fit, PI=TRUE, h=5)
autoplot(fcast)

## SVM

q <- seq(0, 80, by = 1)


svm.dt <- data.frame(cbind(q, dt) )
colnames(svm.dt)<-c("x","y")

tune.gamma.cost = tune(svm, y ~ x, data = svm.dt,
                       ranges = list(gamma = 2^(1:3), cost = 2^(3:5)),
                       tunecontrol = tune.control(sampling = "fix")
)
print(tune.gamma.cost)
svmodel <- svm(y ~ x,data=svm.dt, type="eps",kernel="radial",cost=8, gamma=2)

#specify timesteps for forecast, eg for all series + 12 months ahead
nd <- 1:100
#compute forecast for all the 156 months 
prognoza <- predict(svmodel, newdata=data.frame(x=nd))

ylim <- c(min(svm.dt$y), max(svm.dt$y))
xlim <- c(min(nd),max(nd))
plot(svm.dt$y, col="blue", ylim=ylim, xlim=xlim, type="l")
par(new=TRUE)
plot(prognoza, col="red", ylim=ylim, xlim=xlim)



fit <- nnetar(data, lambda=0.5)

sim <- ts(matrix(0, nrow=20, ncol=5), start=end(gdp.time)[1]+1)

for(i in seq(5)){
  sim[,i] <- simulate(svmodel, nsim=20)}

autoplot(svm.dt) + forecast::autolayer(sim)

fcast <- forecast(fit, PI=TRUE, h=5)
autoplot(fcast)

library(caret)

svm.model <- train(y ~x, data = svm.dt, method="svmRadial", 
      metric= "RMSE")

# shows the optimal tuning parameters, look for the value of “C”
svm.model$bestTun
rmse_svm <- svm.model$results %>% 
  filter(C == 1) %>% 
  .$RMSE 
rmse



#### Rolling K-folds cross validation

k <- 5
i <- 16
rmse_ets <- rep(NA,5)
rmse_arima <- rep(NA,5)
rmse_nn <- rep(NA,5)
rmse_svm <- rep(NA,5)

dt <- ts_tbl(gdp.time) %>% 
  mutate(year = as.numeric(year(time))) %>% 
  filter(year >= 2000) %>% 
  select(value)


pred_ets <- c()
pred_arima <- c()


time_nn <- function(data){
  fit <- nnetar(data, lambda=0.5)
  
  sim <- ts(matrix(0, nrow=20, ncol=5), start=end(data)[1]+1)
  for(i in seq(5)){
    sim[,i] <- simulate(fit, nsim=20)}
  
  autoplot(data) + forecast::autolayer(sim)
  fcast <- forecast(fit, PI=TRUE, h=16)
  return(fcast)
}


for (j in 1:k){
  fold <- dt[1:i, "value"]
  test <- dt[c(1:(i*2)), "value"]
  
  y <- 2000 + round((i/4))
  
  ts <- ts(dt[1:i, "value"], start=c(2000, 1), frequency=4) %>% 
    na.omit()
  
  pred_ets <- data.frame(forecast(ets(ts), 16)$mean)
  pred_arima <- data.frame(forecast(auto.arima(ts), 16)$mean)
  pred_nn <-  data.frame(time_nn(ts)$mean)
  
  q <- seq(1, i, by = 1)
  svm.dt <- data.frame(cbind(q, fold) )
  colnames(svm.dt)<-c("x","y")
  svmodel <- svm(y ~ x,data=svm.dt, type="eps",kernel="radial",cost=8, gamma=2)
  rmse_svm[j] <- svm.model$results %>% 
    filter(C == 1) %>% 
    .$RMSE 
  
  i = i + 16
  
  
  names(pred_arima) <- "arima"
  names(pred_ets) <- "ets"
  names(pred_nn) <- "NN"
  
  
  pred_ets <- ts(pred_ets, start=c(y, 01), frequency = 4)
  pred_arima <- ts(pred_arima, start=c(y, 01), frequency =4)
  pred_nn <-  ts(pred_nn, start=c(y, 01), frequency =4)
  
  rmse_ets[j] <- accuracy(pred_ets, ts(test$value, start=c(y, 1), frequency=4))[2]
  rmse_arima[j] <- accuracy(pred_arima, ts(test$value, start=c(y, 1), frequency=4))[2]
  rmse_nn[j] <- accuracy(pred_nn, ts(test$value, start=c(y, 1), frequency=4))[2]
  
 
  
}

rmse <- data.frame(rmse_arima, rmse_ets rmse_nn)

rmse %>% 
  rename(ARIMA = "rmse_arima",
         ETS = "rmse_ets",
         SVM = "rmse_svm",
         NN = "rmse_nn") %>% 
  mutate(k = seq(from = 1, to = 5, by = 1)) %>% 
  pivot_longer(ARIMA:NN, names_to = "Test", values_to = "RMSE") %>% 
  ggplot(aes(x = k, y= RMSE, color = Test, group = Test))+
  geom_line()+
  theme_classic()

ggsave("K-folds_model_comparison.jpeg")



### 10501-Q, Goods