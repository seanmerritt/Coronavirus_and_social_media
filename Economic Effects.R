pacman::p_load(tidyverse, zoo, lubridate, e1071, ggfortify, forecast, patchwork)

Economy <- read_csv("Economy.csv")

### Make CV comparison between the regular and the bagged models and perhpas the simulated models. 

time_nn <- function(data){
  fit <- nnetar(data, lambda=0.6)
  
  sim <- ts(matrix(0, nrow=16, ncol= 10), start=end(data)[1]+1)
  for(i in seq(5)){
    sim[,i] <- simulate(fit, nsim=16)}
  
  autoplot(data) + forecast::autolayer(sim)
  fcast <- forecast(fit, PI=TRUE, h=16)
  return(fcast)
}

time <- function(data, quarters, variable){
  
  dat <- ts(data, start=c(2000, 1), end=c(2020, 1), frequency=4)
  
  
  d.arima <- auto.arima(dat)
 g1 <- forecast(d.arima, level = c(95), h = 16)
  

  g2 <- forecast(ets(dat), level = c(95), h = quarters)

  fit <- nnetar(dat, lambda=0.5)
  
  sim <- ts(matrix(0, nrow=quarters, ncol=10), start=end(dat)[1]+1)
  for(i in seq(5)){
    sim[,i] <- simulate(fit, nsim=quarters)}
  
  autoplot(dat) + forecast::autolayer(sim)
  
  g3 <- forecast(fit, PI=TRUE, h=quarters)
  
  
graphic <- autoplot(dat) +
    autolayer(g1, series="ARIMA", PI=T, alpha = .5) +
    autolayer(g2, series="ETS", PI=T, alpha = .5) +
    autolayer(g3, series="Neural Network", PI=T, alpha = .5) +
    guides(colour=guide_legend(title="Forecasts"))+
    theme_classic()+
    theme(legend.position = "bottom")+
    labs(y = variable, title = "Forecast Comparison")
  
  
  #### Rolling K-folds cross validation
  
  k <- 5
  i <- 16
  rmse_ets <- rep(NA,5)
  rmse_arima <- rep(NA,5)
  rmse_nn <- rep(NA,5)

  
  dt <- data
  
  
  pred_ets <- c()
  pred_arima <- c()
  
  
  for (j in 1:k){
    fold <- dt[1:i]
    test <- dt[c(1:(i*2))]
    
    y <- 2000 + round((i/4))
    
    ts <- ts(dt[1:i], start=c(2000, 1), frequency=4)
    
    pred_ets <- data.frame(forecast(ets(ts), 16)$mean)
    pred_arima <- data.frame(forecast(auto.arima(ts), 16)$mean)
    pred_nn <-  data.frame(time_nn(ts)$mean)
    
    i = i + 16
    
    names(pred_arima) <- "arima"
    names(pred_ets) <- "ets"
    names(pred_nn) <- "NN"
    
    
    pred_ets <- ts(pred_ets, start=c(y, 01), frequency = 4)
    pred_arima <- ts(pred_arima, start=c(y, 01), frequency =4)
    pred_nn <-  ts(pred_nn, start=c(y, 01), frequency =4)
    
    
    
    rmse_ets[j] <- accuracy(pred_ets, ts(data, start=c(y, 1), frequency=4))[2]
    rmse_arima[j] <- accuracy(pred_arima, ts(data, start=c(y, 1), frequency=4))[2]
    rmse_nn[j] <- accuracy(pred_nn, ts(data, start=c(y, 1), frequency=4))[2]
    
    
  }
  
  rmse <- data.frame(rmse_arima, rmse_ets, rmse_nn)
  
cv <-   rmse %>% 
    rename(ARIMA = "rmse_arima",
           ETS = "rmse_ets",
           NN = "rmse_nn") %>% 
    mutate(k = seq(from = 1, to = 5, by = 1)) %>% 
    pivot_longer(ARIMA:NN, names_to = "Test", values_to = "RMSE") %>% 
    ggplot(aes(x = k, y= RMSE, color = Test, group = Test))+
    geom_line()+
    theme_classic()+
    labs(title = "Rolling K-folds CV")

return(list(CV = cv, Forecast = graphic))
}

## GDP
lam  <- seq(from = 0, to = 1, by = .2)
rmse <- matrix(NA, nrow = 5, ncol = 6)
k <- 6
for(i in 1:k){

modelcv <- CVar(Economy2$GDP, k=5, lambda= .75)

rmse[i,1] <- modelcv$fold1$accuracy[2]
rmse[i,2] <- modelcv$fold2$accuracy[2]
rmse[i,3] <- modelcv$fold3$accuracy[2]
rmse[i,4] <- modelcv$fold4$accuracy[2]
rmse[i,5] <- modelcv$fold5$accuracy[2]
}

t <- time(Economy$GDP, quarters = 12, variable = "GDP")

t$Forecast
ggsave("GDP_Forcast.jpeg")
t$CV
ggsave("GDP_CV.jpeg")

ts.dat <- ts(Economy$GDP, start = 2000, end = 2020, frequency = 4)
nsim <- 1000
sim <- bld.mbb.bootstrap(ts(Economy$GDP, start = 2000, end = 2020, frequency = 4), nsim) 


h <- 16

future <- matrix(0, nrow=nsim, ncol=h)

for(i in seq(nsim))
  future[i,] <- simulate(nnetar(sim[[i]],lambda = .6), nsim=h)

start <- tsp(ts.dat)[2]+1/12
simfc <- structure(list(
  mean = ts(colMeans(future), start=start, frequency=12),
  lower = ts(apply(future, 2, quantile, prob=0.025),
             start=start, frequency=12),
  upper = ts(apply(future, 2, quantile, prob=0.975),
             start=start, frequency=12),
  level=95),
  class="forecast")

nn <- time_nn(ts(Economy$GDP, start = 2000, end = 2020, frequency = 4))

etsfc <- forecast(nnetar(ts(Economy$GDP, start = 2000, end = 2020, frequency = 4)), h=h, level=95, PI = T)
autoplot(ts(Economy$GDP, start = 2000, end = 2020, frequency = 4)) +
  autolayer(simfc, series="Simulated NN", alpha = .4) +
  autolayer(etsfc, series="NN", alpha = .4)+
  autolayer(nn, series = "NNr", alpha = .4)

etsfc <- ts.dat %>% ets() %>% forecast(h=16)
arimafc <- ts.dat %>% auto.arima() %>% forecast(h=16)
baggedfe <- ts.dat %>% baggedETS() %>% forecast(h=16)
baggedfc <- ts.dat %>% baggedModel(fn = auto.arima) %>% forecast(h=16)
autoplot(ts.dat) +
 autolayer(etsfc, series="ETS", PI=F, alpha = 1) +
  autolayer(baggedfe, series="BaggedETS", PI=F, alpha = 1) +
  
  autolayer(baggedfc, series="BaggedARIMA", PI=F, alpha = 1) +
 autolayer(arimafc, series="Arima", PI=F, alpha = 1) +
  guides(colour=guide_legend(title="Forecasts"))+
  theme_classic()

far2 <- function(x, h){forecast(nnetar(x, lambda = .6), h=10)}
e <- tsCV(ts.dat, far2, h=10, window = 16)

## Personal

t <- time(Economy$Personal, quarters = 12, title = "Personal")

t$Forecast
ggsave("Personal_Forcast.jpeg")
t$CV
ggsave("Personal_CV.jpeg")

## Vehicals
t <- time(Economy$`vehicles&parts`, quarters = 12, title = "Vehicals")

t$Forecast
ggsave("Vehical_Forcast.jpeg")
t$CV
ggsave("Vehical_CV.jpeg")

# Furnishings
t <- time(Economy$Furnishings, quarters = 12, title = "Furnishings")

t$Forecast
ggsave("Furnishing_Forcast.jpeg")
t$CV
ggsave("Furnishing_CV.jpeg")

# Recreation

t <- time(Economy$Recreational, quarters = 12, title = "Recreation")

t$Forecast
ggsave("Recreation_Forcast.jpeg")
t$CV
ggsave("Recreation_CV.jpeg")

## Food and beverages

t <- time(Economy$Off_FB, quarters = 12, title = "Food and Beverages Off")

t$Forecast
ggsave("Food and beverage_Forcast.jpeg")
t$CV
ggsave("Food and beverage_CV.jpeg")

## Energy
t <- time(Economy$Energy, quarters = 12, title = "Energy")

t$Forecast
ggsave("Energy_Forcast.jpeg")
t$CV
ggsave("Energy_CV.jpeg")

# Clothing
t <- time(Economy$Clothing, quarters = 12, title = "Clothing")

t$Forecast
ggsave("Clothing_Forcast.jpeg")
t$CV
ggsave("Clothing_CV.jpeg")


# Durable
t <- time(Economy$Durable, quarters = 12, title = "Durable")

t$Forecast
ggsave("Durable_Forcast.jpeg")
t$CV
ggsave("Durable_CV.jpeg")

# Non-durable
t <- time(Economy$Nondurable_goods, quarters = 12, title = "Non-durable")

t$Forecast
ggsave("NonDurable_Forcast.jpeg")
t$CV
ggsave("NonDurable_CV.jpeg")


cormat <- Economy %>% 
  select(-Year) %>% 
  cor()

get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

  
  upper_tri <- get_upper_tri(cormat)
upper_tri

  
  # Melt the correlation matrix
  library(reshape2)
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Heatmap
library(ggplot2)
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_classic()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()





