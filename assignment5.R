
#ID 228

library(dplyr)
library(tidyr)
library(ggplot2)
install.packages("purrr")
library(purrr) #reduce fn comes from here
install.packages("quantmod")
library(quantmod)
                    
                    

symbols <- sort(c("BOTZ","WTAI","IGPT","ROBO","AIQ"))

prices <- 
  getSymbols(symbols, src = 'yahoo', from = "2023-01-01", to = "2024-12-31",
             auto.assign = TRUE, warnings = FALSE) %>% 
  map(~Cl(get(.))) %>%
  reduce(merge) %>% 
  `colnames<-`(symbols)
#turning WTAI to data frame
wtaidf <-data.frame(Date = index(WTAI), coredata(WTAI))

#turning date column to Date class
wtaidf$DATE <- as.Date(wtaidf$Date, "%m/%d/%Y")

data_close <- data.frame(Date = wtaidf$Date,
                         wtai_close = WTAI$WTAI.Close,
                         robo_close = ROBO$ROBO.Close,
                         igpt_close = IGPT$IGPT.Close,
                         botz_close = BOTZ$BOTZ.Close,
                         aiq_close = AIQ$AIQ.Close)

ggplot(data=data_close,aes(x=Date))+
  geom_line(aes(y=WTAI.Close,color="WTAI Close"))+
  geom_line(aes(y=ROBO.Close,color="ROBO Close")) +
  geom_line(aes(y=IGPT.Close,color="IGPT Close")) +
  geom_line(aes(y=BOTZ.Close,color="BOTZ Close")) +
  geom_line(aes(y=AIQ.Close,color="AIQ Close")) +
  labs(title = "Close Prices of Etfs", x = "Date", y = "Close Prices") 

#Here ROBO is the highest price of all and also it is a little bit volatile
#than others. Also IGPT too.
#IGPT increased around the beginning of 2024 and after the July, 2024
#it started to fall. 
#AIQ Falls affter July in both 2023 and 2024 may be this is caused
#because of seasonality.
#I dont think that ROBO and IGPT is stationary.
#WTAI is the one etf that is close to stationarity.

#wix_close turning to time series 

wtai_close <- ts(WTAI$WTAI.Close,frequency = 12) #working days
wtai_close

decomposed <- decompose(wtai_close)
plot(decomposed)

#From random, we can see that this data is stationary because there is
#no so many volatility, so im just predicting from the
#plot that there is constant mean and variance.
#To be sure we will use Dickey-Fuller Test.
library(zoo)
install.packages("forecast")
library(forecast)
install.packages("tseries")
library(tseries)

adf_test <- adf.test(wtai_close, alternative = "stationary")

print(adf_test)
#Dickey-Fuller Test null hypothesis is that there is unit root present 
#which means data is not stationary.
#H0: non-stationary
#H1: stationary

#Stationarity happens in AR(1) when; yt = b0+b1yt-1+ut, b1=1 (random walk)
#S = 1-b1, so yt = b0 + Syt-1+ut
#if S = 0, then we have unit root, time series not stationary.
#H0: S=0
#H1:S=!0
#from this test we have p-value of 0.09, so we fail to reject H0, non-stationarity
#at 5% signifancy level but we reject it at 10% sig. level.
#So we dont have very strong evidence that this time series is stationary.

#To correct this problem we can take differences or log of time series,
#or both.
wtai_new <- ts(diff(log(WTAI$WTAI.Close)),frequency = 12)

wtai_new1 <- na.omit(wtai_new)

adf_test1 <- adf.test(wtai_new1, alternative = "stationary")

print(adf_test1)

#New p-value is 0.01, so we have turned this non-stationary time series
#to stationary with using log differences.

#Now I will look at what should I use, AR or MA? For that I need to
#look for acf and pacf. If acf cuts off after a lag, and pacf 
#decrease gradually then better way is to use MA(moving average)
#but if the situation is the exact opposite then I will use
#AR(Auto Regressive).

acf(wtai_new1)
#after 1st lag ACF cuts off.

pacf(wtai_new1)
#there is not a very clear relationship, it goes up and down.
#so i will look for auto.arima too.

library(forecast)
auto.arima(wtai_new1)

#from here, first one ;
#AR(2) and MA(2), so it uses last 2 values and last 2 errors.
#From the table we can see that AIC AND BIC scores are pretty low,
#which indicates model fit is great because we like low AIC AND BIC.

#AIC(p) = log(SSR/T) + (1+p)*(lnT/T)
#BIC(p) = log(SSR/T) + (1+p)*(2/T)
#AIC is better for forecasting while BIC is better for finding the true model.
#But basically they are closely related, we can see it with just 
#looking their formulas.

#now for calculating returns, i will use differences of logs and 
#basic returns.
library(dplyr)
wtai_return <- wtaidf %>%
  mutate(simple_return = (WTAI.Close - lag(WTAI.Close)) / lag(WTAI.Close))
wtai_return


robodf <-data.frame(Date = index(ROBO), coredata(ROBO))

robo_return <- robodf %>%
  mutate(simple_return =(ROBO.Close -lag(ROBO.Close)) / lag(ROBO.Close))


igptdf <- data.frame(Date=index(IGPT),coredata(IGPT))

igpt_return <- igptdf %>%
  mutate(simple_return = (IGPT.Close - lag(IGPT.Close))/lag(IGPT.Close))

botzdf <- data.frame(Date = index(BOTZ),coredata(BOTZ))

botz_return <- botzdf %>%
  mutate(simple_return = (BOTZ.Close - lag(BOTZ.Close)) / lag(BOTZ.Close))

aiqdf <- data.frame(Date = index(AIQ),coredata(AIQ))

aiq_return <- aiqdf %>%
  mutate(simple_return = (AIQ.Close - lag(AIQ.Close))/lag(AIQ.Close))

df_return <- data.frame(Date = aiq_return$Date,
                        aiq_return1 = aiq_return$simple_return,
                        botz_return1 = botz_return$simple_return,
                        igpt_return1 = igpt_return$simple_return,
                        robo_return1 = robo_return$simple_return,
                        wtai_return1 = wtai_return$simple_return)

df_return1 <- na.omit(df_return)

summary(df_return1)

ggplot(data = df_return1,aes(x=Date))+
  geom_line(aes(y=aiq_return1,color = "AIQ"))+
  geom_line(aes(y=botz_return1,color="BOTZ"))+
  geom_line(aes(y=igpt_return1,color = "IGPT"))+
  geom_line(aes(y=robo_return1,color = "ROBO"))+
  geom_line(aes(y=wtai_return1,color = "WTAI"))+
  labs(title= "Returns From EFTs",x = "Date",y="Returns",color = "ETF")+
  theme_light()

summary(df_return1$aiq_return1)
summary(df_return1$botz_return1)
summary(df_return1$igpt_return1)
summary(df_return1$robo_return1)
summary(df_return1$wtai_return1)
#I am not very good at finance and i had barely take finance courses but
#from here i guess I would invest either AIQ or WTAI because
#AIQ has high returns as we can see in the graph but also
#there is big falls as there is high increases.
#But basically it dominates the graph. 
#The reason I chose WTAI is because its maximum value is 0.047 and
#its mean is high , there is no so much changes.

df_return1 <- df_return1 %>%
  mutate(portfolio_return = (aiq_return1 + botz_return1 + igpt_return1 + robo_return1 + wtai_return1) / 5)

ggplot(df_return1, aes(x = Date, y = portfolio_return)) +
  geom_line(color = "pink") +
  labs(title = "Equally Weighted Portfolio Returns",
       x = "Date", y = "Daily Return") +
  theme_minimal()

mean(df_return1$portfolio_return)
summary(df_return1$portfolio_return)


df_return1 <- df_return1 %>%
  mutate(portfolio_alt_return = (
    0.30 * aiq_return1 +
      0.15 * botz_return1 +
      0.10 * igpt_return1 +
      0.20 * robo_return1 +
      0.25 * wtai_return1
  ))

ggplot(df_return1,aes(x=Date,y=portfolio_alt_return))+
  geom_line(color = "red") +
  labs(title = "Alternative Portfolio Returns",x="Date",y="Daily Return")+
  theme_minimal()


df_return1$Year <- format(df_return1$Date, "%Y")
#because the assignment ask for daily returns we use years.

df_return1 <- df_return1 %>%
  group_by(Year) %>%
  mutate(
    port_equal_yearly = (aiq_return1 + botz_return1 + igpt_return1 + robo_return1 + wtai_return1) / 5
  ) %>%
  ungroup()



df_return1 <- df_return1 %>%
  group_by(Year) %>%
  mutate(
    port_alt_yearly = (
      0.30 * aiq_return1 +
        0.20 * botz_return1 +
        0.15 * igpt_return1 +
        0.10 * robo_return1 +
        0.25 * wtai_return1
    )
  ) %>%
  ungroup()

mean(df_return1$port_equal_yearly)
mean(df_return1$port_alt_yearly)
#alternative portfolio has higher mean, so higher returns than equal one.
summary(df_return1$port_equal_yearly)
summary(df_return1$port_alt_yearly)
#when we look at the summary statistics we see that maximum value of
#alternative one is higher as the minimum value lower and also
#not only mean is higher than equal one but median is higher too.
#So from here we can say that with that alternative portfolio
#we have better returns.

mu <- mean(df_return1$aiq_return1, na.rm = TRUE)
sigma <- sd(df_return1$aiq_return1, na.rm = TRUE)

n_days <- 30    # 30 days into the future
n_simulations <- 100  # 1000 paths

set.seed(7) 

simulations <- matrix(0, nrow = n_days, ncol = n_simulations)

for (i in 1:n_simulations) {
  simulations[, i] <- rnorm(n_days, mean = mu, sd = sigma)
}

start_price <- tail(AIQ$AIQ.Close, 1)

price_paths <- matrix(0, nrow = n_days + 1, ncol = n_simulations)
price_paths[1, ] <- start_price

for (i in 2:(n_days + 1)) {
  price_paths[i, ] <- price_paths[i-1, ] * (1 + simulations[i-1, ])
}

library(ggplot2)

# Transforming to df
price_paths_df <- as.data.frame(price_paths)
price_paths_df$Day <- 0:n_days


library(tidyr)
price_paths_long <- pivot_longer(price_paths_df, cols = -Day, names_to = "Simulation", values_to = "Price")


ggplot(price_paths_long, aes(x = Day, y = Price, group = Simulation)) +
  geom_line(alpha = 0.1, color = "darkblue") +
  labs(title = "Monte Carlo Simulation of AIQ Price over 30 Days",
       x = "Day", y = "Price") +
  theme_minimal()

#montecarlo studies basically helps us to understand whats going on
#in the data with simulating different high number of situations.
#Here we had 100 different situations. We assume normal distribution with
#the help of central limit theorem; if there is high number 
#of sample size f.e like higher than 25 then this sample
#will follow a normal distribution.

#This plot shows up different 100 paths for AIQ for the following 
#30 days span. As time goes by the volatility increases,
#Its in the range of 36$-48$.


set.seed(35)

n_simulations1 <- 500
n_days <- 30
start_value <- 1

mu_return <- mean(df_return1$port_alt_yearly, na.rm = TRUE)
sigma_return <- sd(df_return1$port_alt_yearly, na.rm = TRUE)


simulations <- matrix(NA, nrow = n_days, ncol = n_simulations1)


for (i in 1:n_simulations1) {
  daily_returns <- rnorm(n_days, mean = mu_return, sd = sigma_return)
  simulations[, i] <- start_value * cumprod(1 + daily_returns)
}


simulations_df <- data.frame(Day = 1:n_days, simulations)



simulations_long <- pivot_longer(simulations_df, cols = -Day)


ggplot(simulations_long, aes(x = Day, y = value, group = name)) +
  geom_line(alpha = 0.2, color = "blue") +
  labs(
    title = "Monte Carlo Simulation of Portfolio over 30 Days",
    x = "Day",
    y = "Portfolio Value"
  ) +
  theme_minimal()

#In finance, returns tell you how much your portfolio grows each day, thats 
#why we use cumprod in here.
#this graph shows that we start at value 1 and then when its
#1.2 that means there is a positive return of 20%, and if its 0.9 there 
#is a loss of 10%. There is 500 simulations and 
#our mean(mu) value is positive, so we have slighlty positive
#returns than negative.

#finally for sensitivy analysis;
#we will first change the set.seed() couple times and then
#look for 60 day and find out if we have any drastic changes in the 
#distribution or not.

set.seed(88)
for (i in 1:n_simulations1) {
  daily_returns <- rnorm(n_days, mean = mu_return, sd = sigma_return)
  simulations[, i] <- start_value * cumprod(1 + daily_returns)
}


simulations_df <- data.frame(Day = 1:n_days, simulations)


simulations_long <- pivot_longer(simulations_df, cols = -Day)


ggplot(simulations_long, aes(x = Day, y = value, group = name)) +
  geom_line(alpha = 0.2, color = "blue") +
  labs(
    title = "Monte Carlo Simulation of Portfolio over 30 Days,SS88",
    x = "Day",
    y = "Portfolio Value"
  ) +
  theme_minimal()

set.seed(65)
for (i in 1:n_simulations1) {
  daily_returns <- rnorm(n_days, mean = mu_return, sd = sigma_return)
  simulations[, i] <- start_value * cumprod(1 + daily_returns)
}


simulations_df <- data.frame(Day = 1:n_days, simulations)


simulations_long <- pivot_longer(simulations_df, cols = -Day)


ggplot(simulations_long, aes(x = Day, y = value, group = name)) +
  geom_line(alpha = 0.2, color = "blue") +
  labs(
    title = "Monte Carlo Simulation of Portfolio over 30 Days,SS65",
    x = "Day",
    y = "Portfolio Value"
  ) +
  theme_minimal()

#now we will do it by changing the days

set.seed(35)
n_days1 <- 60


simulations <- matrix(0, nrow = n_days1, ncol = n_simulations1)

for (i in 1:n_simulations1) {
  daily_returns <- rnorm(n_days1, mean = mu_return, sd = sigma_return)
  simulations[, i] <- start_value * cumprod(1 + daily_returns)
}

simulations_df <- data.frame(Day = 1:n_days1, simulations)

simulations_long <- pivot_longer(simulations_df, cols = -Day)

ggplot(simulations_long, aes(x = Day, y = value, group = name)) +
  geom_line(alpha = 0.2, color = "blue") +
  labs(
    title = "Monte Carlo Simulation of Portfolio over 60 Days",  # buray?? da 60 yap
    x = "Day",
    y = "Portfolio Value"
  ) +
  theme_minimal()

#here i can see that the changes start at around 1 but its a little
#bit different than we found at the beginning.
#So i can basically say when I look at 30 day period my
#simulations and returns are more predictable and stable
#but with longer time span as 60 days then they are not that good.


