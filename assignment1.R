#ASSIGNMENT 1

1)

#starting with installing packages
library(logr)
library(dplyr)

tmp <- file.path(getwd(), "assignment1.log")

loga <- log_open(tmp)

loga

final_data <- read.csv("final_dataset_full.csv",header = TRUE,sep=",")

head(final_data)
# a) data has a long format, variables are in the columns and observations are 
#in the row

sum(is.na(final_data$gdp))
#7266 of the gdp values are missing.

dim(final_data)
count(final_data)

sum(is.na(final_data))

final_data <- final_data[!is.na(final_data$gdp), ]

sum(is.na(final_data$gdp))

#I just cleaned the NA values and created gdp1 in R with that code
sum(is.na(final_data$gdp1))

head(final_data)
colnames(final_data)

length(unique(final_data$NUTS_ID))
b)there is 1331 different regions in the data

summary(final_data$gdp)
#min value for gdp is 173.9 and the maximum value for it is 253100.7

class(final_data$YEAR)

final_data$year <- as.Date(paste0(final_data$YEAR, "-01-01"))

final_data <- final_data[order(final_data$year),]

head(final_data$year)

d)
min(final_data$gdp, na.rm = TRUE)

final_data$NUTS_ID[min(final_data$gdp, na.rm = TRUE)]

#just to make sure I will use another code 
min_gdp_index <- which.min(final_data$gdp)
final_data$NUTS_ID[min_gdp_index]

final_data[min_gdp_index,]

#second code was true, but what was the 1st one that I have found?
final_data[min(final_data$gdp, na.rm = TRUE), ]
#it wss the 1st observation and because there is NA in the gdp column,
#R shows us this because NA is thw lowest value(like 0).
#so the lowest gdp is EL643 , and its 173.9.

#for highest gdp

max_gdp_index <- which.max(final_data$gdp)
final_data$NUTS_ID[max_gdp_index]
final_data[max_gdp_index, ]
#for the highest gdp its FR101 and it is 253100.7.

class(final_data$year)

#that was all time lowest and highest gdp; now we will look for
#particularly in 2020.
data_2020 <- final_data[format(final_data$year, "%Y") == "2020", ]

min_gdp_2020 <- which.min(data_2020$gdp)

data_2020$NUTS_ID[min_gdp_2020]

data_2020[min_gdp_2020, ]

#lowest GDP in 2020 is 173.94, EL643

max_gdp_2020 <- which.max(data_2020$gdp)
data_2020$NUTS_ID[max_gdp_2020]
data_2020[max_gdp_2020, ]
#highest GDP in 2020 FR101 and it was 226006.9.

#e
highest_gr <- which.max(final_data$growth_rate)

final_data$NUTS_ID[highest_gr]

final_data[highest_gr, ]

#highest growt_rate for all time is DE913 and it was  0.5584698 in 2016.

summary(final_data$growth_rate)
#just for double checking.

#2
#a.1

data_lt <- final_data[substr(final_data$NUTS_ID,1,2)=="LT" ,]

data_lt$population

data_ltSort <- data_lt[order(data_lt$population),]

head(data_ltSort$population)
tail(data_ltSort$population)

head(data_lt)

#We can use the 1st 3 observations here ;
#LT011 means Vilnius,Vilnius City
#LT021 means Kaunas,Kaunas City
#LT022 means Klaipėda County

#first start with SPI_12_yearly
#(Standardized Precipitation Index over 12 months); 

head(data_lt$spi12_yearly)
#spi shows us if is there drought or excessive rainfall. If its negative then
#this region can be taken into account as dry conditions and if its positive then we
#can think about it as a wet conditions.
#average for SPI measure is 0.
#here we can see from the 1st 3 regions we had chosen are above 0,
#so we can say that Vilnius,Kaunas and Klaipeda has wet conditions.
#(may be bacuse of snow?)

#secon we will look for yearly heatwaves(yearly_hw);
head(data_lt$yearly_hw)
#common range is 1 to 6 for heatwaves and for 
#the regions I picked, it is 2,3,4 respectively.
#So over a year Vilnius has 2,Kaunas has 3 and Klaipeda has 4 heatwaves.

#then we ill look for aw_hw_intensity
head(data_lt$avg_hw_intensity)
#this shows us how much of the normal average heat over year 
#had been exceeded. For here in Vilnius 1.86 degrees exceeded
#normal value.
#For Kaunas it is 2.45 and for Klaipeda it is 1.85.

#last we will look for avg_len_hw;
head(data_lt$avg_len_hw)
#this shows the duration of the days of the heatwaves.
#For Vilnius its 5 days, Kaunas 2.6 and Klaipeda is 2.5 days.
#Long days of that can cause serious problems.

range(data_lt$spi12_yearly)
#Here the range is -2.1(lowest) to +2.1(highest)
range(data_lt$yearly_hw,na.rm=TRUE)
#here the range is 1 to 8
range(data_lt$avg_hw_intensity,na.rm=TRUE)
#here the range is 0.17 to 3.58
range(data_lt$avg_len_hw,na.rm=TRUE)
#here the range is 1 to 5.25

#a.2

head(data_lt)
mean(data_lt$gross_value_added_A)
#mean value for Lithuania is 111.2393 for gross_value_added_A

median(data_lt$gross_value_added_A)
#meadian for that economic indicator is 92.42

install.packages("DescTools")  
library(DescTools)

Mode(data_lt$gross_value_added_A)
#mode of thet variable in Lithuania dataset is 122.36, its frequency is 2.

sd(data_lt$gross_value_added_A)
#standard deviation for that variable in Lithuania dataset is 63.65

#b
library(ggplot2)


selected_regions <- c("LT011", "LT021", "LT022")

data_selected <- data_lt[data_lt$NUTS_ID %in% selected_regions, ]

ggplot(data_selected,aes(x=YEAR,y=spi12_yearly,color=NUTS_ID))+
geom_line(size=1) +
labs(title="Time Series of SPI 12 (Yearly) for Selected Regions",
     x = "Year",
     y= "SPI 12 (Yearly)")+
theme_minimal()

#from that graph I can understand that this climate indicator is very volatile
#for all of the regions but especcially Klaipeda looks a little bit more
#volatile than others.

ggplot(data_selected,aes(x = YEAR,y=yearly_hw,color = NUTS_ID))+
geom_line(size = 1)+
labs(title = "Time Series of Yearly Heatwaves for Selected Regions",
     x = "Year",
     y="Yearly Heatwaves")+
theme_minimal()
#there are some missing points for Kaunas, and maximum of yearly heatwaves
#region is Kaunas. Kaunas looks a bit volatile, as well as Klaipeda too.

ggplot(data_selected,aes(x = YEAR,y=avg_hw_intensity,color = NUTS_ID))+
geom_line(size = 1)+
labs(title = "Time Series of Average Heatwave Intensity for Selected Regions",
     x = "Year",
     y="Avg. Heatwave Intensity")+
theme_minimal()

#there are some missing points for all 3 regions between 2015 to 2018,
#rather than that we can see that the most volatile region here is Vilnius.
#Klaipeda and Kaunas follows eachother almost same.


ggplot(data_selected,aes(x = YEAR,y=avg_len_hw,color = NUTS_ID))+
geom_line(size = 1)+
labs(title = "Time Series of Average Heatwave Lenght for Selected Regions",
     x = "Year",
     y="Avg. Heatwave Lenght")+
theme_minimal()

#Here especially in 2015 and 2016 average heatwave lenght in Kaunas 
#and Klaipeda increased enormously, and after that point 
#there are again missing values until like 2018-2019.
#Kaunas had suffered by heatwaves in 2005-08 and then we can see Klaipeda
#also suffered tgem 2006-2008 but then there is a decrease for both regions.

#gross_value_added_A : mostly about agricultural economic activities
#in a country and as we can predict having very hot ot cold weather
#would not help agriculture, it can also be seen as the enemy of
#agriculture because to have a more agricultural economy we need
#temperate climates.

#gross_value_added_c : this is about manufacturing economic activites 
#here is another important point; yes this may not be affected by climate
#as much as agriculture but having so hot climate or so cold or so mold
#will cause people to lose their want to work? This may be the case too
#because if the weather is so cold I wouldn't want to get out of my 
#bed and if its so hot then I don't want to get out of the room with 
#air conditioner. 

#So climate actually an important for economic activities and it is not 
#only limited by agricultural economics. For the sake of my point of 
#view I can say that now in İstanbul, Türkiye there is so much snow so
#bicycle courriers are not working, it is forbidden for 4 days and also
#there was an announcement that do not go outside for the following days.
#I think this could have effect on economy.

ggplot(data_selected,aes(x=YEAR,y=gross_value_added_A,color=NUTS_ID))+
geom_line(size=1)+
labs(title="Time Series of GVA_A Lenght for Selected Regions",
     x = "Year",
     y="Gross Value Added A")+
theme_minimal()

#From here we can see that Klaipeda's economy mostly depends on agricultural
#activities, but as we saw before especially between 2008-2010 there 
#was a decrease in this activities may be because of last years
#heatwaves make that decrease because above average length of the 
#heatwaves were almost the maximum in 2005-08. This would affect
#next years harvests.

ggplot(data_selected,aes(x=YEAR,y=gross_value_added_C,color = NUTS_ID))+
geom_line(size=1)+
labs(title="Time Series of GVA_C Length for Selected Regions",
     x= "Year",
     y="Gross Value Added C")+
theme_minimal()

#We can see that Kaunas is not a good position here, and also there is
#a huge decrease in Vilnius and Klaipeda, when we look at the year interval
#we see that ist between 2007-2011, the decrease may be caused
#by 2008 Crises. Because this gross value is more about manufacture we can say that.
#Also we can see here that ending of 2019 to 2020 it decreased,
#this one probably caused due to COVID-19 pandemic crises.

log_close(tmp)

