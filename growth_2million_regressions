#main 3 variables are initial income level, 
#life expectancy, and primary-school enrollment (in 1960).
data <- variablesforreplication

summary(data$GDPSH60)
summary(data$LIFEE060)
summary(data$gamma) #dependent var.
summary(data$P60)
#all above except gamma are the fixed independent variables.

data$gamma[is.na(data$gamma)] <- mean(data$gamma,na.rm=TRUE)
data$GDPSH60[is.na(data$GDPSH60)] <- mean(data$GDPSH60,na.rm = TRUE)
data$LIFEE060[is.na(data$LIFEE060)] <- median(data$LIFEE060,na.rm = TRUE) 
data$P60[is.na(data$P60)] <- mean(data$P60,na.rm = TRUE)

modelbasic <- lm(gamma ~ GDPSH60 + LIFEE060 + P60,data = data)
summary(modelbasic)
# growth = 0.019 - 0.007GDPSH60 + 0.0007LIFEE060 + 0.02P60
#         (0.012)      (0.0021)     (0.00019)      (0.0066)
#from here we can see that except P60 all of the variables has 3 stars,
#so gdpsh60 and lifee060 are significant at 0, and p60 significant
#at 0.001.
#but P60 has 2 stars. In that case we can say that the variables are all
#significant due to very low p-values.
#Also all model is significant due to low (almost 0) f-stat. p value
#R2 here is 0.33, so our model can explain the variations
#in growth by 33%.


install.packages("ggplot2")
library(ggplot2)

ggplot(data = data,aes(x = GDPSH60, y = gamma)) + geom_point()
ggplot(data = data,aes(gamma,P60)) +geom_point()
ggplot(data=data,aes(gamma,LIFEE060)) +geom_point()

ggplot(data=data,aes(GDPSH60,LIFEE060)) + geom_point()
#here is as the GDP increases, life expactancy increase too.

#Adding new variables to the our basic model

model2 <- lm(gamma ~ GDPSH60 + LIFEE060 + P60 + PI6089,data=data)
options(scipen = 999)
summary(model2)
#growth = 0.021 - 0.0073GDPSH60 + 0.00066LIFEE060 + 0.022P60 - 0.00003PI6089
#std errors (0.01)     (0.0024)     (0.00022)        (0.007)    (0.00002)
#here we see that inflation rate is not related to growth,
#but it has a negative sign. It is like phlips curve relation
#in short run inflation may have effect over growth but
#in the long run there is no effect over it. 
#But high and low inflation can both harm growth.
#also R2 is 0.34, it increased but it would increase even though 
#a non related variable added to the regression so
#instead of looking at it we can look for adjusted one, in
#our first model adjusted R2 was 0.31, now its 0.32 so
#we can say adding inflation can explain something but still
#it is not significant.

model3 <- lm(gamma ~ GDPSH60 + LIFEE060 + P60 + PI6089 + dpop6090,data=data)
options(scipen = 999)
summary(model3)
# growth = 0.03 - 0.007GDPSH60 +0.0006LIFEE060 + 0.02P60 -0.00003PI6089 
#         (0.016)   (0.0025)    (0.00023)        (0.007)        (0.000024)
#-0.17dpop6090
#   (0.147)

#population is not a significant variable and also it has a negative 
#sign, we can interpreted as if it was a significant variable
#having high pop. will harm growth, is like basic Solow model.
#in Solow model as we know that countries with low population
#and high capital accumulation will have growth.

ggplot(data=data, aes(gamma,dpop6090)) + geom_point()

#not a very clear relationship

model4 <- lm(gamma ~ GDPSH60 + LIFEE060 + P60 + PI6089 + dpop6090
             + human60,data=data)
options(scipen=999)
summary(model4)

#growth = 0.0581 -0.011GDPSH60 +0.00056LIFEE060 +0.0235P60-0.00003PI6089
#         (0.02)    ( 0.0033)      (0.00031)     (0.01)     (0.00002)
#-0.326dpop6090 + 0.0003human60
# (0.178)         ( 0.00128)
#human education is not significant when we look at the table but,
#it has a positive relation with growth, which is expected.
#??nstead of that after adding that variable dpop's p value
#fell as the LIFEE060 increased. 
#here is the problem that adjusted R2 decreased to 0.28. As I keep adding
#non-related variables/non-significant will make fall R2.

ggplot(data=data,aes(human60,gamma)) +geom_point()
#not a very great relationship as we see from the table. 

model5 <- lm(gamma ~ GDPSH60 + LIFEE060 + P60 + PI6089 + dpop6090
             +RULELAW ,data=data)
options(scipen = 999)
summary(model5)
#growth =  0.046 -0.011GDPSH60 +0.0005LIFEE060 + 0.020P60 -0.00001PI6089
#       (0.017)     (0.0028)     (0.0002)         (0.007)    (0.00002)
#-0.058dpop6090 + 0.0187RULELAW
# (0.16)          (0.006)

#here is the RULELAW variable is significant, also adjusted R2 is almost
#0.33. So as RULELAW increases by 1 percent, growth will increase
#by 1.87% so there is a positive relation between RULELAW and growth as
#Daron Acemoglu stated. 
ggplot(data=data,aes(x = RULELAW,y=gamma)) + geom_point()

#not so much of a relationship, however after 0.5 in RULELAW
#there is increase in growth. So we will keep this variable

model6 <- lm(gamma ~ GDPSH60 + LIFEE060 + P60 + PI6089 + dpop6090
             +RULELAW + pinstab2 ,data=data)
options(scipen = 999)

summary(model6)
# growth =  0.051 -0.012GDPSH60 +0.0005LIFEE060 +0.0246P60 -0.00002PI6089 
#          (0.017)   (0.0027)      ( 0.00024)    (0.0078)    ( 0.00002)
#-0.081dpop6090 + 0.022RULELAW + 0.03pinstab2
#  (0.16)          (0.0061)       (0.012)


#political instability is also a significant variable and it has a positive
#relationship between growth. From this table we can say that
#we have 5 significant explanatory variables. RULELAW became
#even more better significany. Asjusted R2 become 0.37 so 
#political instability can explain growth, which is no suprising.
#This is also in the Daron Acemoglu's books and researchers.
#So when there is low political instability, growth will be high too. 
#Also coef. of RULELAW increased, so these two variable may be
#related with each other.

ggplot(data=data,aes(x = pinstab2,y=gamma)) + geom_point()
#we can see that relation through the graph, low political ins.
#regions has high growth.

model7 <- model6 <- lm(gamma ~ GDPSH60 + LIFEE060 + P60 + PI6089 + dpop6090
                       +RULELAW + pinstab2 + tot1 ,data=data)
options(scipen=999)
summary(model7)
#growth = 0.047 -0.011GDPSH60 +0.0005LIFEE060 +0.023P60 -0.00003PI6089
#       (0.017)     ( 0.003)    (0.0002)         (0.0079)  (0.000022)
#-0.113dpop6090 + 0.0131RULELAW + 0.026pinstab2 +0.0281tot1
#(0.16)            (0.0065)        (0.012)        (0.047)

#terms of trade ((export/import)*100) seen as no effect over growth,
#because p-value of that variable is 0.
#but if it was a significant variable we could said that it has a positive
#effect over growth. And it is actually make sense. How big the
#tot1, growth would increase becuase high tot1 means export>import.
#But the adjusted R2 falled to 0.35 from 0.37, so we would not
#keep tot1 because it does not explains growth.

model8 <- lm(gamma ~ GDPSH60 + LIFEE060 + P60 + PI6089 + dpop6090
                       +RULELAW + pinstab2 + freeop ,data=data)
options(scipen = 999)
summary(model8)
#growth = 0.059 -0.016GDPSH60 + 0.0009LIFEE060 +0.013P60 -0.00002PI6089
#        (0.02)   ( 0.0033)     (0.00038)       (0.010)    (0.000024)
#0.045dpop6090 +0.0255RULELAW +0.0379pinstab2 + 0.0221freeop 
# (0.178)       ( 0.007)        (0.013)           (0.0248)


#from that result, we can see that free trade openness is not a 
#significant variable because it is p-value is 0.37 so we fail to reject H0
#H0: B8 = 0, but even though its a insignificant variable its 
#still increased adjusted R2 to 0.37 to 0.45, so we wil keep it.

ggplot(data=data,aes(x=freeop,y=gamma)) + geom_point()
#as free trade openness increase, there is a little bit relation
#between growth, positively.
#but after adding freeop, P60 (one of our fixed variables) became
#insignificant.

model9 <- lm(gamma ~ GDPSH60 + LIFEE060 + P60 + PI6089 + dpop6090
             +RULELAW + pinstab2 + freeop +URB60,data=data)
options(scipen=999)
summary(model9)
#growth = 0.071 -0.017GDPSH60 +0.00086LIFEE060 +0.0139P60 -0.00005PI6089
#        ( 0.023)  (0.0035)     (0.0004)        (0.0103)   (0.00004)
#-0.018dpop6090 +0.023RULELAW +0.038pinstab2 + 0.0137freeop +0.011URB60
# (0.187)          (0.0072)     (0.013)         ( 0.026)     (0.0108)


#urbanization is not a significant variable, and it increased
#adjusted R2 so little. It also decreased p-value of RULELAW. 
#But we will keep it.

ggplot(data=data,aes(x=URB60,y=gamma))+geom_point()
#scatter plot is shown as it is very messy, not a real relation we can see.


model10 <- lm(gamma ~ GDPSH60 + LIFEE060 + P60 + PI6089 + dpop6090
              +RULELAW + pinstab2 + freeop +URB60 +wardum,data=data)
options(scipen=999)
summary(model10)

#growth = 0.063 -0.017GDPSH60 +0.0008LIFEE060 +0.014P60 -0.00005PI6089
#        (0.025)   (0.0036)     (0.0004)         (0.0103)   ( 0.00004)
#0.0381dpop6090 + 0.026RULELAW +0.039pinstab2 +0.0176freeop +0.0105URB60  
# (0.196)          ( 0.008)      (0.0139)      ( 0.0268)     (0.0108)
#0.0037wardum
#(0.0108)

#after adding war dummy, adjusted r2 decreased so little, also it is not 
#a significant variable. And its sign is positive, so if
#there is war, wardum=1, then the growth will increase. But i dont 
#think this is true in any sense. 





