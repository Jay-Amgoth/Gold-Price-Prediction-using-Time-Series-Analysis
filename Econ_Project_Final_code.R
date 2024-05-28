library(dplyr)
library(urca)
library(readxl)
library(fpp2)

#Monthly_data_frame = read.csv("C:/Users/jayar/Downloads/MSBA/Sem_2/ECON/Project/Final/Gold Futures Historical Data.csv")
Monthly_data_frame=read.csv("C:/Users/jayar/Downloads/MSBA/Sem_2/ECON/Project/Final/Gold Futures Historical Data.csv")
Monthly_data_frame

class(Monthly_data_frame)

ts_Monthly_data = ts(Monthly_data_frame, start = c(2014,1), end = c(2024,3), frequency = 12)
ts_Monthly_data
class(ts_Monthly_data)
ts_Monthly_data[,"Price"]
class(ts_Monthly_data[,"Price"])


autoplot(ts_Monthly_data[,"Price"])+
  geom_line(color = "Blue",size = 1)+
  labs(x = "Year", y = "Price", title = "Gold Price Over Time")+
  theme(axis.title = element_text(color = "Black", size = 20),
        axis.text = element_text(color = "Black", size = 20),
        text = element_text(size = 20),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "Light Grey"))

#EDA:

#SeasonPLot:

ggseasonplot(ts_Monthly_data[,"Price"], year.labels=TRUE, year.labels.left=TRUE) +
  ylab("Price") +
  ggtitle("Gold Price-Monthly Plot")+
  geom_line(size = 0.6)+
  theme(axis.title = element_text(color = "Black", size = 20),
        axis.text = element_text(color = "Black", size = 20),
        text = element_text(size = 20),
        plot.title = element_text(hjust = 0.5))


#Decompose Plot:

#plot(decompose(ts_Monthly_data[,"Price"]))

plot(decompose(ts_Monthly_data[,"Price"]), col = "Blue",lwd = 2, xlab = "Year")+
  par(cex.lab =2, cex.main = 2, cex.axis = 1.5)+title(main = "Updated Title")

# Splitting into Train & test Set:

Train_set = window(ts_Monthly_data[,"Price"],start = c(2014,1),end = c(2022,2),frequency = 12)
length(Train_set)
Test_set = window(ts_Monthly_data[,"Price"], start = c(2022,3), end = c(2024,3), frequency = 12)
length(Test_set)


#1)ARIMA with RAW Data

autoplot(ts_Monthly_data[,"Price"])

#No unusual observation and variance may be needs to be stabilized a bit. Let's check transformations

lamval = BoxCox.lambda(ts_Monthly_data[,"Price"])
#autoplot(cbind("Raw" = ts_Monthly_data[,"Price"],"BC" = BoxCox(ts_Monthly_data[,"Price"], elam),"Log" = log(ts_Monthly_data[,"Price"])),facets = T)

autoplot(cbind(Raw=(ts_Monthly_data[,"Price"]),BoxCox=BoxCox((ts_Monthly_data[,"Price"]), lamval),Log=log(ts_Monthly_data[,"Price"])),facets = T)+
  ylab(" ")+
  xlab("Year")+
  geom_line(color = "Blue",size = 1)+
  theme(axis.title = element_text(color = "Black", size = 20),
        axis.text = element_text(color = "Black", size = 20),
        text = element_text(size = 20))


#After comparing RAW, Boxcox and Log transformation, we see all three looks same, so we go with raw data, no transformation required.

ggtsdisplay(ts_Monthly_data[,"Price"])

#The ACF graph shows that the time series is not stationary as it decreasing slowly, it need to be differenced.

#Let's check for the no of differencing required and presence of unit root, there is no seasonality in our data but still making a confirmation from Augmented Dickey-Fuller Test Unit Root Test # 

nsdiffs(ts_Monthly_data[,"Price"]) # No seasonal differencing required

unit_test = ur.df(ts_Monthly_data[,"Price"], type ="trend")
summary(unit_test)


#Value of test-statistic is: -2.634 3.7073 4.4927 

#Critical values for test statistics: 
#  1pct  5pct 10pct
#tau3 -3.99 -3.43 -3.13
#phi2  6.22  4.75  4.07
#phi3  8.43  6.49  5.47

#The test statistic is grater than critical values so we failed to reject the null hypothesis. There is unit root and it requires differencing 

autoplot(ts_Monthly_data[,"Price"])
autoplot(diff(ts_Monthly_data[,"Price"]))


unit_test_2 = ur.df(diff(ts_Monthly_data[,"Price"]), type ="drift")
summary(unit_test_2)

#Value of test-statistic is: -7.7959 30.4175 

#Critical values for test statistics: 
#  1pct  5pct 10pct
#tau2 -3.46 -2.88 -2.57
#phi1  6.52  4.63  3.81

#We can reject the null hypothesis of unit root. No evidence of unit root after first differencing.

ggtsdisplay(diff(ts_Monthly_data[,"Price"]))


#building initial model

#We we see that sinusoidal pattern on both ACF and PACF graph. Significant spikes on both graphs at lag 6. So we can estimate three possible initial models, AR, MA, and ARIMA(6,1,6)
initial_model1 = Arima(ts_Monthly_data[,"Price"],c(6,1,6),include.drift=TRUE)
summary(initial_model1) #1363.56

initial_model2 = Arima(ts_Monthly_data[,"Price"],c(0,1,6),include.drift=TRUE)
summary(initial_model2) #1361.02

initial_model3 = Arima(ts_Monthly_data[,"Price"],c(6,1,0),include.drift=TRUE)
summary(initial_model3) #1361.19


#Lets try nearby best models for our initial_model2
#adjusting P, q values to try near by best models

near_model1 = Arima(ts_Monthly_data[,"Price"],c(0,1,7),include.drift=TRUE)
summary(near_model2) #1361.63

near_model2 = Arima(ts_Monthly_data[,"Price"],c(0,1,6),include.drift=TRUE)
summary(near_model2) #1361.02

near_model3 = Arima(ts_Monthly_data[,"Price"],c(0,1,5),include.drift=TRUE)
summary(near_model3) #worse 

near_model4 = Arima(ts_Monthly_data[,"Price"],c(0,1,4),include.drift=TRUE)
summary(near_model4) #worse

near_model5 = Arima(ts_Monthly_data[,"Price"],c(0,1,3),include.drift=TRUE)
summary(near_model5) #1360.07

near_model6 = Arima(ts_Monthly_data[,"Price"],c(0,1,2),include.drift=TRUE)
summary(near_model6) #1358.72 

near_model7 = Arima(ts_Monthly_data[,"Price"],c(0,1,1),include.drift=TRUE)
summary(near_model7) #1356.6

near_model8 = Arima(ts_Monthly_data[,"Price"],c(0,1,0),include.drift=TRUE)
summary(near_model8) #1355.25


#Based on AICc values near_model8 is the best

#Check residuals

checkresiduals(near_model8)
#The residuals are white noise

#forecasting

arima_fcast = forecast(near_model8, h = 21)
arima_fcast

autoplot(ts_Monthly_data[,"Price"])+
  autolayer(arima_fcast,series = "Arima",PI=T)+
  ggtitle("ARIMA Testset Forecast")+
  ylab("Price")+
  xlab("Year")+
  guides(colour=guide_legend(title="Forecast"))+
  geom_line(color = "Blue",size = 1)+
  theme(axis.title = element_text(color = "Black", size = 20),
        axis.text = element_text(color = "Black", size = 20),
        text = element_text(size = 20),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "Light Grey"))

#2) ETS (Error, Trend and Seasonal)


ets1 = ets(ts_Monthly_data[,"Price"],"AAA")
summary(ets1) #AIC = 1622.503, AICc = 1628.332, BIC = 1670.311

ets2 = ets(ts_Monthly_data[,"Price"],"AAN")
summary(ets2) # 1614.252, 1614.764, 1628.312 

ets3 = ets(ts_Monthly_data[,"Price"],"AAA", damped = TRUE)
summary(ets3) #1626.366,  1632.943, 1676.985 

ets5 = ets(ts_Monthly_data[,"Price"],"AAN", damped = TRUE)
summary(ets5) #1618.601, 1619.325, 1635.474

ets6 = ets(ts_Monthly_data[,"Price"],"ANA")
summary(ets6) #1622.320, 1626.806, 1664.502 

ets7 = ets(ts_Monthly_data[,"Price"],"ANN")
summary(ets7) #1611.768, 1611.970, 1620.205

ets8 = ets(ts_Monthly_data[,"Price"],"MAA")
summary(ets8) #1613.130, 1618.959, 1660.938

ets9 = ets(ts_Monthly_data[,"Price"],"MAM")
summary(ets9) #1616.542, 1623.119, 1667.161 

ets10 = ets(ts_Monthly_data[,"Price"],"MAN")
summary(ets10) #1602.598, 1603.111, 1616.659 

ets11 = ets(ts_Monthly_data[,"Price"],"MAA", damped = TRUE)
summary(ets11) #1618.906, 1625.483, 1669.526 

ets12 = ets(ts_Monthly_data[,"Price"],"MAM", damped = TRUE)
summary(ets12) #1616.542, 1623.119, 1667.161


ets13 = ets(ts_Monthly_data[,"Price"],"MAN", damped = TRUE)
summary(ets13) #1606.977, 1607.701, 1623.850 

ets14 = ets(ts_Monthly_data[,"Price"],"MNM")
summary(ets14) #1657.858, 1662.344, 1700.041 

ets15 = ets(ts_Monthly_data[,"Price"],"MNN")
summary(ets15) #1599.445, 1599.646, 1607.881 best model

ets_auto = ets(ts_Monthly_data[,"Price"])
ets_auto

# Based on above models, ets15 best minimizes the AIC, AICc and BIC values. The ets() function also picks MNN as the best model.

#Forecasting ets15 MNN model

ets_fcast = forecast(ets15, h=21)

autoplot(ts_Monthly_data[,"Price"])+
  autolayer(ets_fcast,series = "ets",PI=T)+ 
  ggtitle("ETS")+
  ylab("Price")+
  xlab("Year")+
  guides(colour=guide_legend(title="Forecast"))+
  geom_line(color = "Blue",size = 1)+
  theme(axis.title = element_text(color = "Black", size = 20),
        axis.text = element_text(color = "Black", size = 20),
        text = element_text(size = 20),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "Light Grey"))


ets_fcast



arima_train =Arima(Train_set,c(0,1,0), include.drift=TRUE)
arima_test = forecast(arima_train,h=25 )
ets_train = ets(Train_set,"MNN")
ets_test = forecast(ets_train,h=25)


accuracy(arima_test,Test_set)
accuracy(ets_test,Test_set)

# For ETS:

autoplot(ts_Monthly_data[,"Price"])+
  autolayer(ets_test,series = "ets",PI=T)+
  ggtitle("ETS Testset Forecast")+
  ylab("Price")+
  xlab("Year")+
  guides(colour=guide_legend(title="Forecast"))+
  geom_line(color = "Blue",size = 1)+
  theme(axis.title = element_text(color = "Black", size = 20),
        axis.text = element_text(color = "Black", size = 20),
        text = element_text(size = 20),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "Light Grey"))
 
#For Arima:

autoplot(ts_Monthly_data[,"Price"])+
  autolayer(arima_test,PI=T,series = "arima")+
  ggtitle("ARIMA Test set Forecast")+
  ylab("Price")+
  xlab("Year")+
  guides(colour=guide_legend(title="Forecast"))+
  geom_line(color = "Blue",size = 1)+
  theme(axis.title = element_text(color = "Black", size = 20),
        axis.text = element_text(color = "Black", size = 20),
        text = element_text(size = 20),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "Light Grey"))

#When comparing training and test set values between ARIMA(0,1,0) and ETS(MNN) models, ARIMA(0,1,0) has the lower ME, RMSE, MAE, MPE, MAPE, MASE. So between these two models, ARIMA(0,1,0) performing better.


#3. Exponential smoothing models

#3a) Simple exponential smoothing (SES):

ses1 = ses(ts_Monthly_data[,"Price"], h = 21, alpha = 0.5)
ses1

ses2 = ses(ts_Monthly_data[,"Price"], h = 21, alpha = 0.75)
ses2

ts_ses = ses(ts_Monthly_data[,"Price"],h=21)
ts_ses
ts_ses$model

autoplot(window(ts_Monthly_data[,"Price"],start=2019))+
  autolayer(ses1,series = "Alpha=0.5",PI=F)+
  autolayer(ses2,series = "Alpha=0.75",PI=F)+
  autolayer(ts_ses,series = "Alpha=Optimal",PI=F)


#3b) Holt's linear trend method

#creating a forecast with the holt model. It adds trend to the ses model. 
Holt_mod1 = holt(ts_Monthly_data[,"Price"],h=21)
Holt_mod1
Holt_mod1$model
summary(Holt_mod1)

#Damped Holt'smethod
Holt_Damp = holt(ts_Monthly_data[,"Price"],h=21,damped = T)
Holt_Damp$model
Holt_Damp


autoplot(window(ts_Monthly_data[,"Price"],start=2019))+
  autolayer(Holt_mod1,series = "Holt's Method",PI=F)+
  autolayer(Holt_Damp,series = "Damped Holt's Method",PI=F)+
  ggtitle("Forecasts from Holt's method")+
  ylab("Price")+
  xlab("Year")+
  guides(colour=guide_legend(title="Forecast"))+
  geom_line(color = "Blue",size = 1)+
  theme(axis.title = element_text(color = "Black", size = 20),
        axis.text = element_text(color = "Black", size = 20),
        text = element_text(size = 20),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "Grey"))


trainHolt = holt(Train_set,h=25)
trainDamped = holt(Train_set,h=25,damped = T)

accuracy(trainHolt,Test_set)
accuracy(trainDamped,Test_set)


autoplot(window(ts_Monthly_data[,"Price"], start = 2019))+
  autolayer(trainDamped,series = "Damped Holt's Method",PI=F)+
  autolayer(trainHolt,series = "Holt's Method",PI=F)+
  ggtitle("Forecasting on Test Set Holt's Linear Method")+
  ylab("Price")+
  xlab("Year")+
  guides(colour=guide_legend(title="Forecast"))+
  geom_line(color = "Blue",size = 1)+
  theme(axis.title = element_text(color = "Black", size = 20),
        axis.text = element_text(color = "Black", size = 20),
        text = element_text(size = 20),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "Grey"))


#Based on training and test set values, The Holt's method having the lower RMSE, MAE, MPE, MAPE and MASE values. So we can say that Holt's method performing better compared to Damped Holt's Method



#3c) Holt-Winter's seasonal method

#creating forecast with Holt-winter's. It adds trend and seasonality to ses.

Holt_Wint_Add = hw(ts_Monthly_data[,"Price"],h=21,seasonal = "additive")
Holt_Wint_Add$model
Holt_Wint_Add

Holt_Wint_Add_damped = hw(ts_Monthly_data[,"Price"],h=21,seasonal = "additive", damped = TRUE)
Holt_Wint_Add_damped$model
Holt_Wint_Add_damped

#or multiplicatively if it increases with the level of the time series. 
Holt_Wint_Mul = hw(ts_Monthly_data[,"Price"],h=21,seasonal = "multiplicative")
Holt_Wint_Mul$model
Holt_Wint_Mul


Holt_Wint_Mul_damped = hw(ts_Monthly_data[,"Price"],h=21,seasonal = "multiplicative", damped = TRUE)
Holt_Wint_Mul_damped$model
Holt_Wint_Mul_damped


autoplot(window(ts_Monthly_data[,"Price"],start=2022))+
  autolayer(Holt_Wint_Add,series = "Additive Season",PI=F)+
  autolayer(Holt_Wint_Add_damped,series = "Additive Damped",PI=F)+
  autolayer(Holt_Wint_Mul,series = "Multiplicative Season",PI=F)+
  autolayer(Holt_Wint_Mul_damped,series = "Multiplicative Damped",PI=F)+
  ggtitle("Forecasts from Holt-Winter's method")+
  ylab("Price")+
  xlab("Year")+
  guides(colour=guide_legend(title="Forecast"))+
  geom_line(color = "Blue",size = 1)+
  theme(axis.title = element_text(color = "Black", size = 20),
        axis.text = element_text(color = "Black", size = 20),
        text = element_text(size = 20),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "Light Grey"))


trainAdd = hw(Train_set,h=25,seasonal = "additive")
trainAdd_damped = hw(Train_set,h=25,seasonal = "additive", damped = TRUE)
trainMulti = hw(Train_set,h=25,seasonal = "multiplicative")
trainMulti_damped = hw(Train_set,h=25,seasonal = "multiplicative", damped = TRUE)




accuracy(trainAdd,Test_set)
accuracy(trainAdd_damped,Test_set)
accuracy(trainMulti,Test_set)
accuracy(trainMulti_damped,Test_set)


autoplot(window(ts_Monthly_data[,"Price"],start=2019))+
  autolayer(trainAdd,series = "seasonal Additive",PI=F)+
  autolayer(trainAdd_damped,series = "Additive Damped",PI=F)+
  autolayer(trainMulti,series = "Seasonal Multiplicative",PI=F)+
  autolayer(trainMulti_damped,series = "Multiplicative Damped",PI=F)+
  ggtitle("Forecasts on Test set Holt-Winter's method")+
  ylab("Price")+
  xlab("Year")+
  guides(colour=guide_legend(title="Forecast"))+
  geom_line(color = "Blue",size = 1)+
  theme(axis.title = element_text(color = "Black", size = 20),
        axis.text = element_text(color = "Black", size = 20),
        text = element_text(size = 20),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "Light Grey"))

#Based on Training and test set values among 4 models of Holt wilters: Additive, Additive Damped, Multiplicative, Multiplicative Damped. 
#Compring between Additive models, the Additive model has lower RMSE, MAE, MPE, MAPE & MASE as compared to Additive damped
#Comparing between Multiplicative models, Multiplicative Damped model has lower RMSE, MAE, MPE, MAPE values, so Multiplicative Damped is performing better than Multiplicative model

#Now comparing between Additive and Multiplicative damped: The Additive model has lower RMSE, MAE, MPE, and MAPE values so overall
#we can conclude that Holt-winter's additive model is performing better among these four models.



arima_train =Arima(Train_set,c(0,1,0), include.drift=TRUE)
arima_test = forecast(arima_train,h=25 )
ets_train = ets(Train_set,"MNN")
ets_test = forecast(ets_train,h=25)
trainHolt = holt(Train_set,h=25)
trainAdd = hw(Train_set,h=25,seasonal = "additive")


accuracy(arima_test,Test_set)
accuracy(ets_test,Test_set)
accuracy(trainHolt,Test_set)
accuracy(trainAdd,Test_set)


autoplot(window(ts_Monthly_data[,"Price"], start = 2019) )+
  autolayer(arima_test,PI=F,series = "Arima")+
  autolayer(ets_test,series = "ETS",PI=T)+
  autolayer(trainHolt,series = "Holt's Method",PI=F)+
  autolayer(trainAdd,series = "HW Seasonal Additive",PI=F)+
  ggtitle("Comparision of Best Models")+
  ylab("Price")+
  xlab("Year")+
  guides(colour=guide_legend(title="Forecast"))+
  geom_line(color = "Blue",size = 1)+
  theme(axis.title = element_text(color = "Black", size = 20),
        axis.text = element_text(color = "Black", size = 20),
        text = element_text(size = 20),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "Light Grey"))




#Forecasting all models are test set

trainHolt = holt(Train_set,h=25)
trainDamped = holt(Train_set,h=25,damped = T)
trainAdd = hw(Train_set,h=25,seasonal = "additive")
trainMulti = hw(Train_set,h=25,seasonal = "multiplicative")
arima_train =Arima(Train_set,c(0,1,0), include.drift=TRUE)
arima_test = forecast(arima_train,h=25 )
ets_train = ets(Train_set,"MNN")
ets_test = forecast(ets_train,h=25)




accuracy(trainHolt,Test_set)
accuracy(trainDamped,Test_set)
accuracy(trainAdd,Test_set)
accuracy(trainMulti,Test_set)
accuracy(arima_test,Test_set)
accuracy(ets_test,Test_set)



autoplot(window(ts_Monthly_data[,"Price"], start = 2020))+
  autolayer(trainAdd,series = "seasonal add",PI=F)+
  autolayer(trainMulti,series = "seasonal multi",PI=F)+
  autolayer(trainDamped,series = "Damped",PI=F)+
  autolayer(trainHolt,series = "holt",PI=F)+
  autolayer(arima_test,PI=F,series = "arima")+
  autolayer(ets_test,series = "ets",PI=F)
 




  


#None of the above models fitting with the training data so above models are not giving good a forecast. 
# But Seasonal additive is making an close forecast
