"# Gold-Price-Prediction-using-Time-Series-Analysis" 

Project Description:

Data:

  Source: https://www.investing.com/commodities/gold-historical-data
  
  Period: January 2014 to March 2024
  
  Frequency: Monthly

Libraries Used:

  dplyr
  urca
  readxl
  fpp2


Steps and Methodologies:

1. Exploratory Data Analysis (EDA):
   
   Time Series Plot: Visualized the gold price over time
   
   Seasonal Plot: Examined seasonal patterns in the gold price data
   
   Decomposition Plot: Decomposed the time series to analyze trend, seasonality, and residuals

3. Data Preparation:
   
   Train-Test Split: 80% training set and 20% test set
   
   Training set: January 2014 to February 2022
   
   Test set: March 2022 to March 2024

5. ARIMA Model:
   
   Transformation: Evaluated raw, Box-Cox, and log transformations; decided to use raw data
   
   Stationarity Check: Used Augmented Dickey-Fuller Test to check for unit roots
   
   Model Identification: Evaluated various ARIMA models and selected the best based on AICc values
   
   Residual Diagnostics: Ensured residuals behaved like white noise
   
   Forecasting: Made forecasts for the next two years using the selected ARIMA model

7. Error Trend Seasonal (ETS) Models:
   
   Model Evaluation: Tested various ETS configurations (additive, multiplicative, damped)
   
   Best Model Selection: Chose the ETS model with the lowest AIC, AICc, and BIC values
   
   Forecasting: Produced forecasts for the next two years using the best ETS model

9. Exponential Smoothing:
    
   Simple Exponential Smoothing (SES): Generated forecasts using SES with different alpha values
   
   Holt's Linear Trend Method: Created forecasts considering both linear and damped trends
   
   Holt-Winters Seasonal Method: Evaluated additive and multiplicative seasonal models, including damped variations

11. Model Comparison:
    
    Accuracy Metrics: Compared the models based on RMSE, MAE, MPE, MAPE, and MASE
   
    Visualization: Plotted forecasts from all models for visual comparison

Results:

  Best Performing Model: ARIMA(0,1,0) with drift was found to have the best performance on the test set.
  
  ETS Model: The ETS(MNN) model was also considered competitive but slightly underperformed compared to ARIMA.
  
  Exponential Smoothing: Holt's Linear Trend method was preferred over the Damped Holt's method, and Holt-Winters additive model performed better than its multiplicative counterpart

Conclusion:

This project demonstrates the application of various time series forecasting models to predict future gold prices. The ARIMA model outperformed other models in accuracy metrics, followed by the ETS and Holt's Linear Trend models. This analysis provides a comprehensive approach to time series forecasting, showcasing the strengths and limitations of different models.


How to Run:

1. Clone the repository
   
2. Ensure you have R and the required packages installed

3. Load the dataset and run the R script provided to reproduce the analysis and forecasts



For more details, please refer to the code and comments within the script. Feel free to raise any issues or contribute to the project!


  
