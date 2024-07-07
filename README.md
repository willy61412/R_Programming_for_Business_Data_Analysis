1. **Classification_BankTermDeposit.R**
   - Processes and analyzes data aiming to predict whether clients will subscribe to a term deposit. It employs logistic regression, LDA, naive Bayes, k-NN, and decision tree models, focusing on data preprocessing, handling missing values, and optimizing model performance using different statistical learning techniques.
   - The models identify "duration," "previous," and "pdaysnew" as key factors influencing term deposit decisions. By focusing on these factors, the bank can tailor marketing strategies to effectively target potential customers, enhancing engagement and conversion rates.
  
2. **ForecastingModels_Employment_StockPrice.R**
   - Employs exponential smoothing state space (ETS) and ARIMA models to analyze and predict employment and stock closing prices, demonstrating how different model configurations impact forecast accuracy and including visual comparisons of predicted results against actual data.
<img width="1467" alt="Screenshot 2024-07-07 at 1 56 30 PM" src="https://github.com/willy61412/Classification_Model_Unraveling_Startup_Success/assets/133930618/449f592a-f2a6-4f62-86e4-d7b24456e9d7">

3. **RegressionModels_CreditCardBalance.R**
   - Utilizes linear, polynomial, and interaction regression models to analyze and predict credit card balances based on demographic and financial variables. It incorporates model selection techniques and split-sample testing to validate the predictive accuracy.
   - Key recommendations for management include adopting a well-balanced model that generalizes well to new data, monitoring for potential overfitting, and regularly updating the model to maintain accuracy. Incorporating additional data on customer behaviors, economic conditions, and external factors could further enhance predictive capabilities and improve the credit card balance prediction model.
  
4. **RegressionValidation_BikeRental.R**
   - Applies multiple regression methods such as linear regression, subset regression, forward and backward stepwise regression, Ridge, and LASSO, alongside cross-validation to predict bike rental counts, considering environmental and temporal influences.
   - I recommend the company focus on selecting significant features to enhance prediction accuracy and reduce computational costs, as different models highlight the importance of feature selection. Understanding the impact of each feature can improve model performance.

5. **TimeSeriesAnalysis_HousingStarts.R**
   - Utilizes polynomial regression and time series decomposition methods (classical and STL) to analyze and forecast housing starts data, focusing on identifying patterns of trends and seasonality.
   - I recommend the company utilize the STL decomposition model for predicting housing starts due to its superior performance in capturing trends and seasonality. The data shows a significant trend with housing starts decreasing from 1983 to 1992, increasing from 1990 to 2004, and again from 2010 to 2020, along with a notable seasonal effect each year.
<img width="1468" alt="Screenshot 2024-07-07 at 1 48 08 PM" src="https://github.com/willy61412/Classification_Model_Unraveling_Startup_Success/assets/133930618/45b861d9-6442-4c99-beca-1a86bfacbda6">

6. **TimeSeriesForecasting_GDPAndHousing.R**
   - Utilize AR, MA, ARMA, ARIMA, and ETS models, this script explores and predicts the behavior of simulated data, GDP growth, and housing market trends, showcasing various aspects of time series modeling.
   <img width="1470" alt="Screenshot 2024-07-07 at 2 02 33 PM" src="https://github.com/willy61412/Classification_Model_Unraveling_Startup_Success/assets/133930618/6531f0a2-1cdd-4cdd-b61b-21233d332a0c">
