#calling libraries
library(readxl)
library(openxlsx)
library(neuralnet)
library(grid)
library(MASS)
library(MLmetrics)
library(ggplot2)

#reading dataset
ExchangeUSD <- read_excel("Desktop/MSc Big Data/Data Mining and ML/Assessment 1/Neural Networks/ExchangeUSD.xlsx")
Exchange1 <- ExchangeUSD[,3] #takes only one column

str(ExchangeUSD$`USD/EUR`)
summary(ExchangeUSD$`USD/EUR`)

#normalisation process
normalize <- function(x) {
return((x-min(x))/(max(x)-min(x)))} #here you call the function
Exchange_normalized <- as.data.frame(lapply(Exchange1, normalize)) #we create a normalised dataset

str(Exchange_normalized$USD.EUR)
summary(Exchange_normalized$USD.EUR)

#convert normalised dataset into excel file
write.xlsx(Exchange_normalized, file ="Desktop/PG MSc Big Data/Data Mining and ML/Assessment 1/Neural Networks/Exchange_normalized.xlsx")

#lag in Excel completed
Ex_norm <- read_excel("Desktop/MSc Big Data/Data Mining and ML/Assessment 1/Neural Networks/Ex_norm.xlsx")

#neuralnet
str(Ex_norm)
Exchange_train <- Ex_norm[1:374,]
Exchange_test <- Ex_norm[375:496,]
set.seed(12345)
Exchange_model <- neuralnet(E~A+B+C+D,data= Exchange_train,hidden=c(3,2)) #1
plot(Exchange_model)
Exchange_model <- neuralnet(E~A+B+C+D,data= Exchange_train,hidden=c(2,1)) #2
plot(Exchange_model)
Exchange_model <- neuralnet(C~A+B,data= Exchange_train,hidden=c(3,2)) #3
plot(Exchange_model)
Exchange_model <- neuralnet(C~A+B,data= Exchange_train,hidden=c(2,1)) #4
plot(Exchange_model)
Exchange_model <- neuralnet(F~A+B+C+D+E,data= Exchange_train,hidden=c(4,3)) #5
plot(Exchange_model)
Exchange_model <- neuralnet(F~A+B+C+D+E,data= Exchange_train,hidden=c(2,1)) #6
plot(Exchange_model)

#model results
model_results <- compute(Exchange_model, Exchange_test)

#obtain predicted Output(E) values
predicted_E <- model_results$net.result
head(predicted_E)

ExchangeUSD_TRAIN_original_E <- ExchangeUSD[1:374,"USD/EUR"]
ExchangeUSD_TEST_original_E <- ExchangeUSD[375:496,"USD/EUR"]

#find its maximum & minimum value - UNMORMALIZATION
E_min <- min(ExchangeUSD_TRAIN_original_E)
E_max <- max(ExchangeUSD_TRAIN_original_E)

#unnormalization
unnormalize <- function(x, min, max) {
return((max - min)*x + min)
}

#call function
E_pred <- unnormalize(predicted_E,E_min,E_max)
E_pred

#examine the correlation between predicted and actual values
cor(E_pred, ExchangeUSD_TEST_original_E)

#correlation graph
par(mfrow=c(1,1))
plot(ExchangeUSD_TEST_original_E$`USD/EUR`,E_pred,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='NN', pch=18,col='red', bty='n')

#Metrics
MAPE(exp(E_pred),ExchangeUSD_TEST_original_E$`USD/EUR`)
RMSE(exp(E_pred),ExchangeUSD_TEST_original_E$`USD/EUR`)
MAE(exp(E_pred),ExchangeUSD_TEST_original_E$`USD/EUR`)

#Graph1
plot(E_pred, col='red', pch=15, ylab = "Predicted vs Actual Output")
par(new= TRUE)
plot(ExchangeUSD_TEST_original_E$`USD/EUR`, col= 'blue',pch= 20, yaxt="n", ylab = " ")

#Graph2
excel_file <- cbind(seq(1:122), ExchangeUSD_TEST_original_E, E_pred)
colnames(excel_file) <- c("Time","Original","Predict")
ggplot(as.data.frame(excel_file), aes(Time))+labs(y= "USD/EUR", x = "Time")+geom_line(aes(y=as.numeric(Original)), colour ='GREEN')+geom_line(aes(y=as.numeric(Predict)), colour ='RED')
