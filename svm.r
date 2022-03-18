#calling libraries
library(readxl)
library(grid)
library(MASS)
library(ggplot2)
library(e1071)
library(MLmetrics)

#reading dataset
ExchangeUSD <- read_excel("Desktop/MSc Big Data/Data Mining and ML/Assessment 1/SVM/ExchangeUSD.xlsx")
Exchange1 <- ExchangeUSD[,3] #takes only one column

#post normalisation and lag
Ex_norm <- read_excel("Desktop/MSc Big Data/Data Mining and ML/Assessment 1/SVM/Ex_norm.xlsx")
Exchange_train <- Ex_norm[1:374,]
Exchange_test <- Ex_norm[375:496,]

#SVM tuning for two input layers
svm_tuned <- tune(svm,C~A+B, data = Exchange_train,
ranges = list(gamma = seq(.05,.11,.01), cost = seq(1,4,0.5)),
tunecontrol = tune.control(sampling = "cross"))

#SVM
Exchange_model <- svm(C~A+B, data=Exchange_train, kernel= 'linear',cost = 4, scale = FALSE)
summary(Exchange_model)
Exchange_model2 <- svm(E~A+B+C+D, data=Exchange_train, kernel= 'linear',cost = 0.4, scale = FALSE)
summary(Exchange_model2)
Exchange_model3 <- svm(C~A+B, data=Exchange_train, kernel= 'radial',cost = 4, gamma = 0.06, scale = FALSE)
summary(Exchange_model3)
Exchange_model4 <- svm(E~A+B+C+D, data=Exchange_train, kernel= 'radial',cost = 3, gamma = 0.05, scale = FALSE)
summary(Exchange_model4)

Pred <- predict(Exchange_model,Exchange_test)
ExchangeUSD_TRAIN_original_E <- ExchangeUSD[1:374,"USD/EUR"]
ExchangeUSD_TEST_original_E <- ExchangeUSD[375:496,"USD/EUR"]

E_min <- min(ExchangeUSD_TRAIN_original_E)
E_max <- max(ExchangeUSD_TRAIN_original_E)

#unmormalization of Pred
unnormalize <- function(x, min, max) {
return((max - min)*x + min)
}

#call function
Prediction <- unnormalize(Pred,E_min,E_max)
Prediction

#correlation
cor(Prediction,ExchangeUSD_TEST_original_E$`USD/EUR`)

#correlation graph
par(mfrow=c(1,1))
plot(ExchangeUSD_TEST_original_E$`USD/EUR`,Prediction,col='red',main='Real vs predicted SVM',pch=18,cex=0.9)
abline(0,1,lwd=1)
legend('bottomright',legend='NN', pch=18,col='red', bty='n')
#metrics
MAPE(exp(Prediction),ExchangeUSD_TEST_original_E$`USD/EUR`)
RMSE(exp(Prediction),ExchangeUSD_TEST_original_E$`USD/EUR`)
MSE(exp(Prediction),ExchangeUSD_TEST_original_E$`USD/EUR`)
#Graph1
plot(ExchangeUSD_TEST_original_E$`USD/EUR`,pch=17, ylab= "SVM Predicted vs Actual")
par(new= TRUE)
plot(Prediction, col= 'orange',pch= 20,yaxt="n", ylab = " ")
#Graph2
excel_file <- cbind(seq(1:122), ExchangeUSD_TEST_original_E, Prediction)
colnames(excel_file) <- c("Time","Original","Predict")
ggplot(as.data.frame(excel_file), aes(Time))+ labs(y= "USD/EUR", x = "Time")+ geom_line(aes(y=as.numeric(Original)), colour ='BLUE')+ geom_line(aes(y=as.numeric(Predict)), colour ='RED')
