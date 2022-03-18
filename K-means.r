K-means clustering in R Studio

#calling libraries
library(readxl)
library(NbClust)
library(flexclust)
library(fpc)
library(MASS)

#reading dataset
wine <- read_excel("Desktop/MSc Big Data/Data Mining and ML/Assessment 1/K /wine.xlsx")
head(wine) #present first six rows of the data
boxplot(wine[,-12]) #shows all the columns according to mean, median and outliers
summary(wine)
str(wine)

#data pre-processing (removing outliers) 

#outlier for pH
outlier <- boxplot(wine$pH,plot=FALSE)$out
print(outlier)
#save outlier code
save_outlier <- wine[which(wine$pH %in% outlier),]
#remove outlier code
wine <- wine[-which(wine$pH %in% outlier),] #
boxplot(wine$pH)
#outlier for free sulphur
outlier <- boxplot(wine$`free sulfur dioxide`,plot=FALSE)$out
print(outlier)
#save outlier code
save_outlier <- rbind(save_outlier,wine[which(wine$`free sulfur dioxide` %in% outlier),])
#remove outlier code
wine <- wine[-which(wine$`free sulfur dioxide` %in% outlier),]
boxplot(wine$`free sulfur dioxide`)
#outlier for residual sugar
outlier <- boxplot(wine$`residual sugar`,plot=FALSE)$out
print(outlier)
#save outlier code
save_outlier <- rbind(save_outlier,wine[which(wine$`residual sugar` %in% outlier),])
#remove outlier code
wine <- wine[-which(wine$`residual sugar` %in% outlier),]
boxplot(wine$`residual sugar`)
#outlier for total sulfur dioxide
outlier <- boxplot(wine$`total sulfur dioxide`,plot=FALSE)$out
print(outlier)
#save outlier code
save_outlier <- rbind(save_outlier,wine[which(wine$`total sulfur dioxide` %in% outlier),])
#remove outlier code
wine <- wine[-which(wine$`total sulfur dioxide` %in% outlier),]
boxplot(wine$`total sulfur dioxide`)
boxplot(wine)
set.seed(1234)

#scaling
data.train <- scale(wine[, -12])

#Nbclust
ss <- NbClust(data.train,distance = "euclidean", min.nc = 2, max.nc=10, method="kmeans")

#Evaluation
ss.m <- kmeans(data.train, 2)
confusetable <- table(wine$quality, ss.m$cluster)
randIndex(confusetable)
plotcluster(data.train, ss.m$cluster)
parcoord(data.train, ss.m$cluster)
