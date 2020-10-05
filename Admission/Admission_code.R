admission <- read.csv("C:/Users/AISHIKA/Desktop/binary.csv")    #loading our training data into dataframe
head(admission)          #shows first six rows of the dataset

#Exploratory Data Analysis
library(Amelia)  #to explore how much missing data we have we use this package
missmap(admission, main='Admission Dataset-Missing Map', col=c("yellow","black"),legend=FALSE)

#Data Visualization Using ggplot2
library(ggplot2)
ggplot(admission,aes(rank)) + geom_bar(aes(fill=factor(rank)), alpha=0.5)
ggplot(admission, aes(x = gre)) + geom_histogram(fill='purple',bins=20,alpha=0.5)
ggplot(admission, aes(x = gpa)) + geom_histogram(fill='green',bins=20,alpha=0.5)

#Data Cleaning
#Getting the mode value of the categorical variables
impute_mode <- function(x){
  tb <- table(x)
  tbmax <- max(tb)
  if(all(tb == tbmax))
    mode = NA
  else if(is.numeric(x))
    mode = as.numeric(names(tb))[tb==tbmax]
  else
    mode = names(tb)[tb==tbmax]
  return(mode)
}

#For character variables we use mode value of the attribute
admission$rank[is.na(admission$rank)] = impute_mode(admission$rank)

#For numeric variables we use mean value of the attribute
admission$gre[is.na(admission$gre)] <- 0
admission$gpa[is.na(admission$gpa)] <-0

missmap(admission, main='Admission Dataset-Missing Map', col=c("yellow","black"),legend=FALSE)

#To check the structure of the dataset
str(admission)

admission$rank <- factor(admission$rank)   #converting into factor

str(admission)

#Training the model
library(caTools)
sample=sample.split(admission$admit, SplitRatio=0.70)
train_data=subset(admission,sample==TRUE)
test_data=subset(admission,sample==FALSE)
log.model <- glm(formula=admit ~ . , family=binomial(link='logit'), data=train_data)
summary(log.model)

#Check the Prediction Accuracy
fitted.probabilities <- predict(log.model,newdata=test_data, type='response')

#Calculate from predicted values
fitted.results<- ifelse(fitted.probabilities >0.5,1,0)
misClasificError <- mean(fitted.results != test_data$admit)
print(paste('Accuracy:',1-misClasificError))

#Creating the confusion matrix
cf<- table(test_data$admit, fitted.probabilities > 0.5)
cf

#True Negative - Actual & Predicted is 0/N
TN <- cf[1,1] 
TN

#True Positive - Actual & Predicted is 1/Y
TP <- cf[2,2] 
TP

#False Positive - Actual is 0/N but Predicted is 1/Y
FP <- cf[2,1] 
FP

# False Negative - Actual is 1/Y but Predicted is 0/N
FN <- cf[1,2] 
FN

#Total number of observations
TO <- TN+TP+FP+FN 
TO

#Calculating Error Rate
ERR <- (FP+FN)/TO 
ERR

#Calculating Accuracy
ACC <- (TP+TN)/TO
ACC

#Calculating Specificity (True Negative Rate)
SP <- TN/(TN+FP)
SP

#Calculating Sensitivity (True Positive Rate)
SN <- TP/(TP+FN)
SN

#Calculating Precision
PREC <- TP/(TP+FP)
PREC

#Calculating False Positive Rate
FPR <- FP/(FP+TN)
FPR

#Checking ROC Curve of the model
library(pROC)
test_prob = predict(log.model, newdata = test_data, type = "response")
test_roc = roc(test_data$admit ~ test_prob, plot = TRUE, print.auc = TRUE)
