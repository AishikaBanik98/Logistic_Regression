credit <- read.csv("C:/Users/AISHIKA/Desktop/Credit_Risk_Train.csv")    #loading our training data into dataframe
head(credit)       #shows first six rows of the dataset

#Exploratory Data Analysis
library(Amelia)  #to explore how much missing data we have we use this package
missmap(credit, main='Credit Risk Training Data-Missing Map', col=c("yellow","black"),legend=FALSE)

#Data Visualization Using ggplot2
library(ggplot2)
ggplot(credit,aes(Gender)) + geom_bar(aes(fill=factor(Gender)), alpha=0.5)
ggplot(credit, aes(x = LoanAmount)) + geom_histogram(fill='pink',bins=20,alpha=0.5)
ggplot(credit, aes(x = ApplicantIncome)) + geom_histogram(fill='green',bins=20,alpha=0.5)
ggplot(credit, aes(x = CoapplicantIncome)) + geom_histogram(fill='yellow',bins=20,alpha=0.5)
ggplot(credit,aes(Married)) + geom_bar(aes(fill=factor(Married)), alpha=0.5)

#Data Cleaning
#Getting the mode value of the character variables
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
credit$Gender[is.na(credit$Gender)] = impute_mode(credit$Gender)
credit$Married[is.na(credit$Married)] = impute_mode(credit$Married)
credit$Credit_History[is.na(credit$Credit_History)] = impute_mode(credit$Credit_History)


#For numeric variables we use mean value of the attribute
credit$LoanAmount[is.na(credit$LoanAmount)] <- mean(credit$LoanAmount, na.rm = TRUE)
credit$Loan_Amount_Term[is.na(credit$Loan_Amount_Term)] <- mean(credit$Loan_Amount_Term, na.rm = TRUE)

missmap(credit, main='Credit Risk Training Data-Missing Map', col=c("yellow","black"),legend=FALSE)

str(credit)

library(dplyr)

cr=credit
#2 Unique values treatment
cr$Dummy_Gender=ifelse(credit$Gender=="Male",1,0)
cr$Dummy_Married=ifelse(credit$Married=="Yes",1,0)
cr$Dummy_Education=ifelse(credit$Education=="Graduate",1,0)
cr$Dummy_Self_employed=ifelse(credit$Self_Employed=="Yes",1,0)

#More than 2 unique values treatment
cr$Dummy_Urban=ifelse(credit$Property_Area=="Urban",1,0)
cr$Dummy_Rural=ifelse(credit$Property_Area=="Rural",1,0)
cr$Dummy_Semiurban=ifelse(credit$Property_Area=="Semiurban",1,0)

# Taking first character each of them
cr$Dummy_Dep=as.numeric(substr(credit$Dependents,1,1)) 

#Target response variable
cr$Loan_Status=ifelse(credit$Loan_Status=="Y",1,0)  

#Checking the transformed dataset 
head(cr,3)

library(dplyr)
cr<- select(cr,-Loan_ID, -Gender,-Married, -Education, -Self_Employed, -Property_Area, -Dependents)
head(cr,3)

str(cr)

#Training the model
library(caTools)
sample<- sample.split(cr$Loan_Status, SplitRatio = 0.70)
train = subset(cr, sample == TRUE)
test = subset(cr, sample == FALSE)
logistic_model <- glm(formula=Loan_Status ~ . , family=binomial(link='logit'), data=train)
summary(logistic_model)

#Check the Prediction Accuracy
fitted_probabilities <- predict(logistic_model, newdata=test, type='response')

#Calculate from predicted values
fitted_results<- ifelse(fitted_probabilities >=0.5,1,0)
misClasificError <- mean(fitted_results != test$Loan_Status)
print(paste('Accuracy:',1-misClasificError))

#Creating the confusion matrix
cf <- table(test$Loan_Status, fitted_probabilities > 0.5)
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

# False Nefgative - Actual is 1/Y but Predicted is 0/N
FN <- cf[1,2] 
FN

#Total number of observations
TO <- TN+TP+FP+FN 

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
test_prob = predict(logistic_model, newdata = test, type = "response")
test_roc = roc(test$Loan_Status ~ test_prob, plot = TRUE, print.auc = TRUE)
