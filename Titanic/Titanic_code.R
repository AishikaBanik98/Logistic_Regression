df.train<- read.csv('C:/Users/aishi/Desktop/Logistic_Regression/Titanic/Titanic_Dataset.csv')    #loading our training data into dataframe
head(df.train)            #shows first six rows of the dataset

#Exploratory Data Analysis
install.packages("Amelia")
library(Amelia)  #to explore how much missing data we have we use this package
missmap(df.train, main='Titanic Training Data-Missing Map', col=c("yellow","black"),legend=FALSE) #yellow is missing and black means existing

#Data visualization using ggplot2
library(ggplot2)
ggplot(df.train, aes(Survived)) + geom_bar()
ggplot(df.train,aes(Pclass)) + geom_bar(aes(fill=factor(Pclass)),alpha=0.5)
ggplot(df.train,aes(Sex)) + geom_bar(aes(fill=factor(Sex)), alpha=0.5)
ggplot(df.train,aes(Age)) + geom_histogram(fill='blue',bins=20,alpha=0.5)
ggplot(df.train,aes(SibS))

#Data Cleaning
#we want to fill in missing age data instead of just dropping the missing age rows
p1<- ggplot(df.train,aes(Pclass,Age)) + geom_boxplot(aes(group=Pclass,fill=factor(Pclass),alpha=0.4))
p1 + scale_y_continuous(breaks= seq(min(0), max(80),by=2))

impute_age <- function(age,class){
  out<- age
  for (i in 1:length(age)){
    if(is.na(age[i])) {
      if(class[i]==1){
        out[i] <- 37
      }else if (class[i]==2){
        out[i]<- 29
      }else{
        out[i] <- 24
      }
    }else{
      out[i]<- age[i]
    }
  }
  return(out)
}

fixed.ages<- impute_age(df.train$Age, df.train$Pclass)
df.train$Age<- fixed.ages

missmap(df.train, main='Titanic Training Data-Missing Map', col=c("yellow","black"),legend=FALSE)

str(df.train)

library(dplyr)
df.train<- select(df.train,-PassengerId, -Name, -Ticket, -Cabin)
head(df.train,3)
str(df.train)

df.train$Survived <- factor(df.train$Survived)   #converting into factor
df.train$Pclass <- factor(df.train$Pclass)
df.train$Parch <- factor(df.train$Parch)
df.train$SibSp <- factor(df.train$SibSp)
str(df.train)

#Training the model
library(caTools)
set.seed(101)
split=sample.split(df.train$Survived, SplitRatio=0.70)
final.train=subset(df.train,split==TRUE)
final.test=subset(df.train,split==FALSE)
final.log.model <- glm(formula=Survived ~ . , family=binomial(link='logit'), data=final.train)
summary(final.log.model)

#Check the Prediction Accuracy
fitted.probabilities <- predict(final.log.model,newdata=final.test, type='response')

#Calculate from predicted values
fitted.results<- ifelse(fitted.probabilities >0.5,1,0)
misClasificError <- mean(fitted.results != final.test$Survived)
print(paste('Accuracy:',1-misClasificError))

#Creating the confusion matrix
cf <- table(test_data$admit, fitted.probabilities > 0.5)
cf

