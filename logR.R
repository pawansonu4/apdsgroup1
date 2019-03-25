
library(rpart)
library(rpart.plot)
library(caTools)
library(plyr)
library(ggplot2)
library(caret)
library(fmsb)


getwd()
setwd("D:\\APDS\\Project\\correl\\input_data")
#setwd("D:\\Murali\\APDS\\Project\\Rcode\\cov")

norm.fun = function(x){ 
  (x - min(x))/(max(x) - min(x)) 
  
}

logr.func<-function(a)
{
#ipdata<-read.csv("consolidatedWeeklySilver_lag3 Price.csv")
  
 ipdata<-read.csv(a)
 ipdata[is.na(ipdata)] <- 0 ##### some of the rows had NA for positive, negative or neutral count, instead of NA I have changed it to 0
# ipdata <- apply(ipdata, 2, norm.fun) ##### Here I had tried to normalize the data - but it not improved anything
# ipdata <- as.data.frame(ipdata)
#ipdata$UpDown <- ifelse(ipdata$Growth > 0,1,0)
head(ipdata)

# LogR - Data Partitioning
set.seed(1000)
split = sample.split(ipdata, SplitRatio =0.75) ####? mur - why seed. Random split
lgtrain = subset(ipdata, split==TRUE)
lgtest = subset(ipdata, split==FALSE)

#index <-  round(nrow(ipdata) * 0.8)

#lgtrain <- ipdata[1:index,]
#lgtest <- ipdata[-(1:index),]

nrow(lgtrain)
nrow(lgtest)
str(lgtest)

str(ipdata)

#lgmodel = glm(updownnextwk ~ Polarity_pos_avg+polarity_neg_avg, data=lgtrain,  family="binomial")
#lgmodel = glm(updownnextwk ~ ., data=lgtrain,  family="binomial")
#lgmodel = glm(updownnextwk ~ weekyr+AvgPrice+Growth+Polarity_pos_avg+polarity_neg_avg+subjectivity_avg+Negative+Neutral+Positive+Polarity_weighted_avg, data=lgtrain,  family="binomial")
lgmodel = glm(updownnextwk ~ Polarity_weighted_avg, data=lgtrain,  family="binomial")
summary(lgmodel)
NagelkerkeR2(lgmodel)

lgpred = predict(lgmodel, lgtest[-12], type="response") 

#Confusion Matrix

lgpred = round(lgpred)
lgresdf = data.frame("Actual"=lgtest[12], "Predicted"=lgpred)
confMat = table(lgtest[,12], lgpred)
COnfMatDetails <- confusionMatrix(as.factor(lgpred),as.factor(lgtest$updownnextwk), positive = "1")
confMat
#five_var=sum(diag(confMat)) / nrow(lgtest)
five_var=sum(diag(confMat)) / sum(confMat)
#print(a)
print(COnfMatDetails)


return(COnfMatDetails)
}


#### Oil
final = logr.func("consolidatedWeeklyOil Price.csv")
mur<-matrix(c(final$overall[1],final$byClass),nrow = 12, ncol = 1)
rownames(mur) <- c("Accuracy","Sensitivity","specificity", "Pos Pred Value","Neg Pred Value","Precision","Recall","F1","Prevalence","Detection Rate","Detection Prevalence","Balanced Accuracy")
#print(mur)

#####Gold
final = logr.func("consolidatedWeeklyGold Price.csv")
mur<-cbind(mur,c(final$overall[1],final$byClass))
#print(mur)

#####silver
final = logr.func("consolidatedWeeklySilver Price.csv")
mur<-cbind(mur,c(final$overall[1],final$byClass))
#print(mur)

#####natural gas
final = logr.func("consolidatedWeeklyNatural Gas Price.csv")
mur<-cbind(mur,c(final$overall[1],final$byClass))
#print(mur)

colnames(mur) <- c("Oil","Gold","Silver","Natural Gas")
print(mur)


