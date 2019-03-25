library(rpart)
library(rpart.plot)
library(caTools)
library(plyr)
library(ggplot2)
library(EnvStats)
library(Math)

getwd()
setwd("D:\\APDS\\Project\\correl")
#setwd("D:\\Murali\\APDS\\Project\\Rcode\\cov")



#function to generate the consolidated file
#a => commodity price file - "Oil Price.csv"
#b => sentiment data file - "Sentiment_data_with_date.csv"
#c => commodity type - "oil"
#d => date format "%b%d,%Y" or "%d-%b-%y"

consolidatefileGen.fun <- function(a,b,c,d)
{
  
po1  <- read.csv(a)
names(po1)<-c("Date","Price","Open","High","Low","Volume","Change")
po1$Price = as.numeric(gsub(",","",as.character(po1$Price)))

po1$Date<-as.Date(po1$Date,d)
po1$week<-format(po1$Date,"%W")
po1$weekyr<-paste(format(po1$Date,"%Y"), po1$week,sep = "")
poprice <-  aggregate(po1$Price, list(po1$weekyr),mean)
poprice1 <- as.data.frame(poprice)
head(poprice1)
names(poprice1)<-c("weekyr","AvgPrice")
head(poprice1)

#---- Weekly CHANGE DATA 


po2  <- read.csv(a)
names(po2)<-c("Date","Price","Open","High","Low","Volume","Change")
po2$Price = as.numeric(gsub(",","",as.character(po2$Price)))

po2$Date<-as.Date(po2$Date,d)
po2$week<-format(po2$Date,"%W")
po2$weekyr<-paste(format(po2$Date,"%Y"), po2$week,sep = "")
po2$change <- as.numeric(substr(po2$Change,1,4))  ####?Why % is remvoved
pogrowth <-  aggregate(po2$change,list(po2$weekyr),sum)
pogrowth1 <- as.data.frame(pogrowth)
names(pogrowth1)<-c("weekyr","Growth")
head(pogrowth1)

#---- Merging of Current PRICE DATA and Change Data

poagg1 <-merge(poprice1,pogrowth1,by = "weekyr")



#---- SENTIMENT DATA

so1  <- read.csv(b)

#take only oil sentiment data
senti_oil <- subset(so1, Commodity == c, select=c(Commodity, Date, polarity, subjectivity, Sentiment_Type))

senti_oil$Date<-as.Date(senti_oil$Date,"%d.%m.%Y")
#head(so1)
#head(so1[so1$Date=="2013-01-01",])

#create week yr column
senti_oil$week<-format(senti_oil$Date,"%W")
senti_oil$weekyr<-paste(format(senti_oil$Date,"%Y"),senti_oil$week,sep = "")
table(senti_oil$Sentiment_Type)

#count the sentiment type count for each week                  
soagg <- aggregate(senti_oil$Sentiment_Type,list(senti_oil$weekyr),table)
soagg1 <-as.data.frame(soagg)
head(soagg1)
names(soagg1)


soagg2 <-as.data.frame(as.matrix(soagg1)) 
dim(soagg2 )
head(soagg2)
head(senti_oil)
names(soagg2)<-c("weekyr","Negative","Neutral","Positive")
head(soagg2)
#soagg2$Negative=as.numeric(as.character(soagg2$Negative))

polarity_pos_avg = function(x){
  pos <- mean(subset(x,x>0))
#  neg <- sum(subset(x,x<0))
#  return(list(pos_sum = pos,neg_sum = neg))
  return(pos)
}

polarity_neg_avg = function(x){
  neg <- mean(subset(x,x<0))
  #  return(list(pos_sum = pos,neg_sum = neg))
  return(neg)
}
#Take only polarity and weekyr. Aggreate the polarity score for each week
senti_oil_polarity <- subset(senti_oil, Commodity == c, select=c(Commodity, polarity, weekyr))
senti_oil_pagg_pos <- aggregate (senti_oil_polarity$polarity,list(senti_oil_polarity$weekyr),FUN = polarity_pos_avg)
names(senti_oil_pagg_pos)<-c("weekyr","Polarity_pos_avg")
senti_oil_pagg_neg <- aggregate (senti_oil_polarity$polarity,list(senti_oil_polarity$weekyr),FUN = polarity_neg_avg)
names(senti_oil_pagg_neg)<-c("weekyr","Polarity_neg_avg")


senti_oil_pagg <- merge(senti_oil_pagg_pos,senti_oil_pagg_neg,by="weekyr")

names(senti_oil_pagg)<-c("weekyr","Polarity_pos_avg","polarity_neg_avg")

#take only subjectivity and weekr and aggreate the subjective score for each week
senti_oil_sub <- subset(senti_oil, Commodity == c, select=c(Commodity, subjectivity, weekyr))
senti_oil_subagg <- aggregate (senti_oil_sub$subjectivity,list(senti_oil_sub$weekyr),FUN = mean)
names(senti_oil_subagg)<-c("weekyr","subjectivity_avg")


# create Final OIl Data

#merge sentiment count, polarity score and subjective score with the price date
oil1 <-merge(senti_oil_subagg,soagg2,by = "weekyr")
oil2 <- merge(senti_oil_pagg,oil1,by = "weekyr")
oil <- merge(poagg1,oil2, by = "weekyr")
 
#calculate weighted mean
i=1
while(i<=nrow(oil))
{

  pos_cnt <- as.numeric(as.character(oil$Positive[i]))
  neu_cnt <- as.numeric(as.character(oil$Neutral[i]))
  neg_cnt <- as.numeric(as.character(oil$Negative[i]))
  
  oil$Polarity_weighted_avg[i] <- weighted.mean(x=c(oil$Polarity_pos_avg[i],0,oil$polarity_neg_avg[i]),c(pos_cnt,neu_cnt,neg_cnt))

i = i+1
}

head(oil)
str(oil)

##### lag up down for the next week..

nextwkPrice <- c(oil$AvgPrice[-1],NA)
oil$updownnextwk <- ifelse((nextwkPrice-oil$AvgPrice) >= 0, 1,0)

#create the weekly considated file
write.csv(oil, file = paste("consolidatedWeekly",a,sep=""))

#return the considated data. It is not used.
return(oil)
 
}


#oil correlation "%d-%b-%y"
ans = consolidatefileGen.fun("Oil Price.csv","Sentiment_data_with_date.csv","oil","%b%d,%Y" )   

#gold correlation "%b%d,%Y", "%b %d,  %Y"
gans = consolidatefileGen.fun("Gold Price.csv","Sentiment_data_with_date.csv","gold","%b%d,%Y" ) 

#Silver correlation
silans = consolidatefileGen.fun("Silver Price.csv","Sentiment_data_with_date.csv","silver","%b%d,%Y")

#NG correlation
Ngans = consolidatefileGen.fun("Natural Gas Price.csv","Sentiment_data_with_date.csv","natural gas","%b%d,%Y")


