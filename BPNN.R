library('openxlsx')
library("readxl")
library("dplyr")
library('lubridate')
library('xts')
library("neuralnet")
library('caret')
library('devtools')
library('caTools')

rm(list=ls())

data <- read.csv(file="D:/consolidatedWeeklyGold Price.csv", header=TRUE, stringsAsFactors = FALSE)
# data <- read.csv(file="D:/consolidatedWeeklyOil Price.csv", header=TRUE, stringsAsFactors = FALSE)
# data <- read.csv(file="D:/consolidatedWeeklySilver Price.csv", header=TRUE, stringsAsFactors = FALSE)
#data <- read.csv(file="D:/consolidatedWeeklyNatural Gas Price.csv", header=TRUE, stringsAsFactors = FALSE)

######Also change the neurons on line 69 as per commodity

data <- data[,c(-1,-3:-10)]

names(data) <- c('Period','Sentiment_Lag','Trend_Ahead')

#data <- mutate(data,Trend = ifelse(Change >= 0,'1',0))
#data <- data[,c(-2)]
data$Period <- as.Date(paste(data$Period,1),"%Y%U %u") 
data <- na.omit(data)
data <- xts(data[,c(2:3)], order.by = data$Period )
data_lag <- lag.xts(data, k = 1)

input_data <-  merge(data[,c(1:2)],data_lag[,2], join = 'left',fill = 0)
#  input_data <- last(input_data,"5 years")
input_data <- na.omit(input_data)
input_data <- data.frame(date=index(input_data), coredata(input_data))
input_data <- input_data[-1]

input_data[,1] <- as.numeric(as.character(input_data[,1]))
input_data[,2] <- as.numeric(as.character(input_data[,2]))
input_data[,3] <- as.numeric(as.character(input_data[,3]))
# input_data[,4] <- as.numeric(as.character(input_data[,4]))
# input_data[,5] <- as.numeric(as.character(input_data[,5]))
# input_data[,6] <- as.numeric(as.character(input_data[,6]))
# input_data[,7] <- as.numeric(as.character(input_data[,7]))
# input_data[,8] <- as.numeric(as.character(input_data[,8]))

# Normalize the data for NN
norm.fun = function(x){ 
  (x - min(x))/(max(x) - min(x)) 
  
}

scaled <- apply(input_data, 2, norm.fun)
scaled <- as.data.frame(scaled)
names(scaled) <- c(names(scaled[1:2]),'Trend_Lag')

set.seed(1000)
split = sample.split(scaled$Sentiment_Lag, SplitRatio =0.75)
training_data <- subset(scaled, split==TRUE)
test_data <- subset(scaled, split==FALSE)

# index <-  round(nrow(scaled) * 0.8)
# training_data <- scaled[1:index,]
# test_data <- scaled[-(1:index),]

# split = sample.split(scaled, SplitRatio =0.80) ####? mur - why seed. Random split
# training_data = subset(scaled, split==TRUE)
# test_data = subset(scaled, split==FALSE)



i <- c(3)  # Use for gold
#i <- c(1)  # Use for Oil
#i <- c(1)  # Use for Silver
#i <- c(2,1)  # Use for Natural Gas



nn <- neuralnet(Trend_Ahead  ~  Sentiment_Lag + Trend_Lag, training_data, 
                hidden =  i, 
                learningrate = 0.01, threshold = 0.1, rep = 100)

# # Plot the neural net
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
plot.nnet(nn)


# # # Predict the test data and plot confusion matrix
pr.nn <- compute(nn, test_data[,c(-2)])

original <- as.data.frame(test_data[,c(1,2)])
predicted <- as.data.frame(round(pr.nn$net.result))
predicted <- mutate(predicted, Trend = ifelse(V1 == 1, 'Up', 'Down'))
original  <- mutate(original, Trend = ifelse(Trend_Ahead == 1, 'Up', 'Down'))
original$Trend <- as.factor(original$Trend)
predicted$Trend <- as.factor(predicted$Trend)
table(original$Trend)
cm <- confusionMatrix( predicted$Trend,original$Trend)
cm$table

#### Plot the confusion matrix
fourfoldplot(cm$table, color = c("#ffcccc", "#88cc00"), main = 'Confusion matrix for test data')

#### Plot the confusion matrix using ggplot
data <- as.data.frame(cm$table)
names(data) <- c('Predicted', 'Actual', 'Freq')
ggplot(data =  data, mapping = aes(x = Predicted, y = Actual)) +
  geom_tile(aes(fill = Freq), colour = "white") +
  geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1, size = 10) +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_bw() + theme(legend.position = "none") +  theme(text = element_text(size=20))



###### Accuracy
PCC <- cm$overall[1]

total_down <- nrow(filter(original, Trend == 'Down'))
pred_down <- cm$table[1,1]

total_up <- nrow(filter(original, Trend == 'Up'))
pred_up <- cm$table[2,2]

correct_down <- pred_down / total_down
correct_up <- pred_up / total_up
accuracy <- cm$overall[1]

correct_up
correct_down
accuracy






