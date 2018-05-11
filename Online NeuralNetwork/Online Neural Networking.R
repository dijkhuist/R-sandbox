## Creating index variable 

# Read the Data
data = read.csv("cereals.csv", header=T)

# Random sampling
samplesize = 0.60 * nrow(data)
set.seed(80)
index = sample( seq_len ( nrow ( data ) ), size = samplesize )
print((nrow(data)))
# Create training and test set
datatrain = data[ index, ]
datatest = data[ -index, ]

## Scale data for neural network

max = apply(data , 2 , max)
min = apply(data, 2 , min)
scaled = as.data.frame(scale(data, center = min, scale = max - min))

## Fit neural network 

# install library
#install.packages("neuralnet ")

# load library
library(neuralnet)

# creating training and test set
trainNN = scaled[index , ]
testNN = scaled[-index , ]

# fit neural network
set.seed(2)
NN = neuralnet(rating ~ calories + protein + fat + sodium + fiber, trainNN, hidden = 3 , linear.output = T )

# plot neural network
plot(NN)

## Prediction using neural network

predict_testNN = compute(NN, testNN[,c(1:5)])
predict_testNN = (predict_testNN$net.result * (max(data$rating) - min(data$rating))) + min(data$rating)

plot(datatest$rating, predict_testNN, col='blue', pch=16, ylab = "predicted rating NN", xlab = "real rating")

abline(0,1)
#rmse
# Calculate Root Mean Square Error (RMSE)
RMSE.NN = (sum((datatest$rating - predict_testNN)^2) / nrow(datatest)) ^ 0.5
print (RMSE.NN)
# save the model to disk
saveRDS(NN, "./final_NN.rds")

# later...

# load the model
final_NN <- readRDS("./final_NN.rds")
print(final_NN)
#new data
NN_adjusted <- neuralnet(rating ~ calories + protein + fat + sodium + fiber,
                         testNN,hidden = 3 , linear.output = T, startweights = final_NN$weights)

## Prediction using neural network

predict_testNN_adj = compute(NN_adjusted, testNN[,c(1:5)])
predict_testNN_adj = (predict_testNN_adj$net.result * (max(data$rating) - min(data$rating))) + min(data$rating)

plot(datatest$rating, predict_testNN_adj, col='blue', pch=16, ylab = "predicted rating NN ADJ", xlab = "real rating")

abline(0,1)

# Calculate Root Mean Square Error (RMSE)
RMSE.NN_ADJ = (sum((datatest$rating - predict_testNN_adj)^2) / nrow(datatest)) ^ 0.5
plot(NN)
plot(final_NN)
plot(NN_adjusted)
