## Creating index variable 

# Read the Data
titanic = read.csv("train_titanic.csv", header=T)

## Scale data for neural network

max = apply(titanic[,2:3] , 2 , max)
min = apply(titanic[,2:3], 2 , min)

scaled = as.data.frame(scale(titanic[,2:3], center = min, scale = max - min))
y=titanic$y
scaled_titanic = cbind(y,scaled)
#different sets
train_titanic_1=scaled_titanic[0:700,]
train_titanic_2=scaled_titanic[701:891,]


## Fit neural network 

# install library
#install.packages("neuralnet ")

# load library
library(neuralnet)



# fit neural network
set.seed(2)
NN = neuralnet(y~x1+x2, train_titanic_1, hidden = 6 , linear.output = T )

# plot neural network
plot(NN)


# save the model to disk
saveRDS(NN, "./final_NN.rds")

# later...

# load the model
final_NN <- readRDS("./final_NN.rds")
plot(final_NN)
#new data
NN_adjusted <- neuralnet(y~x1+x2,train_titanic_2,hidden = 3 , linear.output = T, startweights = final_NN$weights)

plot(NN_adjusted)

# Compute Predictions
NN_adjusted$net.result <- sapply(NN_adjusted$net.result,round,digits=0)
table(train_titanic_2$y,NN_adjusted$net.result)
NN$net.result<-sapply(NN$net.result,round,digits=0)
table(train_titanic_1$y,NN$net.result)

plot(NN)
plot(NN_adjusted)
