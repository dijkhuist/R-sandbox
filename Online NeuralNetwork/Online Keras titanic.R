#install.packages("keras")

library(keras)
#tensor flow installation
#install_keras()
#load minst (digits) set, is a part of the keras installation
# Read the Data
data = read.csv("train_titanic_keras.csv", header=T)
str(data)

#change to matrix
data =as.matrix(data)
str(data)

dimnames(data)<-NULL

#normalize
data[, 2:5] <- normalize(data[,2:5])
data[,1]<-as.numeric(data[,1])

#set.seed
set.seed(1234)

ind <- sample(2, nrow(data), replace = T, prob = c(0.7, 0.3))
training <- data[ind==1,2:5]
test <- data[ind==2,2:5]

trainingtarget <- data[ind==1, 1]
testtarget <- data[ind==2, 1]

# One Hot Encoding of the final result
trainLabels <- to_categorical(trainingtarget)
testLabels <- to_categorical(testtarget)
#parameterize activation softmax/sigmoid
model <- keras_model_sequential() 
model %>% 
  layer_dense(units =8, activation = "relu", input_shape = c(4)) %>% 
  layer_dense(units = 4, activation = "relu") %>%
  layer_dense(units = 2, activation = "softmax")

summary(model)
#parameterize loss
model %>% compile(
  loss = "binary_crossentropy",
  optimizer = optimizer_rmsprop(),
  metrics = c("accuracy")
)

history <- model %>%
  fit(training,
      trainLabels,
      epoch = 200,
      batch_size = 32,
      validation_split = 0.2)


model1 <- model %>%
  evaluate(test, testLabels)
print(model1)

# Prediction & confusion matrix - test data
prob <- model %>%
        predict_proba(test)

pred <- model %>%
        predict_classes(test)
table_sigmoid <- table(Predicted = pred, Actual = testtarget)
print(table1)
print(table_binary)
print(table_sigmoid)
table2<-cbind(prob, pred, testtarget)

print(table2)
#proof for online learning
save_model_hdf5(model, 'model.hdf5', overwrite = TRUE,
                include_optimizer = TRUE)

loaded_model<-load_model_hdf5('model.hdf5', custom_objects = NULL, compile = TRUE)
history_loaded<-loaded_model%>% fit(
  test, testLabels, 
  epochs = 30, batch_size = 32, 
  validation_split = 0.2
)

prob <- loaded_model %>%
  predict_proba(test)

pred <- loaded_model %>%
  predict_classes(test)
table_loaded <- table(Predicted = pred, Actual = testtarget)
print(table_loaded)
print(table1)
