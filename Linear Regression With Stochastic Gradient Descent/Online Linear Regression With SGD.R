filename='subset_wine.txt'
dataset<-data.frame(read.csv(file=filename,sep=','))
for (row in dataset){ print(row)}

length(list(0,1,2,3,4))

#determine coefficients using SGD
coeff_sgd <-function (train,l_rate,n_epoch){
  coef =0.0 for i in range(length(train))
     
}