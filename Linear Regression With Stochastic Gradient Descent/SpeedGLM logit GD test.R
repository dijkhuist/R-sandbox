#'Test of online algorith
#'start with glmspeed to determine coefficients
#'change to logit/expit plogis/qlogis in a continu value
#'use the coefficients as a starting point for gradient descent
#'save the coefficients and turn the out come to a logistic regression prediction
#'
#' read csv file (read.csv("csvfilename.csv"))

titanic=read.csv("train_titanic.csv")

#divide dataset in three subsets (subset(datasetname,factor <= or == or > or < value))
train_titanic_1=titanic[0:700,]
train_titanic_2=titanic[701:891,]
train_titanic_3=titanic[10:10,]

#generate the first model based on the speedglm package
glm_model<- speedglm(y~x1+x2,train_titanic_1,family = binomial(logit))
summary(glm_model)
#predict the result based on the model
glm_model_pred=predict(glm_model,newdata=train_titanic_3,type="response")
#determine the yhat with a threshold of 0.5
yhat <- ifelse(glm_model_pred > 0.5,1,0)
#determine the coefficients of the model
coef=(coef(glm_model))


# function to predict with the coefficients, it is a logit function so the final result is converted 
predict_osl<-function(row,coef){
  row_m=as.matrix(row)
  coef_m=as.matrix(coef)
  #determine number of features
  n=ncol(row)-1
  #determine the value of the intercept
  yhat_intercept = coef_m[1]
  y_pred=0
  #calculate the prediction of y
  for (i in 1:n){
    y_pred = y_pred + coef_m[i + 1] * row_m[i+1]
  }
  #convert the result into a logit function
  yhat_return=plogis(yhat_intercept+y_pred)  
  return(yhat_return)
}

#Gradient descent function
gradDescent<-function(y, x,intercept,theta,yhat,alpha){
    #calculate the adjustment of the theta and intercept based on one record 
    #(coefficients are a combination of theta and intercept )
    n <- length(y)
    # this is a vectorized form for the gradient of the cost function
    theta_new <- theta - alpha*(1/n)*((x)%*%(x%*%theta - y))
    intercept_new <- intercept - alpha * ((1 / n) * (yhat - y))
    results=c(intercept_new,theta_new)
    return(results)
}
#Online Gradient descent 
gradientdescent_osl<-function (row,coef,alpha){
  #prediction of the yhat using the former coefficients
  prev_yhat=predict_osl(row,coef)
  
  row_m=as.matrix(row)
  coef_m=as.matrix(coef)
  #determine intercept
  intercept=coef_m[1]
  #determine the coefficients (or theta)
  coef_x=coef_m[-1,]
  #the values of the row
  y=row_m[1]
  x=row_m[,-1]
  results=gradDescent (y, x,intercept,coef_x,prev_yhat,alpha)
  return(results)
}  

#determine the new coefficients
coef_new=gradientdescent_osl(train_titanic_3,coef,0.0001)
print(coef_new)

y_pred=predict_osl(train_titanic_3,coef_new)
print(y_pred)

#y_pred_old=predict_osl(train_titanic_3,coef)
#print(y_pred_old)








