#'Test of online algorithm
#'start with glmspeed to determine coefficients
#'change to logit/expit plogis/qlogis in a continu value
#'use the coefficients as a starting point for gradient descent
#'save the coefficients and turn the out come to a logistic regression prediction
#'
#' read csv file (read.csv("csvfilename.csv"))
library(speedglm)
titanic=read.csv("train_titanic.csv")

#different sets
train_titanic_1=titanic[0:700,]
train_titanic_2=titanic[701:891,]
train_titanic_3=titanic[10:10,]

#generate the first model based on the speedglm package
glm_model<- speedlm(y~x1+x2,train_titanic_1,family = binomial(logit))
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
  y_pred=0
  y_pred <- coef %*% t(cbind(intercept = 1, row[-1]))
  #convert the result into a logit function
  yhat_return=plogis(y_pred)  
  return(yhat_return)
}

#Gradient descent function
gradDescent<-function(y, x,theta,alpha){
    #calculate the adjustment of the theta and intercept based on one record 
    #(coefficients are a combination of theta and intercept )
    n <- length(y)
    # this is a vectorized form for the gradient of the cost function
    theta_new <- theta - alpha*(1/n)*((x)%*%(x%*%theta - y))
    results=theta_new
    return(results)
}
#Online Gradient descent 
gradientdescent_osl<-function (row,coef,alpha){
  #prediction of the yhat using the former coefficients
  row_m=as.matrix(row)
  coef_m=as.matrix(coef)
  y=row_m[1]
  x=row_m[,-1]
  x_i=c(intercept=1,x)
  #browser()
  results=gradDescent (y, x_i,coef_m,alpha)
  return(results)
}  

#determine the new coefficients
#to do:plot learning curve

coef=gradientdescent_osl(train_titanic_3,coef,0.001)
print(coef)

y_pred=predict_osl(train_titanic_3,coef_new)
print(y_pred)

#y_pred_old=predict_osl(train_titanic_3,coef)
#print(y_pred_old)








