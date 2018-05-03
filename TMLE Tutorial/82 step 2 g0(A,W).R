#determine g0(a,w)
g<-glm(A~w2+w3+w4,family = binomial)
g1W<-predict(g,type="response")
cat("\propensity score =g1W","\n");
summary(g1W)
g1W_as_dataframe=as.data.frame(g1W)
#clever covariate and fluctuating/substitution parameters
h<-cbind(gAW=(A/g1W-(1-A)/(1-g1W)),g1W=(1/g1W),g0W=(-1/(1-g1W)))
#Y~-1 means no intercept, offset gives the feature a coefficient of 1
epsilon<-coef(glm(Y~-1+h[,1]+offset(Q[,"QAW"]),family=binomial))
#epsilon=0.00336
epsilon
gAW=(A/g1W-(1-A)/(1-g1W))
gAW
