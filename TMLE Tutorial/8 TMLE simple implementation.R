#Simple TMLE
ObsData<-subset(ObsData,select=c(w1,w2,w3,w4,A,Y))
Y  <-ObsData$Y
A  <-ObsData$A
w1 <-ObsData$w1
w2 <-ObsData$w2
w3 <-ObsData$w3
w4 <-ObsData$w4

m<-glm(Y~A+w1+w2+w3+w4, family=binomial, data=ObsData)
Q<-cbind(QAW = predict(m),
         Q1W = predict(m,newdata=data.frame(A=1,w1,w2,w3,w4)),
         Q0W = predict(m,newdata=data.frame(A=0,w1,w2,w3,w4)))

Q0<-as.data.frame(Q)
Y1<-Q0$Q1W
Y0<-Q0$Q0W
QA1<-exp(Y1)/(1+exp(Y1))
QA0<-exp(Y0)/(1+exp(Y0))
#inverse logit (probability scale)
psi=(exp(Y1)/(1+exp(Y1)))-exp(Y0)/(1+exp(Y0))
Psi=mean((exp(Y1)/(1+exp(Y1)))-exp(Y0)/(1+exp(Y0)))
cat("\n Psi :", Psi)
#psi=0.1870482
df<-round(cbind(Logit=(Q),Pr.Y1=QA1,Pr.Y0=QA0,Psi=psi),digits=3)
