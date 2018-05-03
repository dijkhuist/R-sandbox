# The simulated data replicationg the DAG in Figure 1:
#1. Y: mortality binary indicator (1 death, 0 alive)
#2. A: binary treatment (1 Chemotherapy, 0 Radiotherapy )
#3. W1: Gender (1 male; 0 female)
#4. W2: Age at diagnosis (0 <65; 1 >=65)
#5. W3: Cancer TNM classification (scale from 1 to 4; 1: early stage no metastasis; 4:
#                                    advanced stage with metastasis)
#6. W4: Comorbidities (scale from 1 to 5)
options(digits=4)
generateData<-function (n) {
  w1<-rbinom(n,size=1,prob=0.5)
  w2<-rbinom(n,size=1,prob=0.65)
  w3<-round(runif(n, min=0, max=4), digits =3)
  w4<-round(runif(n,min=0, max=5),digits =3)
  A<-rbinom(n, size=1, prob=plogis(-0.4+2*w2+0.15*w3+0.2*w4+0.15*w2*w4))
  Y<-rbinom(n, size=1, prob=plogis(-1+A-0.1*w1+0.3*w2+0.25*w3+0.2*w4+0.15*w2*w4))
  
  #counterfactual
  Y.1<-rbinom(n, size=1,prob=plogis(-1 +1 -0.1*w1+0.3*w2+0.25*w3+0.2*w4+0.15*w2*w4))
  Y.0<-rbinom(n, size=1,prob=plogis(-1 +0 -0.1*w1+0.3*w2+0.25*w3+0.2*w4+0.15*w2*w4))
  #return dataframe
  data.frame(w1,w2,w3,w4,A,Y,Y.0,Y.1)
}
set.seed(7777)
ObsData<-generateData(n=10000)
True_Psi<-mean(ObsData$Y.1-ObsData$Y.0);
cat("True Psi:",True_Psi)
#True_Psi=0.198
Bias_Psi<-lm(data=ObsData,Y~A+w1+w2+w3+w4)
cat("\n")
cat("\n Naive_biased_Psi: ",summary(Bias_Psi)$coef[2,1])
#Naive_Biased_psi=0.211987
Naive_Bias<-(summary(Bias_Psi)$coef[2,1]-True_Psi)
cat("\n Naive_Bias:", Naive_Bias)
#Naive_Bias: 0.01398716
Naive_Relative_Bias<-(((summary(Bias_Psi)$coef[2,1]-True_Psi)/True_Psi)*100)
cat("\n Naive_Relative_Bias:",Naive_Relative_Bias,"%")
#Naive_Relative_Bias=7.64223 %
