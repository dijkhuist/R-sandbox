#determine g0(a,w)
g<-glm(A~w2+w3+w4,family = binomial)
g1W<-predict(g,type="response")
cat("\propensity score =g1W","\n");
#clever covariate and fluctuating/substitution parameters
#HAW is a switch the influence of W on the observed A,H1W and H0W are the counterfactuals 
h<-cbind(HAW=(A/g1W-(1-A)/(1-g1W)),H1W=(1/g1W),H0W=(-1/(1-g1W)))
#Y~-1 means no intercept, offset gives the feature a coefficient of 1
epsilon<-coef(glm(Y~-1+h[,1]+offset(Q[,"QAW"]),family=binomial))
#epsilon=0.00336
epsilon
Qstar=plogis(Q+epsilon*h)
psi_star<-(Qstar[,'Q1W']-Qstar[,'Q0W'])
Psi_star<-mean(Qstar[,'Q1W']-Qstar[,'Q0W'])
#TMLE_Psi_star: 0.1967346
cat("TMLE_Psi_star:",Psi_star)
#True_Psi=0.198

cat("\n TMLE.SI_bias: ",abs(True_Psi-Psi_star))
cat("\ relative TMLE.SI_bias: ", (abs(True_Psi-Psi_star)/True_Psi)*100,'%')
df<-round(cbind(Q0=(Q0),H=(h),epsilon,psi_star),digits=4)

Q<-as.data.frame(Q)
Qstar<-as.data.frame(Qstar)
#influence curve 
IC<-h[,1]*(Y-plogis(Q$QAW))+plogis(Qstar$Q1W-Qstar$Q0W)-Psi_star
summary(IC)
n<-nrow(ObsData)
varHat.IC