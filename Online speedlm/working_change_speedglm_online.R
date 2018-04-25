#climate change 
#
library(speedglm)

require(methods)
# read csv file (read.csv("csvfilename.csv"))
titanic=read.csv("train_titanic.csv")

#divide dataset in three subsets (subset(datasetname,factor <= or == or > or < value))
train_titanic_1=titanic[0:500,]
train_titanic_2=titanic[501:650,]
train_titanic_3=titanic[651:891,]

#lmfit1  <- speedlm(Survived~Pclass+Gender, train_titanic_1)
#lmfit2 <- updateWithMoreData(lmfit1, train_titanic_2)
#lmfit3 <- updateWithMoreData(lmfit2, train_titanic_3)
#yhat=predict(lmfit2,newdata = train_titanic_3)
#train_titanic_3_pred=data.frame(train_titanic_3,yhat)

lmfit1_lin  <- speedlm(Survived~Pclass+Gender,train_titanic_1)
lmfit2_lin <-updateWithMoreData(lmfit1_lin, train_titanic_2)
yhat_value=predict(lmfit2_lin,newdata = train_titanic_3)
yhat<-ifelse (yhat_prob >0.5,1,0)
yhat_prob<-predict(lmfit2_lin, newdata=train_titanic_3,type="response")
yhat_prob
train_titanic_3_pred_lin=data.frame(train_titanic_3,yhat)

lmfit1
lmfit2
lmfit3
print(summary(lmfit2))

#response testing...
titanic=read.csv("train_titanic.csv")
lmfitglm_lin  <- speedglm(Survived~Pclass+Gender,train_titanic_1,family = binomial('logit'))
#response is not working !
yhatglm_prob<-predict(lmfitglm_lin, newdata=train_titanic_3,type="response")
yhatglm<-predict(lmfitglm_lin, newdata=train_titanic_3,type= "link")
yhatglm_prob
yhatglm
plogis