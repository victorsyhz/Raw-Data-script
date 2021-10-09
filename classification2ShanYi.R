rm(list=ls())     # clear all variables
graphics.off()    # Close all figure window(s)
cat("\014")       # clear console

# read data
Data<-read.csv("pragmatics.csv",header=T,stringsAsFactors = TRUE)
n<-nrow(Data)

# random forest

library("randomForest")
set.seed(9999);
RF<-randomForest(Pragmatics~.,Data,importance = T,localImp = T,proximity = T)
# full data
RF
RF$confusion
Imp<-RF$importance 
par(mfrow=c(2,3)) # figure of the variable importance
for(i in 1:6){
barplot(Imp[,i],main=colnames(Imp)[i])}
par(mfrow=c(1,1)) # figure of the local importance
matplot(1:(ncol(Data)-1),RF$local,type='l',xlab =' Variables',ylab = 'Local Importance')

RF.pred<-predict(RF,Data)#,type="class")
CM.RF<-confusionMatrix(RF.pred,Data$Pragmatics) # confusion matrix etc.
CM.RF

## K-fold cross validation
source("Fold.r")
kfold<-10
mm<-Fold(kfold,Data,7,6666)
error<-matrix(0,kfold) # to assign initial value 0
set.seed(9999);
for(i in 1:kfold)
  { 
    m<-mm[[i]]
    RF_cv<-randomForest(Pragmatics~.,data = Data[-m,])
    error[i]<-sum(Data[m,7]!=predict(RF_cv,Data[m,]))/length(m)
  }

error_bar<- mean(error) # average of error
Accuracy<-1-error_bar  
