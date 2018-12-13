library(rpart)
library(caret)
dataad  <- read.csv("C:/Users/kishor/OneDrive - The University of Memphis/Data_SCI_FINAL/training.csv")
dataa <- na.omit(dataad)

test  <- read.csv("C:/Users/kishor/OneDrive - The University of Memphis/Data_SCI_FINAL/training.csv")
test <- na.omit(test)
# grow tree 
fit <- rpart(dataa$Result ~ dataa$I1 + dataa$I2 + dataa$I3 + dataa$I4 + 
               dataa$Sprc  + dataa$Dep + dataa$PRC
             + dataa$SPECcode + dataa$SpecNum,method="class", dataa)

#printcp(fit) # display the results 
#plotcp(fit) # visualize cross-validation results 
#summary(fit) # detailed summary of splits

pfit<- prune(fit, cp=   fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])

# plot the pruned tree 
plot(pfit, uniform=TRUE, 
     main="Pruned Classification Tree for Leaky Data")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)
pred = predict(pfit, type="class")
table(pred)
table(pred,dataa$Result)
a<-confusionMatrix(pred,as.factor(dataa$Result),mode = "prec_recall", positive="1")
a
pred_unseen = predict(fit, newdata=test,type='class')
table(pred_unseen)
table(pred_unseen,test$Result)
b<-confusionMatrix(pred_unseen,as.factor(test$Result),mode = "prec_recall", positive="1")
b

library(rpart.plot)
rpart.plot(fit, extra = 106)
