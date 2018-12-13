
accesslogml_training <- read.csv("C:/Users/kishor/OneDrive - The University of Memphis/Data_SCI_FINAL/training.csv")
#View(accesslogml_training)
 
data = accesslogml_training 
samplesize = 0.7 * nrow(data)
  set.seed(80)
 index = sample( seq_len ( nrow ( data ) ), size = samplesize )
 

      datatrain = data[ index, ]
     datatest = data[ -index, ]
      max = apply(data , 2 , max)
      min = apply(data, 2 , min)
      scaled = as.data.frame(scale(data, center = min, scale = max - min))
 library(neuralnet)

 trainNN = scaled[index , ]
 testNN = scaled[-index , ]
 
 
 NN = neuralnet(data$Result ~ data$Amm +data$I1 + data$I2 + data$I3 + data$I4 + 
                  data$Sprc + data$Contract + data$Dep
                + data$SPECcode + data$SpecNum+ data$PRC, trainNN, hidden =c( 5, 3) , linear.output = T )
 
 plot(NN)
 
 summary(NN)
 
 predict_testNN = compute(NN, testNN[,c( "Amm",  
   "I1", "I2", "I3",   
   "I4","Sprc","Contract","Dep","SPECcode","SpecNum","PRC")
   ])
 predict_testNN = (predict_testNN$net.result * (max(data$Result) - min(data$Result))) + min(data$Result)
 
 #plot(datatest$result, predict_testNN, col='blue', pch=16, ylab = "predicted result NN", xlab = "real result")
 
 #abline(0,1)
 
 # Calculate Root Mean Square Error (RMSE)
 RMSE.NN = (sum((datatest$Result - predict_testNN)^2) / nrow(datatest)) ^ 0.5
 #table(datatest$result,predict_testNN)
 plot(predict_testNN)
 library("caret")
 library("e1071")
 confusionMatrix(as.factor(abs(round(predict_testNN))),as.factor(datatest$Result))
 RMSE.NN

 #test_pred <- predict(NN, newdata = testNN) 

 precision <- posPredValue(as.factor(abs(round(predict_testNN))),  as.factor(datatest$Result), positive="1")

 recall <- sensitivity(as.factor(abs(round(predict_testNN))),  as.factor(datatest$Result), positive="1")
 
 F1 <- (2 * precision * recall) / (precision + recall)
 F1
 recall
precision 
