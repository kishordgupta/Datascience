library("caret")
library("e1071")
library("nnet")
data <- read.csv("C:/Users/ASUS/Dropbox/New folder (2)/New folder/training.csv")
samplesize = 0.5 * nrow(data)
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
RMSE.NN = (sum((datatest$Result - predict_testNN)^2) / nrow(datatest)) ^ 0.5
plot(predict_testNN)
confusionMatrix(as.factor(abs(round(predict_testNN))),as.factor(datatest$Result))
RMSE.NN
precision <- posPredValue(as.factor(abs(round(predict_testNN))),  as.factor(datatest$Result), positive="1")
recall <- sensitivity(as.factor(abs(round(predict_testNN))),  as.factor(datatest$Result), positive="1")
F1 <- (2 * precision * recall) / (precision + recall)
F1
recall
precision 