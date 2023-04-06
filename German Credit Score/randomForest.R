## Random forest

#install.packages("randomForest")
#install.packages("caret")
#install.packages("ROCR")
library(ISLR)
data("Carseats")
Carseats
Data <- Carseats[sample(nrow(Carseats)),]
attach(Data)

Data$Target <- as.factor(ifelse(Sales >= 8, "High","Low"))


library(randomForest)
ntree <- 100

rf <- randomForest(Target~., data=Data, ntree = ntree , mtry = sqrt(ncol(Data)-1), 
             proximity = T , importance = T)
print(rf)

rf$proximity

rf$importance

importance(rf , type = 1)

importance(rf , type = 2)

varImpPlot(rf)   ## Variable imp plot


rf$err.rate[ ntree , 1]      ## OOB rate
print(rf)

rf$predicted ## to get the predicted lable

CM <- table(rf$predicted,Data$Target , dnn = c("Predicted" , " Actual"))
CM

library(caret)

confusionMatrix(rf$predicted , Data$Target , positive = "Low")

library(ROCR)

pred <- prediction(rf$votes[ , 1] , Data$Target)

#Gain Chart
perf <- performance(pred, "tpr" , "rpp")
plot(perf)

#Lift chart
perf <- performance(pred , "lift" , "rpp")
plot(perf)


#lift chart

perf <- performance(pred , )

#Roc Chart

Perf <- performance(pred , "tpr","fpr")
plot(Perf)

# AUC


## function

myAvg <- function( x )
{
   Mymean <- sum(x) / length(X)                
                return(Mymean)
}

x <- 1 : 20
x
myAvg(x)         


                
                













