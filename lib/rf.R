RF <- function(train,test){
  time1 <- Sys.time()
  library(randomForest)
  library(caret)
  library(e1071)
  
  model_rf <- randomForest(as.factor(train[,1]) ~ .,
                         data = train[,-1],
                         importance=TRUE
                         )
  # Prediction
  prediction_rf <- predict(model_rf,test[,-1])
  time2 <- Sys.time()
  return(list(prediction = prediction_rf,time = time2 - time1))
}

