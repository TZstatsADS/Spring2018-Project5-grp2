################################----Train the model with random forest algorithm----##############################

RF <- function(train,test){
  # Input: train--training data
  #        test--test data
  
  # Step 0: library packages
  library(randomForest)
  library(caret)
  library(e1071)
  time1 <- Sys.time()
  # Step 1: train the model with training data 
  model_rf <- randomForest(as.factor(train[,1]) ~ .,
                         data = train[,-1],
                         importance=TRUE
                         )
  # Step 2:Prediction
  prediction_rf <- predict(model_rf,test[,-1])
  time2 <- Sys.time()
  return(list(prediction = prediction_rf,time = time2 - time1))
}

