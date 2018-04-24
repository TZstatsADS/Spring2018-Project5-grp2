################################----Train the model with svm----##############################
SVM <- function(train,test){
  # Input: train--training data
  #        test--test data
  
  # Step 0: library packages
  library(e1071)
  library(dplyr)
  time1 <- Sys.time()
  # Step 1: train the model with training data 
  model_svm <- svm(x = train[,-1], y = train[,1],   
                 kernel="radial", scale = F, type = "C-classification")
  
  # Step 2:Prediction
  prediction_svm <- predict(model_svm,test[,-1])
  time2 <- Sys.time()
  return(list(prediction = prediction_svm,time = time2 - time1))
  
}