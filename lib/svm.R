SVM <- function(train,test){
  time1 <- Sys.time()
  library(e1071)
  library(dplyr)
  
  model_svm <- svm(x = train[,-1], y = train[,1],   
                 kernel="radial", scale = F, type = "C-classification")
  
  # Prediction
  prediction_svm <- predict(model_svm,test[,-1])
  time2 <- Sys.time()
  return(list(prediction = prediction_svm,time = time2 - time1))
  
}