adaboost <- function(dat_train,dat_test,run.ada = FALSE,iter = 70){
  time1 <- Sys.time()
  library(rpart)
  library(ada)
  library(caret)
  
  # Train the model using adaboost model
  control <- rpart.control(cp = -1, maxdepth = 14,maxcompete = 1,xval = 0)
  model_ada <- ada(diagnosis~., data = dat_train, test.x = dat_train[,-1], test.y = dat_train[,1], type = "gentle", control = control, iter = iter)
  
  # Prediction
  prediction_ada <- predict(model_ada, dat_test[,-1])
  save(prediction_ada,file = "../output/prediction_ada.RData")
  
  time2 <- Sys.time()
  return(list(prediction = prediction_ada,time <- time2 - time1))
  
}

