
###############################----Train the model with adaboost model----##############################

adaboost <- function(dat_train,dat_test,run.ada = FALSE,iter = 70){
  
  # Input--:dat_train:training data
  #         dat_test :test data
  #         nrounds  :number of rounds,with default 5000
  #         iter     :number od iteration,with default 70
  
  # Step 0:library packages
  
  time1 <- Sys.time()
  library(rpart)
  library(ada)
  library(caret)
  
  # Step 1:Train the model using adaboost model
  # We set parameters to be:cp = -1
  #                         maxdepth = 14
  #                         maxcomplete = 1
  #                         xval = 0
  control <- rpart.control(cp = -1, maxdepth = 14,maxcompete = 1,xval = 0)
  model_ada <- ada(diagnosis~., data = dat_train, test.x = dat_train[,-1], test.y = dat_train[,1], type = "gentle", control = control, iter = iter)
  
  # Step 2:Prediction
  prediction_ada <- predict(model_ada, dat_test[,-1])

  time2 <- Sys.time()
  return(list(prediction = prediction_ada,time <- time2 - time1))
  }

