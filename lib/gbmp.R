########----Train the model with gbm model----#########

gbmp <- function(df.train, df.test){
  
  # Input: df.train--training data
  #        df.test--test data
  
  # Step 0: library packages
  time1 <- Sys.time()
  
  traindata <- df.train
  testdata <- df.test
  
  # library(gbm)
  
  # Step 1: use training data to do the cross validation and train the model
  n<-names(traindata)

  gbm.form <- as.formula(paste("diagnosis ~", 
                       paste(n[!n %in% "diagnosis"], 
                       collapse = " + ")))
  
  gbmCV = gbm(formula = gbm.form,
              distribution = "bernoulli",
              data = traindata,
              n.trees = 500,
              shrinkage = .1,
              n.minobsinnode = 15,
              cv.folds = 5,
              n.cores = 1)
  
  # In order to find the best number of trees to use for the prediction for the test data, we can use `gbm.perf` function. 
  # This function returns the optimal number of trees for prediction.
  optimalTreeNumberPredictionCV = gbm.perf(gbmCV)
  # Here is optimal number of trees selected by cv is 197 for df.train,
  # For df.train2, it is 104
  
  # Step 2: prediction
  gbmTest = predict(object = gbmCV,
                    newdata = testdata,
                    n.trees = optimalTreeNumberPredictionCV,
                    type = "response")
  
  prediction_gbm <- round(gbmTest,0)
  
  
  time2 <- Sys.time()
  
  return(list(prediction = prediction_gbm,time = time2 - time1))
}