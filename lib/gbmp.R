gbmp <- function(df.train, df.test, run.gbm = FALSE){
  
  time1 <- Sys.time()
  
  traindata <- df.train
  testdata <- df.test
  
  library(gbm)
  
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
  
  optimalTreeNumberPredictionCV = gbm.perf(gbmCV)
  
  gbmTest = predict(object = gbmCV,
                    newdata = testdata,
                    n.trees = optimalTreeNumberPredictionCV,
                    type = "response")
  
  prediction_gbm <- round(gbmTest,0)
  
  #save(prediction_gbm,file = "../output/prediction_gbm.RData")
  
  time2 <- Sys.time()
  
  return(list(prediction = prediction_gbm,time <- time2 - time1))
}