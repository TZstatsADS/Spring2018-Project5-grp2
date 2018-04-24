########----Train the model with logistic model----#########

logi02 <- function(df.train2, df.test2){
  
  # Input: df.train--training data
  #        df.test--test data
  
  # Step 0
  time1 <- Sys.time()
  
  traindata <- df.train2
  testdata <- df.test2
  
  # Step 1 train the model with training data
  model <- glm(diagnosis ~.,family=binomial(link='logit'),data=traindata)
  
  # Step 2 use the trained model to do the prediction
  fitted.results <- predict(model,newdata=subset(testdata,select=c(seq(2,12, step=1))),
                            type='response')
  fitted.results <- ifelse(fitted.results > 0.5,1,0)
  
  prediction_logi <- fitted.results
  
  time2 <- Sys.time()
  
  return(list(prediction = prediction_logi,time <- time2 - time1))
}