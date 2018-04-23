logi <- function(df.train, df.test, run.logi = FALSE){
  
  time1 <- Sys.time()
  
  traindata <- df.train
  testdata <- df.test
  
  model <- glm(diagnosis ~.,family=binomial(link='logit'),data=traindata)
  
  fitted.results <- predict(model,newdata=subset(testdata,select=c(2,3,4,5,6,7,8,9,10,
                            11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)),
                            type='response')
  fitted.results <- ifelse(fitted.results > 0.5,1,0)
  
  prediction_logi <- fitted.results
  #save(prediction_logi,file = "../output/prediction_logi.RData")
  #misClasificError <- mean(fitted.results != testdata$diagnosis)
  #print(paste('Accuracy',1-misClasificError))
  time2 <- Sys.time()
  
  return(list(prediction = prediction_logi,time <- time2 - time1))
}