
################################----Train the model with xgboost model----##############################

xgb <- function(dat_train,dat_test,run.xg = FALSE,nrounds = 5000,early_stopping_round = 100,print_every_n = 100){
  # Input--:dat_train:training data
  #         dat_test :test data
  #         nrounds  :number of rounds,with default 5000
  #         early_stopping_round:early stopping criteria for 100 steps
  #         print_every_n:the frequency of printing partial results is every 100 steps

  # Step 0:library packages
  time1 <- Sys.time()
  library(xgboost)
  library(magrittr)
  library(dplyr)
  library(ggplot2)

  # Step 1: create xgb.DMatrix objects for each trainand test set.
  xgtrain <- xgb.DMatrix(as.matrix(dat_train %>% select(-diagnosis)), label = dat_train$diagnosis)
  xgtest  <- xgb.DMatrix(as.matrix(dat_test  %>% select(-diagnosis)), label = dat_test$diagnosis)
  
  # Step 2: Set parameters
  # We will use a binary logistic objective function. The evaluation metric will be AUC.
  # we start with eta = 0.012, subsample=0.8, max_depth=8, colsample_bytree=0.9 and min_child_weight=5.
  
  params <- list("objective"        = "binary:logistic",
                 "eval_metric"      = "auc",
                 "eta"              = 0.012,
                 "subsample"        = 0.8,
                 "max_depth"        = 8,
                 "colsample_bytree" = 0.9,
                 "min_child_weight" = 5
  )
  
  #Step 3: Train the model using cross validation with 5 folds.
  model_xgb.cv <- xgb.cv(params = params,
                         data = xgtrain, 
                         maximize = TRUE,
                         nfold = 5,
                         nrounds = nrounds,
                         nthread = 1,
                         early_stopping_round = early_stopping_round,
                         print_every_n = print_every_n)
  
  ## Visualization
  d <- model_xgb.cv$evaluation_log
  n <- nrow(d)
  v <- model_xgb.cv$best_iteration
  dat <- data.frame(x=rep(d$iter, 2), val=c(d$train_auc_mean, d$test_auc_mean), set=rep(c("train", "test"), each=n))
  
  plot1 <- ggplot(data = dat, aes(x=x, y=val)) + 
    geom_line(aes(colour=set)) + 
    geom_vline(xintercept=v) + 
    theme_bw() +
    labs(title="AUC values for XGBoost with cross-validation", x="Iteration", y="AUC values")
  ggsave("xgboost_cv.pdf", plot = last_plot(), device = "pdf",path = "../figs",
         scale = 1, width = 10, height = 10, units =c("in", "cm", "mm"),
         dpi = 300, limitsize = TRUE)

  #Step 4: Train the model using xgboost model
  model_xgb <- xgboost(params = params,
                       data = xgtrain, 
                       maximize = TRUE,
                       nrounds = nrounds,
                       nthread = 1,
                       early_stopping_round = early_stopping_round,
                       print_every_n = print_every_n)
  
  ## Visualization
  d1 <- model_xgb$evaluation_log
  n1 <- nrow(d1)
  v1 <- model_xgb$best_iteration
  dat1 <- data.frame(x=rep(d1$iter), val=d1$train_auc)
  
  plot2 <- ggplot(data = dat1, aes(x=x, y=val)) + 
    geom_line(colour = "red") + 
    geom_vline(xintercept=v1) + 
    theme_bw() +
    labs(title="AUC values for XGBoost", x="Iteration", y="AUC values")
  ggsave("xgboost.pdf", plot = last_plot(), device = "pdf",path = "../figs",
         scale = 1, width = 10, height = 10, units =c("in", "cm", "mm"),
         dpi = 300, limitsize = TRUE)
  
  # Step 5: Prediction
  prediction_xg <- round(predict(object = model_xgb ,newdata = xgtest),0)

  time2 <- Sys.time()
  return(list(prediction = prediction_xg,time = time2 - time1))
  
}

