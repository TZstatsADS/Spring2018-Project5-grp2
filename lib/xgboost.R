# XGboost
library(xgboost)
library(magrittr)
library(dplyr)

## create xgb.DMatrix objects for each trainand test set.
xgtrain <- xgb.DMatrix(as.matrix(df.train %>% select(-diagnosis)), label = df.train$diagnosis)
xgtest  <- xgb.DMatrix(as.matrix(df.test  %>% select(-diagnosis)), label = df.test$diagnosis)

## Set parameters: we start with eta = 0.012, subsample=0.8, max_depth=8, colsample_bytree=0.9 and min_child_weight=5.
params <- list("objective"        = "binary:logistic",
               "eval_metric"      = "auc",
               "eta"              = 0.012,
               "subsample"        = 0.8,
               "max_depth"        = 8,
               "colsample_bytree" = 0.9,
               "min_child_weight" = 5
)

## Train the model using cross validation with 5 folds.
model_xgb.cv <- xgb.cv(params = params,
                       data = xgtrain, 
                       maximize = TRUE,
                       nfold = 5,
                       nrounds = 5000,
                       nthread = 1,
                       early_stopping_round = 100,
                       print_every_n = 100)

## Visualization
d <- model_xgb.cv$evaluation_log
n <- nrow(d)
v <- model_xgb.cv$best_iteration
dat <- data.frame(x=rep(d$iter, 2), val=c(d$train_auc_mean, d$test_auc_mean), set=rep(c("train", "test"), each=n))

ggplot(data = dat, aes(x=x, y=val)) + 
  geom_line(aes(colour=set)) + 
  geom_vline(xintercept=v) + 
  theme_bw() +
  labs(title="AUC values for XGBoost with cross-validation", x="Iteration", y="AUC values")

## Train the model using xgboost model
model_xgb <- xgboost(params = params,
                     data = xgtrain, 
                     maximize = TRUE,
                     nrounds = 5000,
                     nthread = 1,
                     early_stopping_round = 100,
                     print_every_n = 100)

## Visualization
d1 <- model_xgb$evaluation_log
n1 <- nrow(d1)
v1 <- model_xgb$best_iteration
dat1 <- data.frame(x=rep(d1$iter), val=d1$train_auc)

ggplot(data = dat1, aes(x=x, y=val)) + 
  geom_line(colour = "red") + 
  geom_vline(xintercept=v1) + 
  theme_bw() +
  labs(title="AUC values for XGBoost", x="Iteration", y="AUC values")

## Prediction
prediction_xg <- round(predict(object = model_xgb ,newdata = xgtest),0)
