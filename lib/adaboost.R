# Adaboost
library(rpart)
library(ada)
library(caret)

# Train the model using adaboost model
control <- rpart.control(cp = -1, maxdepth = 14,maxcompete = 1,xval = 0)
model_ada <- ada(diagnosis~., data = df.train, test.x = df.train[,-1], test.y = df.train[,1], type = "gentle", control = control, iter = 70)

# Prediction
prediction_ada <- predict(model_ada, df.test[,-1])
