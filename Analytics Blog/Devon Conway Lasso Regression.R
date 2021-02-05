#Devon Conway LASSO Regression
library(glmnet)
library(caret)
library(Metrics) 

#############################################################################
# Load Data
#############################################################################
data(College)
dataset <- College

#############################################################################
# Split data into train and test sets
#############################################################################
set.seed(123)
trainIndex <- sample(x = nrow(dataset), size = nrow(dataset) * 0.7)
train <- dataset[trainIndex,]
test <- dataset[-trainIndex,]

train_x <- model.matrix(Grad.Rate ~., train)[,-1]
test_x <- model.matrix(Grad.Rate ~., test)[,-1]

train_y <- train$Grad.Rate
test_y <- test$Grad.Rate

#############################################################################
# Find best values of lamda
#############################################################################
# Find the best lambda using cross-validation
set.seed(123) 
cv.lasso <- cv.glmnet(train_x, train_y, alpha = 1, nfolds = 10)
plot(cv.lasso)

# optimal value of lambda; minimizes the prediction error
log(cv.lasso$lambda.min)
log(cv.lasso$lambda.1se)

#############################################################################
# Fit models based on lambda
#############################################################################
# Fit the final model on the training data using lamda.min
model.min <- glmnet(train_x, train_y, alpha = 1, lambda = cv.lasso$lambda.min)
model.min

# Display regression coefficients
coef(model.min)

# Fit the final model on the training data using lamba.1se
model.1se <- glmnet(train_x, train_y, alpha = 1, lambda = cv.lasso$lambda.1se)

# Display regression coefficients
coef(model.1se)


#############################################################################
# Train set predictions
#############################################################################
# Make predictions on the test data using lambda.min
probabilities.train <- predict(model.1se, newx = train_x)
train.rmse <- rmse(train$Grad.Rate, probabilities.train)

#############################################################################
# Test set predictions
#############################################################################
probabilities.test <- predict(model.1se, newx = test_x)
test.rmse <- rmse(test$Grad.Rate, probabilities.test)

# Compare rmse values
train.rmse
test.rmse

#RIDGE



data("College")
dataset <- College

#Split the data into train and test sets
set.seed(123)
trainIndex1 <- sample(x = nrow(dataset), size = nrow(dataset) * 0.7)
train1 <- dataset[trainIndex1,]
test1 <- dataset[-trainIndex1,]

train_x1 <- model.matrix(Grad.Rate ~., train1)[,-1]
test_x1 <- model.matrix(Grad.Rate ~., test1)[,-1]

train_y1 <- train1$Grad.Rate
test_y1 <- test1$Grad.Rate

#2 and 3 Find best values of lambda
#Alpha parameter is the main different in fitting a lasso compared to ridge model
#Alpha 1 is Lasso, alpha 0 is Ridge
set.seed(123) 
cv.ridge <- cv.glmnet(train_x1, train_y1, alpha = 0, nfolds = 10)
plot(cv.ridge)

# optimal value of lambda; minimizes the prediction error
log(cv.ridge$lambda.min)
log(cv.ridge$lambda.1se)

# Fit models based on lambda
# Fit the final model on the training data using lamda.min
model.min <- glmnet(train_x1, train_y1, alpha = 0, lambda = cv.ridge$lambda.min)
model.min

# Display regression coefficients
coef(model.min)

# Fit the final model on the training data using lamba.1se
options(scipen=999)
model.1se <- glmnet(train_x1, train_y1, alpha = 0, lambda = cv.ridge$lambda.1se)

# Display regression coefficients
coef(model.1se)

#Performance of fit against training model
# Make predictions on the test data using lambda.min
probabilities.train1 <- predict(model.1se, newx = train_x1)
train.rmse <- rmse(train1$Grad.Rate, probabilities.train)

# Test set predictions

probabilities.test1 <- predict(model.1se, newx = test_x)
test.rmse <- rmse(test$Grad.Rate, probabilities.test1)

# Compare rmse values
train.rmse
test.rmse

#Stepwise Selection
step(lm(Grad.Rate ~.,data = College), direction='both')
model_step <- step(lm(Grad.Rate ~.,data = College), direction = 'both')
summary(model_step)








