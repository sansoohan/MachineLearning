library(MASS)
head(Boston)
dim(Boston)

library(reticulate)
use_virtualenv("venv",required = T)
import("scipy")
library(keras)

use_session_with_seed(1, disable_parallel_cpu = F)

Boston
set.seed(100)
idx <- c(sample(1:nrow(Boston), nrow(Boston)*.7))
x_train <- Boston[idx, -14]
y_train <- Boston[idx, 14]
x_test <- Boston[-idx, -14]
y_test <- Boston[-idx, 14]

x_train <- as.matrix(apply(x_train, 2, function(x) (x-min(x))/(max(x)-min(x))))
x_test <- as.matrix(apply(x_test, 2, function(x) (x-min(x))/(max(x)-min(x))))

model = keras_model_sequential()

model %>%
  layer_dense(input_shape = ncol(x_train), units = 1)

model = keras_model_sequential()
model %>% 
  layer_dense(input_shape= ncol(x_train), units = 64, activation= "relu")   %>% 
  layer_dense(units = 64, activation= "relu") %>% 
  layer_dense(units = 1) 

summary(model)

model %>%
  compile(
    loss = "mse",
    optimizer = "adam",
    metrics = list("mean_absolute_error")
  )

fit = model %>%
  fit (
    x = x_train,
    y = y_train,
    epochs = 5000 # 1000, 5000
  )

model %>% evaluate(x_test, y_test, verbose = 0)

pred = model %>% predict(x_test)
head(cbind(y_test, pred, pred-y_test))
plot(y_test, pred, xlim=c(0,55),ylim=c(0,55))



# compare between linear model and trained model

library(MASS)
library(mlbench)
Boston
boston_model<- lm(medv ~ .,Boston)

boston_model$coefficients[2:length(boston_model$coefficients)]

predict_boston <- function(weight_vector, x, bias){
  res <- array()
  colnames(res) <- c("medv")
  for(i in 1:nrow(x)){
    res <- rbind(res, sum(weight_vector*x[i,]) + bias)
  }
  return(res)
}
boston_weight <- boston_model$coefficients[2:length(boston_model$coefficients)]
boston_bias <- boston_model$coefficients[1]

predict_boston(boston_weight, x_train, boston_bias)




# add testing
library(keras)
use_session_with_seed(1, disable_parallel_cpu = F)

numbers = 1000
val1_array = round(runif(numbers)*100)
val2_array = round(runif(numbers)*100)
answer_array = val1_array + val2_array

set.seed(100)
val_idx <- c(sample(1:numbers, numbers*.7))

number_x_train = cbind(val1_array[val_idx], val2_array[val_idx])
colnames(number_x_train) = c("val1", "val2")
number_y_train = answer_array[val_idx]
number_x_test = cbind(val1_array[-val_idx], val2_array[-val_idx])
colnames(number_x_test) = c("val1", "val2")
number_y_test = answer_array[-val_idx]


model = keras_model_sequential()
model %>% 
  layer_dense(input_shape= ncol(number_x_train), units = 2, activation= "relu")   %>% 
  layer_dense(units = 1) 

summary(model)

model %>%
  compile(
    loss = "mse",
    optimizer = "adam",
    metrics = list("mean_absolute_error")
  )

fit = model %>%
  fit (
    x = number_x_train,
    y = number_y_train,
    epochs = 1000
  )

model %>% evaluate(number_x_test, number_y_test, verbose = 0)

number_pred = model %>% predict(number_x_test)
result_sum = cbind(number_x_test, number_pred)
colnames(result_sum) = c("val1", "val2", "Predict")
head(result_sum,30)