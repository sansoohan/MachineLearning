library(keras)
install_keras(method = "conda")

model <- keras_model_sequential()

# clear memory
rm(list=ls())

use_session_with_seed(1, disable_parallel_cpu = F)
# train data : 90
idx <- c(sample(1:50,30), sample(51:100,30), sample(101:150,30))
idx

x_train <- iris[idx, -5]
x_train
y_train <- iris[idx, 5]
# test data : 60
x_test <- iris[-idx, -5]
y_test <- iris[-idx, 5]

x_train <- as.matrix(apply(x_train, 2, function(x) (x-min(x))/max(x) -min(x)))
x_test <- as.matrix(apply(x_test, 2, function(x) (x-min(x))/max(x) -min(x)))

# convert to one_hot
y_train_one_hot <- to_categorical(as.integer(y_train)-1, num_classes=3)
y_test_one_hot <- to_categorical(as.integer(y_test)-1, num_classes=3)

model = keras_model_sequential()
model %>%
  layer_dense(input_shape = ncol(x_train), units = 10, activation = 'relu') %>%
  layer_dense(units = 10, activation = 'relu') %>%
  layer_dense(units = 3, activation = 'softmax')

summary(model)

model %>%
  compile(
    loss = "categorical_crossentropy",
    optimizer = "adagrad",
    metrics = "accuracy"
  )

fit = model %>%
  fit(
    x=x_train,
    y=y_train_one_hot,
    shuffle=T,
    batch_size=5,
    validation_split =0.2,
    epochs = 200
  )
plot(fit)

model %>% evaluate(x_test, y_test_one_hot, verbose=0)
model %>% predict_classes(x_test)
summary(model)
get_config(model)
get_layer(model, index=1)
model$layers
model$inputs
model$outputs
model$get_weights()



save(model, file = 'mymodel.obj')

modle = load('mymodel.obj')
model$layers


# exam 01
model %>%
  layer_dense(input_shape = ncol(x_train), units = 10, activation = 'relu') %>%
  layer_dense(units = 10, activation = 'relu') %>%
  layer_dense(units = 8, activation = 'relu') %>%
  layer_dense(units = 3, activation = 'softmax')

model %>%
  compile(
    loss = "categorical_crossentropy",
    optimizer = "adagrad",
    metrics = "accuracy"
  )

fit = model %>%
  fit(
    x=x_train,
    y=y_train_one_hot,
    shuffle=T,
    batch_size=5,
    validation_split =0.2,
    epochs = 300
  )
plot(fit)

