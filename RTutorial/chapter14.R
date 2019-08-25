library(keras)
mnist <- dataset_mnist()
x_train <- mnist$train$x
y_train <- mnist$train$y
x_test <- mnist$test$x
y_test <- mnist$test$y

dim(x_train)
dim(x_test)

# reshape
dim(x_train) <- c(nrow(x_train), 784)
dim(x_train)
dim(x_test) <- c(nrow(x_test), 784)
dim(x_test)

# make value from 0~255 to 0~1
x_train <- x_train / 255
x_train

# y one hot
y_train_one_hot <- to_categorical(y_train, 10)
y_test_one_hot <- to_categorical(y_test, 10)


model <- keras_model_sequential()
model %>%
  layer_dense(units = 256, activation = 'relu', input_shape = c(784)) %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 10, activation = 'softmax')
summary(model)

model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c("accuracy")
)

history <- model %>% fit(
  x_train, y_train_one_hot,
  epochs = 30, batch_size = 128,
  validation_split = 0.2
)

plot(history)

model %>% evaluate(x_test, y_test_one_hot, verbose = 0)
model %>% predict_classes(x_test)

liver_data <- read.csv('liver.csv', header = T, sep=',', encoding='utf-8')
liver_data <- as.matrix(liver_data)

# sampling
liver_train_idx <- c(sample(1:nrow(liver_data), 0.6*nrow(liver_data)))
liver_x_train <- liver_data[liver_train_idx, -1]
liver_x_train
dim(liver_x_train)
liver_y_train <- liver_data[liver_train_idx, 1]
liver_y_train_one_hot <- to_categorical(liver_y_train, 2)
liver_y_train_one_hot

liver_x_test <- liver_data[-liver_train_idx, -1]
liver_y_test <- liver_data[-liver_train_idx, 1]
liver_y_test_one_hot <- to_categorical(liver_y_test, 2)
liver_y_test_one_hot

model <- keras_model_sequential()

model %>%
  layer_dense(units = 10, activation = 'relu', input_shape = c(6)) %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 10, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 2, activation = 'softmax')
summary(model)

model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c("accuracy")
)
liver_x_train
history <- model %>% fit(
  liver_x_train, liver_y_train_one_hot,
  epochs = 1000, batch_size = 10,
  validation_split = 0.2
)
