library("magick")
img <- image_read(file.choose())
print(img)

image_info(img)
img.ds <- as.integer(image_data(img))/255
img

img.ds[1:10,1:5, 1]

img.gray <- image_channel(img)
print(img.gray)

img.small <- image_resize(img, "200x400")
print(img.small)

img.flip = image_flip(img)
img.flop = image_flop(img)
print(img.flip)
print(img.flop)

image_write(img.flop, path = "C:/Users/sanso/Downloads/test_flop.jpg", format= "jpeg", quality=100)
image_write(img.flip, path = "C:/Users/sanso/Downloads/test_flip.jpg", format= "jpeg", quality=100)


rm(list=ls())
library(keras)
use_session_with_seed(1,disable_parallel_cpu = F)
mnist <- dataset_mnist()
x_train1 <- mnist$train$x
y_train1 <- mnist$train$y
x_test1 <- mnist$test$x
y_test1 <- mnist$test$y

x_train1[60000,,,]

# translate 3dim to 4dim for CNN
dim(x_train1) <- c(nrow(x_train1),28,28,1)
dim(x_test1) <- c(nrow(x_test1),28,28,1)

# rescale
x_train1 <- x_train1/255
x_test1 <- x_test1/255

is.matrix(x_train)

# one-hot-encoding
y_train1_one_hot <- to_categorical(y_train1, 10)
y_test1_one_hot <- to_categorical(y_test1, 10)
dim(y_train1_one_hot)



library(keras)
library("magick")
model1 <- keras_model_sequential()
model1 %>%
  layer_conv_2d(32,3,3,activation='relu', input_shape = c(28,28,1)) %>%
  layer_conv_2d(32,3,3,activation='relu') %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  layer_dropout((rate=0.25)) %>%
  layer_flatten() %>%
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout((rate=0.5)) %>%
  layer_dense(units = 10, activation = 'softmax')

summary(model1)

model1 %>% compile(
  loss = "categorical_crossentropy",
  optimizer = "adam",
  metrics = c("accuracy")
)

history <- model1 %>% fit(
  x_train, y_train_one_hot,
  epochs = 30, batch_size = 128,
  validation_split = 0.2
)



model1 %>% evaluate(x_test, y_test_one_hot, verbose=0)
model1 %>% predict_classes(x_test1)







model <- keras_model_sequential()
model %>%
  layer_conv_2d(32,3,3,activation='relu', input_shape = c(28,28,1)) %>%
  layer_conv_2d(32,3,3,activation='relu') %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  layer_dropout((rate=0.25)) %>%
  layer_flatten() %>%
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout((rate=0.5)) %>%
  layer_dense(units = 10, activation = 'softmax')
  
summary(model)

model %>% compile(
  loss = "categorical_crossentropy",
  optimizer = "adam",
  metrics = c("accuracy")
)

history <- model %>% fit(
  x_train, y_train_one_hot,
  epochs = 30, batch_size = 128,
  validation_split = 0.2
)
plot(history)

model %>% evaluate(x_test, y_test_one_hot, verbose=0)
model %>% predict_classes(x_test)


history <- model %>% fit(
  x_train1, y_train1_one_hot,
  epochs = 30, batch_size = 128,
  validation_split = 0.2
)
model %>% evaluate(x_test1, y_test1_one_hot, verbose=0)
model %>% predict_classes(x_test1)



# assignment

rm(list=ls())
library(keras)
library("magick")
mnist_train_file_list <- list.files('C:/Users/sanso/Downloads/mnist_png/training')
mnist_test_file_list <- list.files('C:/Users/sanso/Downloads/mnist_png/testing')

# x_train
x_train <- array(0, dim = c(length(mnist_train_file_list),28,28))
for(i in 1:length(mnist_train_file_list)){
  x_train[i,,] = matrix(as.integer(image_data(image_read(paste('C:/Users/sanso/Downloads/mnist_png/training/',mnist_train_file_list[i],sep='')))),nrow=28,ncol=28)
  print(paste("file_num",i))
}
x_train <- x_train/255
dim(x_train) <- c(length(mnist_train_file_list),28,28,1)

# x_test
x_test <- array(0, dim = c(length(mnist_test_file_list),28,28))
for(i in 1:length(mnist_test_file_list)){
  x_test[i,,] = matrix(as.integer(image_data(image_read(paste('C:/Users/sanso/Downloads/mnist_png/testing/',mnist_test_file_list[i],sep='')))),nrow=28,ncol=28)
  print(paste("file_num",i))
}
dim(x_test) <- c(length(mnist_test_file_list),28,28,1)
x_test <- x_test/255

# y_train
y_train <- array(0, dim = c(length(mnist_train_file_list)))
for(i in 1:length(mnist_train_file_list)){
  y_train[i] <- as.integer(strsplit(mnist_train_file_list[i], '_')[[1]][1])
}
y_train_one_hot<- c()
y_train_one_hot <- to_categorical(y_train, 10)

# shuffle train data
randomize_ind <- sample(1:length(mnist_train_file_list))
x_train <- x_train[randomize_ind,,,]
y_train_one_hot <- y_train_one_hot[randomize_ind,]

# y_test
y_test <- c()
for(i in 1:length(mnist_test_file_list)){
  y_test <- c(y_test, as.integer(strsplit(mnist_test_file_list[i], '_')[[1]][1]))
}
y_test_one_hot <- c()
y_test_one_hot <- to_categorical(y_test, 10)
y_test_one_hot

model <- keras_model_sequential() %>%
  layer_conv_2d(32, kernel_size = c(3,3),activation='relu', input_shape = c(28,28,1)) %>%
  layer_conv_2d(16, kernel_size = c(3,3),activation='relu') %>%
  layer_conv_2d(64, kernel_size = c(3,3),activation='relu') %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  layer_dropout((rate=0.25)) %>%
  layer_flatten() %>%
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout((rate=0.5)) %>%
  layer_dense(units = 10, activation = 'softmax')

summary(model)

model %>% compile(
  loss = "categorical_crossentropy",
  optimizer = "adam",
  metrics = c("accuracy")
)

history <- model %>% fit(
  x_train, y_train_one_hot,
  epochs = 30, batch_size = 128,
  validation_split = 0.2
)

plot(history)
model %>% evaluate(x_test, y_test_one_hot, verbose=0)
model %>% predict_classes(x_test)
