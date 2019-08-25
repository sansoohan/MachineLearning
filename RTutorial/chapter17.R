
rm(list=ls())
library(keras)
library("magick")
use_session_with_seed(1, disable_parallel_cpu = F)
set.seed(100)

piano_accordion_file_list <- list.files('D:/images/accordion_piano')
piano_accordion_idx <- c(sample(1:length(piano_accordion_file_list), length(piano_accordion_file_list)*.7))
piano_accordion_train_file_list <- piano_accordion_file_list[piano_accordion_idx]
piano_accordion_test_file_list <- piano_accordion_file_list[-piano_accordion_idx]
piano_accordion_file_y <- array(0, dim = c(length(piano_accordion_file_list)))
piano_accordion_file_x <- array(0, dim = c(length(piano_accordion_file_list),60,60,3))

# x_train / x_test
piano_accordion_file_x <- array(0, dim = c(length(piano_accordion_file_list),60,60,3))
for(i in 1:length(piano_accordion_file_list)){
  image <- image_read(paste('D:/images/accordion_piano/',piano_accordion_file_list[i],sep=''))
  image.resized <- image_resize(image, "60x60!")
  piano_accordion_file_x[i,,,] = as.integer(image_data(image.resized))
  print(paste("file_num",i))
}
piano_accordion_file_x <- piano_accordion_file_x/255
piano_accordion_file_x_train = piano_accordion_file_x[piano_accordion_idx,,,]
piano_accordion_file_x_test = piano_accordion_file_x[-piano_accordion_idx,,,]

# y_train / y_test
for(i in 1:length(piano_accordion_file_list)){
  if(strsplit(piano_accordion_file_list[i], '_')[[1]][1] == 'accordian'){
    piano_accordion_file_y[i] <- 0 
  }
  else if(strsplit(piano_accordion_file_list[i], '_')[[1]][1] == 'piano'){
    piano_accordion_file_y[i] <- 1
  }
}
piano_accordion_file_y_train <- piano_accordion_file_y[piano_accordion_idx]
piano_accordion_file_y_test <- piano_accordion_file_y[-piano_accordion_idx]
piano_accordion_file_y_test
piano_accordion_file_y_train
piano_accordion_file_y_train_one_hot <- to_categorical(piano_accordion_file_y_train, 2)
piano_accordion_file_y_test_one_hot <- to_categorical(piano_accordion_file_y_test, 2)

model <- keras_model_sequential() %>%
  layer_conv_2d(16, kernel_size = c(10,10),activation='relu', input_shape = c(60,60,3)) %>%
  layer_conv_2d(16, kernel_size = c(10,10),activation='relu') %>%
  layer_max_pooling_2d(pool_size = c(5,5)) %>%
  layer_dropout((rate=0.25)) %>%
  layer_flatten() %>%
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout((rate=0.5)) %>%
  layer_dense(units = 2, activation = 'softmax')

summary(model)

model %>% compile(
  loss = "categorical_crossentropy",
  optimizer = "adam",
  metrics = c("accuracy")
)

history <- model %>% fit(
  piano_accordion_file_x_train, piano_accordion_file_y_train_one_hot,
  epochs = 30, batch_size = 128,
  validation_split = 0.2
)

plot(history)
model %>% evaluate(piano_accordion_file_x_test, piano_accordion_file_y_test_one_hot, verbose=0)
model %>% predict_classes(piano_accordion_file_x_test)
