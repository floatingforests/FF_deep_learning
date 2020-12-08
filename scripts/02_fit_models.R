#code to fit a model with keras
#given a set of 3D raster stacks, what is the probability that each contains kelp?


#libraries
library(tidyverse)
library(keras)
setwd("~/Desktop/Bioinformatics/FF_deep_learning")
#load data - this is a list containing test and training data with labels

ff_data <- readRDS("../data/ff_tensor_data.RDS")

ff_train <- ff_data$ff_train
ff_train_labels <- ff_data$ff_train_labels
ff_test <- ff_data$ff_test
ff_test_labels <- ff_data$ff_test_labels

sfun <- function(x) (x-mean(x))/sd(x) 
for(i in 1:3){
  ff_train[,,,i] <- sfun(ff_train[,,,i])
}
for(i in 1:3){
  ff_test[,,,i] <- sfun(ff_test[,,,i])
}
#need to write code to separate train/test/labels

#set shape to size of training data?
#mess with filters and kernel size
#add multiple kernels in each layer?
model <- keras_model_sequential() %>%
  layer_conv_2d(filters = 32, kernel_size = c(5,5),
                activation = "relu", input_shape = c(500, 500, 3)) %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  
  layer_conv_2d(filters = 64, kernel_size = c(5,5),
                activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  layer_conv_2d(filters = 64, kernel_size = c(5,5),
                activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  layer_flatten() %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")

#directly from textbook
model %>% compile(
  optimizer = "rmsprop", 
  loss = "binary_crossentropy", 
  metrics = c("accuracy")
)

#set batch size to 10 - should it be lower than the number of input tensors?
#want the loss to assymptote, if it continues to increase then overfit
history <- model %>% fit(
  x = ff_train,
  y = as.vector(ff_train_labels$fraction_yes),
  epochs = 20,
  batch_size = 10,
  validation_data = list(ff_test, as.vector(ff_test_labels$fraction_yes))
)

