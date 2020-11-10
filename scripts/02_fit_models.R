#code to fit a model with keras
#given a set of 3D raster stacks, what is the probability that each contains kelp?


#libraries
library(tidyverse)
library(keras)

#load data - this is a list containing test and training data with labels

ff_data <- readRDS("../data/ff_tensor_data.RDS")
#need to write code to separate train/test/labels

#set shape to size of training data?
model <- keras_model_sequential() %>%
  layer_dense(units = 16, activation = "relu", input_shape = c(3)) %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")

#directly from textbook
model %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy",
  metrics = c("accuracy")
)

#set batch size to 10 - should it be lower than the number of input tensors?
history <- model %>% fit(
  x = ff_train,
  y = as.vector(ff_train_labels$fraction_yes),
  epochs = 20,
  batch_size = 10,
  validation_data = list(ff_test, as.vector(ff_test_labels$fraction_yes))
)
