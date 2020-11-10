library(tidyverse)
library(keras)
ff_data <- readRDS("../data/ff_tensor_data.RDS")
#need to write code to separate train/test/labels


model <- keras_model_sequential() %>%
  layer_dense(units = 16, activation = "relu", input_shape = c(10)) %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")

model %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy",
  metrics = c("accuracy")
)

history <- model %>% fit(
  x = ff_train,
  y = as.vector(ff_train_labels$fraction_yes),
  epochs = 20,
  batch_size = 10,
  validation_data = list(ff_test, as.vector(ff_test_labels$fraction_yes))
)
