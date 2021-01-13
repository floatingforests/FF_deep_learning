
source("00_constants.R")
source("02_image_translate.R")


model <- keras_model_sequential() %>%
  layer_conv_2d(filters = 32, kernel_size = c(5,5),
                activation = "relu", input_shape = c(150, 150, 3)) %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  layer_conv_2d(filters = 64, kernel_size = c(5,5),
                activation = "relu", input_shape = c(150, 150, 3)) %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  layer_conv_2d(filters = 64, kernel_size = c(5,5),
                activation = "relu", input_shape = c(150, 150, 3)) %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  layer_flatten() %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")

model %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy",
  metrics = c("accuracy")
)



# Run Model
history <-model %>%  fit_generator(
  train_generator,
  steps_per_epoch = 50,
  epochs=15,
  validation_data = test_generator,
  validation_steps = 30
)

# Save Prelim Model
setwd(file.path(dat_path))
model %>% save_model_hdf5(paste0('base_ff_imagemod_', 'dat_path','.h5'))

# View Model
plot(history)
