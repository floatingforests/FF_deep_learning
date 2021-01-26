

source("00_constants.R")
source("02_image_translate.R")

model <- keras_model_sequential() %>%
  layer_zero_padding_2d() %>%
  layer_conv_2d(filters = 64, kernel_size = c(3,3),
                input_shape = c(500, 500, 3),
                activation = "relu") %>%
  layer_zero_padding_2d() %>%
  layer_conv_2d(filters = 64, kernel_size = c(3,3),
                activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2,2),strides = c(2,2)) %>%
  
  layer_zero_padding_2d() %>%
  layer_conv_2d(filters = 128, kernel_size = c(3,3),
                activation = "relu") %>%
  layer_zero_padding_2d() %>%
  layer_conv_2d(filters = 128, kernel_size = c(3,3),
                activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2,2),strides = c(2,2)) %>%
  
  layer_zero_padding_2d() %>%
  layer_conv_2d(filters = 256, kernel_size = c(3,3),
                activation = "relu") %>%
  layer_zero_padding_2d() %>%
  layer_conv_2d(filters = 256, kernel_size = c(3,3),
                activation = "relu") %>%
  layer_zero_padding_2d() %>%
  layer_conv_2d(filters = 256, kernel_size = c(3,3),
                activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2,2),strides = c(2,2)) %>%
  
  layer_zero_padding_2d() %>%
  layer_conv_2d(filters = 512, kernel_size = c(3,3),
                activation = "relu") %>%
  layer_zero_padding_2d() %>%
  layer_conv_2d(filters = 512, kernel_size = c(3,3),
                activation = "relu") %>%
  layer_zero_padding_2d() %>%
  layer_conv_2d(filters = 512, kernel_size = c(3,3),
                activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2,2),strides = c(2,2)) %>%
  
  
  layer_zero_padding_2d() %>%
  layer_conv_2d(filters = 512, kernel_size = c(3,3),
                activation = "relu") %>%
  layer_zero_padding_2d() %>%
  layer_conv_2d(filters = 512, kernel_size = c(3,3),
                activation = "relu") %>%
  layer_zero_padding_2d() %>%
  layer_conv_2d(filters = 512, kernel_size = c(3,3),
                activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2,2),strides = c(2,2)) %>%
  
  
  layer_flatten() %>%
  layer_dense(units = 4096, activation = "relu") %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 4096, activation = "relu") %>%
  layer_dropout(rate = 0.5) %>%
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
model %>% save_model_hdf5(paste0('vgg_ff_imagemod_', 'dat_path','.h5'))

# View Model
plot(history)
