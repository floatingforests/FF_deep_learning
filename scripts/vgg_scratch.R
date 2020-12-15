library(keras)

model <- keras_model_sequential() %>%
  layer_zero_padding_2d(input_shape = c(500, 500, 3)) %>%
  layer_conv_2d(filters = 64, kernel_size = c(3,3),
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
