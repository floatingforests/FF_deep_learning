library(tidyverse)
library(raster)
library(keras)
library(tensorflow)

# Set Working Directory for My Sanity
working_dir = "/Users/kellyluis/Desktop/Personal/misc/FF_deep_learning"

# subject metadata
subjects <- readRDS("./data/subjects.RDS")

# classifications
falk <- readRDS("./data/falk.RDS") 

# join classification data to subject metadata
falk_with_meta <- merge(falk, subjects)

# encode responses as fraction of yes/total responses
falk_with_meta <- falk_with_meta %>%
  mutate(total_responses = yes + no) %>%
  mutate(fraction_yes = yes/total_responses) %>%
  dplyr::select(subject_id, yes, no, fraction_yes, total_responses, everything()) %>%
  #some classifications have thousands of responses, drop them
  filter(yes <=20) %>% 
  mutate(is_kelp = round(fraction_yes, 0))

# Make Hacky Category Labels
falk_with_meta$is_kelp[falk_with_meta$is_kelp == 0] <- 'no'
falk_with_meta$is_kelp[falk_with_meta$is_kelp == 1] <- 'yes'

# Helper Functions
# Download Image from the Webs
img_from_subject <- function(subj_url){
  
  filename <- basename(subj_url)
  download.file(subj_url, filename, mode = 'wb')
  Sys.sleep(3)
  
  return(filename)
  
}

# Make Train, Test, Validation Directories Function
make_dirs <- function(dataset_dir){
  
  dir.create(file.path(dataset_dir, "train")) 
  dir.create(file.path(dataset_dir, "test")) 
  dir.create(file.path(dataset_dir, "validation")) 
  
}

# Make Labels Subdirectories
make_labels_subdir <- function(dataset_dir, label_name) {
  
  setwd(file.path(dataset_dir, 'test'))
  dir.create(label_name)
  
  setwd(file.path(dataset_dir, 'train'))
  dir.create(label_name) 
  
  setwd(file.path(dataset_dir, 'validation'))
  dir.create(label_name) 
  
}

# get all files in the directories, recursively
remove_files_dirs <- function(directory) {
  
  f <- list.files(directory, include.dirs = T, full.names = T, recursive = T)
  # remove the files
  invisible(file.remove(f))
}

# Split and Save PNGS to Separate Folders
split_data_to_set_dirs <- function(data, label_name="kelp_yes", dataset_dir, trainEnd, testEnd, valStart, num_images) {
  
  train <- data[1:trainEnd,]
  test <- data[(trainEnd + 1):testEnd,]
  val <- data[valStart:num_images,]
  
  make_labels_subdir(dataset_dir, label_name)
  
  setwd(file.path(dataset_dir,'train',label_name))
  map(train$locations, img_from_subject)
  
  setwd(file.path(dataset_dir,'test',label_name))
  map(test$locations, img_from_subject)
  
  setwd(file.path(dataset_dir,'validation',label_name))
  map(val$locations, img_from_subject)
}

# Write Images to Directories Split by Binary Labels
make_dir_flow <- function(label_data, label_col, num_images, dataset_dir, sample_per, categories) {
  
  # Make Directories
  make_dirs(dataset_dir)
  
  # Figure Out Start Stop Indices
  testEnd <- floor(num_images*sample_per[1])
  trainEnd <- floor(num_images*sample_per[2])
  valStart <- num_images - trainEnd
  
  # Find, Save PNGS to Separate Directories
  for (category in categories) {
    data <- label_data[sample(which(label_data[,label_col] == category), num_images), ]
    split_data_to_set_dirs(data=data, label_name=category, dataset_dir=dataset_dir, testEnd=testEnd, trainEnd=testEnd, valStart=valStart, num_images = num_images)
  }
  
}

# Set up Project Parent Data Directory
project_dirname <- 'mod_a'
dat_path <- paste0(working_dir, '/data/', project_dirname)
dir.create(dat_path)

# Subset and Download Images
make_dir_flow(label_data = falk_with_meta, label_col = 'is_kelp',
              categories=unique(falk_with_meta$is_kelp),
              num_images = 100, sample_per = c(0.7,0.2,0.1), dataset_dir = dat_path)

# Set up Directories
train_dir = file.path(dat_path, 'train')
test_dir = file.path(dat_path, 'test')
val_dir = file.path(dat_path, 'validation')

# Set Up Image Generator
datagen = image_data_generator(rescale=1/255)

train_generator = flow_images_from_directory(
  train_dir,
  datagen,
  target_size = c(150,150),
  batch_size = 20,
  class_mode = "binary"
)

valid_generator = flow_images_from_directory(
  val_dir,
  datagen,
  target_size = c(150,150),
  batch_size = 20,
  class_mode = "binary"
)

# Write Model
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

# Compile Model
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
  validation_data = valid_generator,
  validation_steps = 30
)

# Save Prelim Model
setwd(file.path(dat_path))
model %>% save_model_hdf5(paste0(dat_path,'.h5'))

# View Model
plot(history)