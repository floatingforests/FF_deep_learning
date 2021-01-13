library(raster)

# Set Working Directory for My Sanity
working_dir <- here::here()
script_dir <- paste0(working_dir, "/scripts/kellymod/")

#load constants
source(paste0(script_dir, "00_constants.R"))


# Set up Directories
train_dir = file.path(dat_path, 'train')
test_dir = file.path(dat_path, 'test')
val_dir = file.path(dat_path, 'validation')

# Set Up Image Generator
datagen = image_data_generator(featurewise_center = TRUE,
                               featurewise_std_normalization = TRUE)

train_generator = flow_images_from_directory(
  train_dir,
  datagen,
  target_size = c(500,500),
  batch_size = 20,
  class_mode = "binary"
)

test_generator = flow_images_from_directory(
  test_dir,
  datagen,
  target_size = c(500,500),
  batch_size = 20,
  class_mode = "binary"
)

valid_generator = flow_images_from_directory(
  val_dir,
  datagen,
  target_size = c(500,500),
  batch_size = 20,
  class_mode = "binary"
)
