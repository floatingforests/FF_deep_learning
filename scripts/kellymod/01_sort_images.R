library(tidyverse)


# Set Working Directory for My Sanity
working_dir <- here::here()
script_dir <- paste0(working_dir, "/scripts/kellymod/")

#load constants
source(paste0(script_dir, "00_constants.R"))


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

img_from_subject_ln <- function(subj_url){
  
  filename <- basename(subj_url)
  system(glue::glue("ln -s {working_dir}/data/images/falk_images/{basename} ./{basename}"))
  
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
  map(train$locations, img_from_subject_ln)
  
  setwd(file.path(dataset_dir,'test',label_name))
  map(test$locations, img_from_subject_ln)
  
  setwd(file.path(dataset_dir,'validation',label_name))
  map(val$locations, img_from_subject_ln)
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
dir.create(dat_path)

# Subset and Download Images
make_dir_flow(label_data = falk_with_meta, label_col = 'is_kelp',
              categories=unique(falk_with_meta$is_kelp),
              num_images = 100, sample_per = c(0.7,0.2,0.1), dataset_dir = dat_path)
