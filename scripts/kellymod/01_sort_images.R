library(tidyverse)
library(glue)


# Set Working Directory for My Sanity
working_dir <- here::here()
script_dir <- paste0(working_dir, "/scripts/kellymod/")
setwd(working_dir)

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



#Make a symbolic link from our bolus of imagery data to the correct
#place in our ML dataset
img_from_subject_ln <- function(subj_url, category, dataset_dir, train_test){
  
  filename <- basename(subj_url)
  system(glue::glue("ln -s {working_dir}/data/images/falk_images/{filename} 
                    {dataset_dir}/{train_test}/{category}/{filename}"))
  
  return(filename)
  
}


# Take a slice of the data labeled for training, testing, or validation,
# and map over it to put images in the right place
img_from_traintest <- function(train_test_dat, dat_type, dataset_dir){
  map2(train_test_dat$locations, train_test_dat[[label_col]],
       img_from_subject_ln, #the function!
       dataset_dir = dataset_dir, train_test = dat_type
  )
  
}

# Make Train, Test, Validation Directories Function
make_dirs <- function(dataset_dir){
  
  dir.create(file.path(dataset_dir, "train")) 
  dir.create(file.path(dataset_dir, "test")) 
  dir.create(file.path(dataset_dir, "validation")) 
  
}

# Make Labels Subdirectories
make_labels_subdir <- function(dataset_dir, label_name) {
  walk(c("test", "train", "validation"),
       ~dir.create(file.path(dataset_dir, .x, label_name)))
  # setwd(file.path(dataset_dir, 'test'))
  # dir.create(label_name)
  # 
  # setwd(file.path(dataset_dir, 'train'))
  # dir.create(label_name) 
  # 
  # setwd(file.path(dataset_dir, 'validation'))
  # dir.create(label_name) 
  
}

# get all files in the directories, recursively
remove_files_dirs <- function(directory) {
  
  f <- list.files(directory, include.dirs = T, full.names = T, recursive = T)
  # remove the files
  invisible(file.remove(f))
}

# Split and Save PNGS to Separate Folders

# Split and Save PNGS to Separate Folders
split_data_to_set_dirs <- function(label_data, label_col = 'is_kelp', 
                                   dataset_dir, 
                                   train_idx, test_idx, val_idx) {
  
  #make dirs we will use
  categories <- unique(data[[label_col]])
  walk(categories, ~ make_labels_subdir(dataset_dir, .x))
  
  #put data into a labeled list
  dat <- list(train = label_data[train_idx,],
              test = label_data[test_idx,],
              validation = label_data[val_idx,])
  
  #walk over each dataset and get images from it
  iwalk(dat, img_from_traintest, dataset_dir = dataset_dir))
# 
#   setwd(file.path(dataset_dir,'train',label_name))
#   map(train$locations, img_from_subject_ln)
#   
#   setwd(file.path(dataset_dir,'test',label_name))
#   map(test$locations, img_from_subject_ln)
#   
#   setwd(file.path(dataset_dir,'validation',label_name))
#   map(val$locations, img_from_subject_ln)
}





# Get indices for train, test, and validation data 
# set given a percentage split of the data
get_ttv_indicies <- function(vec,  sample_per = c(0.7,0.2,0.1)){
  if(sum(sample_per) != 1) stop("Sampling split between trian, test, and validation\n
                                does not sum to 1")
  cat("splitting indices\n")
  train_samp <- sample(vec, size = round(sample_per[1]*length(vec)))
  
  test_samp <-  sample(vec[!(vec %in% train_samp)], size = round(sample_per[2]*length(vec)))
  
  val_samp <- vec[!(vec %in% c(train_samp, test_samp))]
  cat("done\n\n")
  
  return(list(train = train_samp, test = test_samp, val = val_samp))
}


# Write Images to Directories Split by Binary Labels
make_dir_flow <- function(label_data, label_col, num_images, dataset_dir, sample_per, categories) {
  
  # Make Directories
  make_dirs(dataset_dir)
  
  #Figure out test, train, and validation indices
  #Stratify by yes/no and instrument - later stratify by source in next version
  idx_list <- pmap(crossing(unique(label_data$spacecraft),
                            unique(label_data[[label_col]])) %>% as.list(),
                  ~ which(label_data$spacecraft==.x & label_data$is_kelp==.y))
  
  train_test_val <- map(idx_list, get_ttv_indicies, sample_per = sample_per)
  
  train_idx <- map(train_test_val, ~.x$train) %>% flatten_dbl()
  test_idx <- map(train_test_val, ~.x$test) %>% flatten_dbl()
  val_idx <- map(train_test_val, ~.x$val) %>% flatten_dbl()
  
  # Find, Save PNGS to Separate Directories
  split_data_to_set_dirs(label_data=label_data, 
                         label_name=category, 
                         dataset_dir=dataset_dir, 
                         train_idx = train_idx, test_idx = test_idx, 
                         val_idx = val_idx)
  
  
}

# Set up Project Parent Data Directory
dir.create(dat_path)


# Subset and Download Images
set.seed(2021)
make_dir_flow(label_data = falk_with_meta, label_col = 'is_kelp',
              categories=unique(falk_with_meta$is_kelp),
              #num_images = 100, #testing
              num_images = nrow(falk_with_meta), #the whole shebang
              sample_per = c(0.7,0.2,0.1), dataset_dir = dat_path)
