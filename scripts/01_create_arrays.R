#code to import floating forests swipe data and prepare it for keras 
#data was generated from binary yes/no classifications made by citizen scientists on zooniverse.com

#libraries
library(tidyverse)
library(raster)
library(keras)

#subject metadata
subjects <- readRDS("../data/subjects.RDS")
#classifications
falk <- readRDS("../data/falk.RDS") 

#join classification data to subject metadata
falk_with_meta <- falk %>%
  left_join(subjects) %>%
  drop_na()

#encode responses as fraction of yes/total responses
falk_with_meta <- falk_with_meta %>%
  mutate(total_responses = yes + no) %>%
  mutate(fraction_yes = yes/total_responses) %>%
  dplyr::select(subject_id, yes, no, fraction_yes, total_responses, everything()) %>%
  #some classifications have thousands of responses, drop them
  filter(yes <=20)

#make labels
falk_labels <- falk_with_meta %>%
  dplyr::select(subject_id, fraction_yes)

##################
#Helper functions
##################

#function to download, rasterize, and convert ff images to arrays
array_from_subject <- function(subject){
  subj_metadata <- subjects %>% filter(subject_id == subject)
  subj_url <- subj_metadata$locations
  
  crs <- str_c("+proj=utm +zone=",  subj_metadata$`#utm_zone`, 
               " +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
  filename <- basename(subj_url)
  
  download.file(subj_url, filename, mode = 'wb')
  
  img <- raster::brick(filename,              
                       crs=crs)
  
  img@extent = raster::extent(as.numeric(subj_metadata$`#tile_UL_x`), as.numeric(subj_metadata$`#tile_LR_x`),
                              as.numeric(subj_metadata$`#tile_LR_y`), as.numeric(subj_metadata$`#tile_UL_y`))
  
  ff_array <- as.array(img)
  
  unlink(filename)
  
  
  return(ff_array)

}

#function that takes a bunch of subjects and creates array_unlisted from those subjects
generate_input_tensors <- function(sub_vector){
  subject_test_set <- clean_subjects %>%
    filter(subject_id %in% sub_vector)
  
  #download all images and convert to array
  array_list <- map(subject_test_set$subject_id, array_from_subject)
  #convert this list of 3d arrays to a 4d array
  array_unlisted <- aperm(array(as.numeric(unlist(array_list)), 
                                dim=c(500, 500, 3, length(array_list))), 
                          c(4, 1, 2, 3))
  
  return(array_unlisted)
}
###############################
#our goals:
#input: subject id
#output: 500x500x3 array 

#example, subject zooniverse ID = 28751214
array_test <- array_from_subject(28751214)

#check, should be 500x500x3
dim(array_test)


##################################
#Time out: not all subjects are 500x500

#how many subjects are the wrong size?
bad_subjects <- falk_with_meta %>%
  filter(`#width` != 350 |
           `#height` != 350 |
            is.na(`#width`) |
           is.na(`#height`)            
         ) 

##652 out of 10K
#30 are the wrong size (all too small)
#622 are NAs
#These have classifications, but no subject metadata
#these NA subjects seem to be old (created in 2016) and look like 1.0 data
#why are they in this subject set?
#for example, subject 2934266 
#https://panoptes-uploads.zooniverse.org/production/subject_location/23f70d98-90b2-4272-842f-069a982d71ae.jpeg	
#drop these for now?

######################

#for now, drop all that are NA or the wrong size

clean_subjects <- falk_with_meta %>%
  filter(`#width` == 350 &
           `#height` == 350 &
           !is.na(`#width`) &
           !is.na(`#height`)) 

#generate training/testing data/labels
ff_train <- generate_input_tensors(clean_subjects$subject_id[1:10])
ff_test <- generate_input_tensors(clean_subjects$subject_id[11:20])
ff_train_labels <- falk_labels[1:10,]
ff_test_labels <- falk_labels[11:20,]

#save as a list for easy loading
saveRDS(list(ff_train = ff_train, 
             ff_test = ff_test, 
             ff_train_labels = ff_train_labels,
             ff_test_labels = ff_test_labels), "../data/ff_tensor_data.RDS")
