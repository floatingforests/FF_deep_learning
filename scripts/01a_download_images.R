#'------------------------------------------------------
#'
#' @title Scripts to download images
#' 
#' @author Jarrett Byrnes
#' 
#'------------------------------------------------------

library(tidyverse)
setwd(here::here())

#subject metadata
subjects <- readRDS("./data/subjects.RDS")
#classifications
falk <- readRDS("./data/falk.RDS") 

#join classification data to subject metadata
falk_with_meta <- falk %>%
  left_join(subjects)




#function to download file given subject ID
download_subject <- function(subject, image_out = "data/images/falk_images/"){
  subj <- subjects %>% filter(subject_id == subject)
  subj_url <- subj$locations
  f <- basename(subj_url)
  
  #download file
  download.file(subj_url, paste0(image_out, f), 
                method = "curl", 
                extra = "--retry 10 --retry-delay 10")
  
}


# Get the datasets we will download

# Falklands Image Download ####

falk_with_meta <- falk_with_meta %>%
  mutate(total_responses = yes + no) %>%
  mutate(fraction_yes = yes/total_responses) %>%
  dplyr::select(subject_id, yes, no, fraction_yes, total_responses, bad_image,
                clouds, everything()) %>%
  #some classifications have thousands of responses, drop them
  filter(yes <=20) %>%
  filter(`#width` == 350 &
           `#height` == 350 &
           !is.na(`#width`) &
           !is.na(`#height`)) 

#download the images
walk(falk_with_meta$subject_id, download_subject)

