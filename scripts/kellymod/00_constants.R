library(keras)
library(tensorflow)

# Set up Project Parent Data Directory
# Set Working Directory for My Sanity
working_dir = here::here()
project_dirname <- 'mod_a'
dat_path <- paste0(working_dir, '/data/', project_dirname)
