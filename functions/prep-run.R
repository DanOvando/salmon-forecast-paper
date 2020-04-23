prep_run <- function(results_name,
                     results_description =  "test folder for results"
) {
  # add any library you need here -------------------------------------------
  
  library(tidyverse)
  library(keras)
  library(ggridges)
  library(recipes)
  library(here)
  library(ranger)
  library(rerddap)
  library(tidymodels)
  library(furrr)
  library(ggsci)
  library(ggrepel)
  library(gt)
  library(tensorflow)
  
  # create directory for result batch and prodide description if desired
  
  results_name <<- results_name # folder name for batch of results
  
  results_dir <<- here("results", results_name)
  
  if (!dir.exists(results_dir)) {
    
    dir.create(results_dir, recursive = TRUE)
    
    write(results_description,
          file.path(results_dir, "result-description.txt"))
    
  }
  
}
