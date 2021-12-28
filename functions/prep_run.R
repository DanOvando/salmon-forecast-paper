prep_run <- function(results_name,
                     results_description =  "test folder for results",
                     first_year = 1990,
                     last_year = 2019,
                     min_year = 1963,
                     eval_year = 2000
) {
  # add any library you need here -------------------------------------------
  
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
  library(patchwork)
  library(fishualize)
  library(PNWColors)
  library(cowplot)
  library(magick)
  library(ggtext)

  # for edm script
  
  library(doParallel)
  library(reshape2)
  library(rEDM)
  library(tsensembler)
  library(lubridate)
  library(geosphere)
  library(DescTools)
  
  # for DLM script
  library(MARSS)
  library(snowfall)
  require(parallel)
  require(snowfall)
  # always last
  library(tidyverse)
  
  # create directory for result batch and prodide description if desired
  
  assign("results_name", results_name, envir = .GlobalEnv)
  
  # results_name <<- results_name # folder name for batch of results
  
  results_dir <- here("results", results_name)
  
  assign("results_dir",results_dir , envir = .GlobalEnv)
  
  # results_dir <<- here("results", results_name)
  
  assign("first_year", first_year, envir = .GlobalEnv)
  
  # first_year <<- first_year
  
  # last_year <<- last_year
  
  assign("last_year", last_year, envir = .GlobalEnv)
  # 
  # min_year <<- min_year
  # 
  assign("min_year", min_year, envir = .GlobalEnv)
  
  assign("eval_year", eval_year, envir = .GlobalEnv)
  
  
  if (!dir.exists(results_dir)) {
    
    dir.create(results_dir, recursive = TRUE)
    
    dir.create(file.path(results_dir,"figs"), recursive = TRUE)
    
    dir.create(file.path(results_dir,"fits"), recursive = TRUE)
    
    write(results_description,
          file.path(results_dir, "result-description.txt"))
    
  }
  
}
