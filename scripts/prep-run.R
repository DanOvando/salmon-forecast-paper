prep_run <- function(results_name){

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

# create directory for result batch and prodide description if desired

results_name <- "v0.5" # folder name for batch of results

results_dir <- here("results", results_name) 

if (!dir.exists(results_dir)){
  
  dir.create(results_dir)
  
  result_description <- "test folder for results"
  
  write(result_description, file.path(results_dir,"result-description.txt"))
  
}

}
