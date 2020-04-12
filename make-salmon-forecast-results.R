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

# set options -------------------------------------------------------------

run_ml <- TRUE # run machine learning scripts

run_dlm <- FALSE # run dynamic linear model scripts

run_edm <- FALSE # run EDM scripts


# create directory for result batch and prodide description if desired

results_name <- "v0.5" # folder name for batch of results

results_dir <- here("results", results_name) 

if (!dir.exists(results_dir)){
  
  dir.create(results_dir)
  
  result_description <- "test folder for results"
  
  write(result_description, file.path(results_dir,"result-description.txt"))
  
}


# run machine learning ----------------------------------------------------

if (run_ml){
  
  source(here("scripts","run-ml-salmon-forecast.R"))
  
} else {
  
  # if you'd like to load specific saved results from a succesful run do so here
  
}

# run dlm -----------------------------------------------------------------

if (run_dlm){
  
  
} else {
  
  
}

# run edm -----------------------------------------------------------------

if (run_edm){
  
  
} else {
  
}

# process results ---------------------------------------------------------



