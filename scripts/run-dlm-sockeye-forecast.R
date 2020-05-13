#==================================================================================================
#Project Name: SALMON FORECAST PAPER - Dynamic Linear Sibling Models (DLMs)
#Creator: Curry James Cunningham, College of Fisheries and Ocean Sciences, UAF
#Date: 3.10.17
#
#Purpose: To generate retrospective forecasts from dynamic linear sibling models
#
#
#==================================================================================================
#NOTES:

#==================================================================================================
require(tidyverse)
require(dplyr)
require(ggthemes)
require(MARSS)
require(readr)
require(here)

# Control Section ---------------------------------------------------------

# Controls for 1-step ahead prediction
years <- 2000:2018
n.years <- length(years)

stocks <- c('Kvichak','Alagnak','Naknek','Egegik','Ugashik','Igushik','Wood','Nushagak','Togiak')
n.stocks <- length(stocks) 

# Controls for DLM model
maxit <- 1e4
model.type <- 0 #NO log transformation
start.year <- 1963

# load data ---------------------------------------------------------------

dat <- read.csv(here::here("data", "2019.csv"), stringsAsFactors = FALSE) 

# Source Model Script ------------------------------------------------------
source(here::here("functions","fit-dlm-model.R"))



# Write Output -------------------------------------------------------------
# write_rds(lag_model, path = file.path(results_dir,"lag_model_predictions.rds"))


