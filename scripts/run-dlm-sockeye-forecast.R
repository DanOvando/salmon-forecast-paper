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
# require(tidyverse)
# require(dplyr)
# require(ggthemes)
# require(MARSS)
# require(readr)
# require(here)
# require(parallel)
# require(snowfall)

# Control Section ---------------------------------------------------------

# Whether to fit model
do.est <- TRUE
do.parallel <- TRUE

# Number of cores to use for parallel
n.cores <- detectCores()-4

model <- "dlm"

run_name <- results_name

# Output directory for intermediate results
dir.out <- here::here("results", run_name)

if (!dir.exists(dir.out)){
dir.create(dir.out, recursive=TRUE)
}

# Output folder for complete object
# results_dir <- here::here("results")

# Controls for 1-step ahead prediction
years <- 2000:2019
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

# Fit 1-step ahead forecasts -----------------------------------------------
# Note: Can run in parallel to speed things up

if(do.est==TRUE) {
  if(do.parallel==FALSE) {
    # ESTIMATION: Loop ---------------------------------------
    y <- 1
    for(y in 1:n.years) {
      print(paste(y, "of", n.years))
  
      return_year <- years[y]
  
      fit <- fit_dlm_model(pred.year=return_year, start.year=start.year, 
                             model.type=model.type, data=dat, rivers.include=stocks, maxit=maxit)  
      #Output list
      if(y==1) {
        out.dlm <- data.frame(model, return_year, fit$short)
      }else {
        out.dlm <- rbind(out.dlm,  data.frame(model, return_year, fit$short))
      }
    }# next y
  }else {
    # ESTIMATION: Parallel ---------------------------------------
  
    # Wrapper function 
    wrapper <- function(x) {
      return_year <- years[x]
      fit <- fit_dlm_model(pred.year=return_year, start.year=start.year, 
                             model.type=model.type, data=dat, rivers.include=stocks, maxit=maxit)
      # Outputs
      wrapper.out <- data.frame(model, return_year, fit$short)
      return(wrapper.out)
    }
    # Snowfall call
    sfInit(parallel=TRUE, cpus=n.cores, type='SOCK')  #Detect Cores
    sfExportAll()
    sfLibrary(MARSS)
    snowfall.out <- sfLapply(x=1:n.years, fun=wrapper)
    sfStop()
    
    # Unlist snowfall object
    for(y in 1:n.years) {
      if(y==1) {
        out.dlm <- snowfall.out[[y]]
      }else {
        out.dlm <- rbind(out.dlm,   snowfall.out[[y]])
      }
    }#next y
    
    
  }
  # ESTIMATION: Save Output ------------------------------------
  write_rds(out.dlm, path=file.path(dir.out, "out.dlm.rds"))

}else {
  out.dlm <- read_rds(path=file.path(dir.out, "out.dlm.rds"))
}

# Add attach brood_year, age_group, observed_returns -------------------------------

out.dlm.2 <- out.dlm %>% mutate("brood_year"=(return_year-fwAge-oAge-1),
                                "age_group"=paste0(fwAge,"_",oAge))



out.dlm.3 <- out.dlm.2 %>% left_join(dat, by=c("System"="System",
                                               "return_year"="retYr",
                                               "fwAge"="fwAge",
                                               "oAge"="oAge"))
# Convert Observed returns to thousands and change name
out.dlm.3$ret <- out.dlm.3$ret #* 1e3
out.dlm.3 <- out.dlm.3 %>% rename("observed_returns"="ret")

# Reorder and remove unneeded columns ----------------------------------------------
dlm_model <-
  out.dlm.3 %>% select(model,
                       brood_year,
                       return_year,
                       System,
                       age_group,
                       observed_returns,
                       Forecast) %>%
  mutate(Forecast = Forecast / 1e3) %>% 
  rename("system" = "System", "predicted_returns" =
           "Forecast")

# Write Output -------------------------------------------------------------
write_rds(dlm_model, path = file.path(results_dir,"dlm_loo_results.rds"))
write_csv(dlm_model, path = file.path(dir.out, "dlm_loo_results.csv"))

# Fun Plotting -------------------------------------------------------------
g <- dlm_model %>% ggplot(aes(x=observed_returns/1e6, y=predicted_returns/1e6, color=return_year)) +
theme_linedraw() +
scale_color_viridis_c() +
  geom_abline(slope=1, col="red") +
geom_point() +
facet_wrap(system~age_group, scales="free")
# 
g
