#------------------------------------------------------------------------------
#Clean script to run EDM analyses

#------------------------------------------------------------------------------
#Load packages
# library(plyr)
# library(tidyverse)
# library(doParallel)
# library(reshape2)
# library(here)
# library(rEDM)
# library(tsensembler)
# library(lubridate)
# library(geosphere)
# library(DescTools)

# setwd("C:/Users/peter.kuriyama/SynologyDrive/research/postdoc/salmon-forecast-paper")
# source("functions/edm_helper_functions.R")

#------------------------------------------------------------------------------
#Function to change my here directory (I don't use rstudio so Rprojects
#not feasible)
# change_here <- function(new_path){
#   new_root <- here:::.root_env
#   
#   new_root$f <- function(...){file.path(new_path, ...)}
#   
#   assignInNamespace(".root_env", new_root, ns = "here")
# }
# set_here("C:/Users/peter.kuriyama/SynologyDrive/research/postdoc/salmon-forecast-paper")

functions <- list.files(here::here("functions"))

purrr::walk(functions, ~ source(here::here("functions", .x)))

prep_run(results_name = "v0.5", results_description = "testing")

# set options -------------------------------------------------------------

ncores <- 2
# run <- "v0.1"
description <- "testing"

# rundir <- here::here("edm", "results", run)

rundir <- results_dir

# if (!dir.exists(rundir)) {
  # dir.create(rundir, recursive = TRUE)
  # dir.create(file.path(rundir, "edm_figs"), recursive = TRUE)
  # dir.create(file.path(rundir, "edm_fits"), recursive = TRUE)
  # write(description, file = file.path(rundir, "description.txt"))
# }

# last_year <- 2019 # the final year in the data
age_groups <- 4 #number of top age groups to include
min_year <- 1965 # only include data greater than or equal this year
first_year <- 2000 # the first year splitting the test and training data
max_year <- 2015

#------------------------------------------------------------------------------
#Load and process Data
dat <- read.csv(here::here("data", "2019.csv"), stringsAsFactors = FALSE) 
# dat <- read.csv("data/2019.csv", stringsAsFactors = F)

#add ages
dat$Age <- paste(dat$fwAge, dat$oAge, sep = "_")
dat$unq <- paste(dat$System, dat$Age, sep = "_")
dat$ret_std <-  dat$ret #standardize(dat$ret)

#Filter dat by min_year specification
dat <- dat %>% filter(retYr >= min_year)

#Sum returns by retYr and system
tot_dat <- dat %>% group_by(retYr, System) %>% 
	summarize(ret = sum(ret)) %>% as.data.frame

#Remove system/ages with all zeroes
dat <- dat %>% group_by(unq) %>% mutate(nvals = length(ret), 
  nzeroes = length(which(ret == 0)), prop_zero = nzeroes / nvals) %>%
  as.data.frame
dat <- dat %>% filter(prop_zero != 1)

#Standardize the total returns
# tot_dat$ret_std <- standardize(tot_dat$ret)
tot_dat$ret_std <- (tot_dat$ret)

tot_ret <- tot_dat %>% dcast(retYr ~ System, value.var = 'ret_std')


#------------------------------------------------
# 1. Run EDM block_lnlp with simplex and smap for 
# each river by itself
#------------------------------------------------
#Run for each system
rivs <- unique(dat$System)
rivs_res <- vector('list', length = length(rivs))

start_time <- Sys.time()
for(zz in 1:length(rivs)){
	lib_cols <- dat %>% filter(System == rivs[zz]) %>%
		distinct(unq) %>% pull(unq)
	preds <- dat %>% filter(System == rivs[zz]) %>% group_by(unq) %>%
	  summarize(avg_ret = mean(ret)) %>% arrange(desc(avg_ret)) %>%
	  slice(1:5) %>% pull(unq)
	one_riv <- multiple_pred_group(in_group = lib_cols, in_col = preds)
	one_riv$type <- "one system all ages"
	rivs_res[[zz]] <- one_riv
}
rivs_res <- plyr::ldply(rivs_res) 
run_time <- Sys.time() - start_time; run_time #Runs in about 1.2 minutes
#ignore warnings

wtf <- rivs_res %>% 
  unnest(cols = model_output)

rivs_res %>% filter(mase5 < 1, rho5 > 0, p_val5 <= 0.05) %>% dim

#------------------------------------------------
# 2. Run single age from all systems
#------------------------------------------------
#Single age from all systems
focus_ages <- c("1_2", "2_2", "1_3", "1_4", "2_3")
age_res <- vector('list', length = length(focus_ages))

start_time <- Sys.time()
for(zz in 1:length(focus_ages)){
	lib_cols <- unique(dat$unq)[grep(focus_ages[zz], unique(dat$unq))]
	one_riv <- multiple_pred_group(in_group = lib_cols, in_col = lib_cols)
	one_riv$type <- "one age all systems"
	age_res[[zz]] <- one_riv
}
age_res <- plyr::ldply(age_res) 
run_time <- Sys.time() - start_time; run_time  #Runs in 1.2 minutes
age_res %>% filter(mase5 < 1, rho5 > 0, p_val5 <= 0.05) %>% dim

#------------------------------------------------
# 3. Specifically westside and eastside returns
#------------------------------------------------
#Look at west and east side returns

wests <- c("Nushagak", "Igushik", "Wood", "Togiak")
focus_ages <- c("1_2", "2_2", "1_3", "1_4", "2_3")
ww <- crossing(System = wests, Age = focus_ages)
ww$unq <- paste(ww$System, ww$Age, sep = "_")

west_res <- vector('list', length = nrow(ww))
lib_cols <- dat %>% filter(unq %in% ww$unq) %>%
		distinct(unq) %>% pull(unq)
	
west_res <- multiple_pred_group(in_group = lib_cols, in_col = lib_cols)
west_res$type <- "westside, focus ages"

west_res %>% filter(mase5 < 1, rho5 > 0, p_val5 <= 0.05) 

#---------------------------
#East results
easts <- c("Ugashik", "Egegik", "Kvichak", "Naknek", "Alagnak")
focus_ages <- c("1_2", "2_2", "1_3", "1_4", "2_3")
ww <- crossing(System = easts, Age = focus_ages)
ww$unq <- paste(ww$System, ww$Age, sep = "_")

easts_res <- vector('list', length = nrow(ww))
lib_cols <- dat %>% filter(unq %in% ww$unq) %>%
		distinct(unq) %>% pull(unq)
	
easts_res <- multiple_pred_group(in_group = lib_cols, in_col = lib_cols)
easts_res$type <- "eastside, focus ages"
easts_res %>% filter(mase5 < 1, rho5 > 0, p_val5 <= 0.05) %>% dim

#------------------------------------------------
# 4. One system, focus on top 4 ages (by average returns)
#Specified in arguments at top of script
#------------------------------------------------
#one system focus ages
rivs <- unique(dat$System)
rivs_res_focusages <- vector('list', length = length(rivs))

start_time <- Sys.time()

for(zz in 1:length(rivs)){
	preds <- dat %>% filter(System == rivs[zz]) %>% group_by(unq) %>%
	  summarize(avg_ret = mean(ret)) %>% arrange(desc(avg_ret)) %>%
	  slice(1:age_groups) %>% pull(unq)
	one_riv <- multiple_pred_group(in_group = preds, in_col = preds)
	one_riv$type <- "one system top6 ages"
	rivs_res_focusages[[zz]] <- one_riv
}
rivs_res_focusages <- plyr::ldply(rivs_res_focusages) 
run_time <- Sys.time() - start_time; run_time
#------------------------------------------------
# 5. Combine and save all the results
#------------------------------------------------
all_res <- rbind(west_res, easts_res, rivs_res,
	age_res, rivs_res_focusages)
  
  all_res$pred_type <- 'smap'
all_res[all_res$theta == 999, 'pred_type'] <- 'simplex'

temp <- all_res %>% 
  filter(type == "one system top6 ages") %>% 
  unnest(cols = model_output) %>% 
  filter(time == predyr) %>% 
  mutate(obs = pmax(0,obs),
         pred = pmax(0,pred)) %>% 
  separate(pred_col, into = c("system", "fwa","oa"), sep = '_', convert = TRUE) %>% 
  mutate(age = fwa + oa + 1,
         age_group = paste(fwa,oa, sep = '_')) %>% 
  mutate(brood_year = time - age,
         model = paste(pred_type,type, sep = '_')) %>% 
  rename(
         observed_returns = obs,
         predicted_returns = pred,
         return_year = time) %>% 
select(model, brood_year, return_year, system, age_group, observed_returns, predicted_returns)

readr::write_csv(temp, path = file.path(results_dir,"edm_loo_results.csv"))


# temp %>%
#   group_by(model, return_year) %>% 
#   summarise(observed = sum(observed_returns),
#             predicted = sum(predicted_returns)) %>% 
#   ggplot(aes(observed, predicted)) +
#   geom_point() + 
#   facet_wrap(~model)

# temp %>%
#   filter(model == "smap_one system top6 ages") %>% 
#   group_by(model, return_year) %>%
#   summarise(observed = sum(observed_returns),
#             predicted = sum(predicted_returns)) %>%
#   ggplot() +
#   geom_col(aes(return_year, observed)) +
#   geom_point(aes(return_year, predicted)) +
#   facet_wrap(~ model)

save(all_res, file = file.path(rundir, "all_edm_res.Rdata"))

#------------------------------------------------
#Load raw results and create summary values
load(file = file.path(rundir, "all_edm_res.Rdata"))

#Add river name
rivs <- strsplit(all_res$pred_col, split = "_")
rivs <- plyr::ldply(rivs)
all_res$riv <- rivs$V1
#five years in future
npreds <- 5

to_loop <- all_res %>% distinct(theta, type, riv, predyr) 

total_summary <- lapply(1:nrow(to_loop), FUN = function(dd){
# total_summary <- lapply(1:50, FUN = function(dd){
	temp_row <- to_loop[dd, ]
	temp <- all_res %>% filter(theta == temp_row$theta, type == temp_row$type,
		riv == temp_row$riv, predyr == temp_row$predyr)
	temp1 <- plyr::ldply(temp$model_output) %>% group_by(time) %>% summarize(obs = sum(obs),
		pred = sum(pred)) %>% as.data.frame
	temp1 <- temp1 %>% filter(time >= temp_row$predyr, 
		time < (temp_row$predyr + npreds))
	summarize_predictions(temp1)
})
total_summary <- plyr::ldply(total_summary)

all_res_summ <- cbind(to_loop, total_summary)

all_res_summ %>% filter(rho > 0, mase < 1) %>% 
	group_by(riv, theta, type) %>% summarize(nyears = length(predyr)) %>%
	as.data.frame %>% arrange(desc(nyears))  %>% head

#------------------------------------------------
# 6. Plot results
#------------------------------------------------
#look at predictions for each of the rivers and one system, top6 ages
# all_res$pred_type <- 'smap'
# all_res[all_res$theta == 999, 'pred_type'] <- 'simplex'

to_loop <- all_res %>% distinct(type, riv, predyr, pred_type)

each_pred <- lapply(1:nrow(to_loop), FUN = function(ss){
	temp_loop <- to_loop[ss, ]
	temp <- all_res %>% filter(type == temp_loop$type, riv == temp_loop$riv, 
		predyr == temp_loop$predyr, pred_type == temp_loop$pred_type)
	tt1 <- plyr::ldply(temp$model_output) %>% group_by(time) %>% summarize(obs = sum(obs),
		pred = sum(pred)) %>% as.data.frame
	tt1$type <- temp_loop$type
	tt1$riv <- temp_loop$riv
	tt1$predyr <- temp_loop$predyr
	tt1$pred_type <- temp_loop$pred_type
	return(tt1)
})
each_pred <- plyr::ldply(each_pred)

write_csv(each_pred,file.path(results_dir, "raw_edm_results.csv"))

temp_comp <- each_pred %>% filter(type %in% c("one system top6 ages"), 
	predyr == 2000)

edm_results <- each_pred %>% 
  filter(type %in% c("one system top6 ages"),
         time == predyr)
                     
  


ggplot(temp_comp, aes(x = time)) + geom_point(aes(y = obs)) +
	geom_line(aes(y = pred, group = pred_type, col = pred_type)) + 
	facet_wrap(~ riv, scales = 'free_y') 

# ggplot(tt1, aes(x = time)) + geom_point(aes(y = obs)) + geom_line(aes(y = pred))

