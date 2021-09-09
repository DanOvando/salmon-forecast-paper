#------------------------------------------------------------------------------------
#Double Check Salmon rEDM
#------------------------------------------------------------------------------------
# options(max.print = 1000, device = 'windows')

# library(plyr)
# # library(rerddap)
# library(reshape2)
# library(tidyverse)
# library(doParallel)
# library(lubridate)
# library(patchwork)
# library(maps)
# library(ggmap)
# # library(ggsidekick)
# library(devtools)
# library(patchwork)
# devtools::install_github("SugiharaLab/rEDM")
# library(rEDM)
# states_map <- map_data("state")
# world_map <- map_data("world")


# setwd("C://Users//peter.kuriyama//SynologyDrive//Research//noaa//salmon-forecast-paper")

ldply <- plyr::ldply
ae <- function (y, y_hat) 
{
  stopifnot(length(y) == length(y_hat), is.numeric(y), is.numeric(y_hat))
  abs(y - y_hat)
}

my_mase <- function (y, y_hat, y_prev) 
{
  # print("make sure NAs filtered out of input")
  #Calculate the ae using the previous time steps
  den <- sum(ae(y, y_prev), na.rm = T)
  out <- sum(ae(y, y_hat)) / den
  return(out)
}
#------------------------------------------------------------------------------------
#Age class data
#------------------------------------------------------------------------------------
agedat <- read.csv(here("data",paste0(return_table_year,".csv")), stringsAsFactors = FALSE)
agedat$age_class <- paste(agedat$fwAge, agedat$oAge)
# agedat %>% group_by(age_class) %>% summarize(avg_ret = mean(ret)) %>% arrange(desc(avg_ret))

#Only try to predict the 2 2, 1 3, 1 2, 2 3 age classes
agedat$unq <- paste(agedat$System, agedat$age_class)
agedat$unq <- gsub(" ", "_", agedat$unq)
agedat$ret_std <- (agedat$ret - mean(agedat$ret)) / sd(agedat$ret)
focus_ages <- c("2 2", "1 3", "1 2", "2 3")
agedat_all <- agedat
agedat <- agedat_all %>% filter(age_class %in% focus_ages)
agedat$year <- agedat$retYr

unqs <- agedat %>% distinct(System, unq)

#------------------------------------------------------------------------------------
#Multiview function
#
# maxE <- 5
# year_range <- 2000:2001
# unq_inds = 1:6
# nmultiview = 5


multiview_years <- function(maxE = 5, year_range = first_year:(last_year - 1), unq_inds = 1:6,
                            nmultiview = 5){
  
  out <- vector('list', length = length(year_range))
  for(ii in 1:length(year_range)){
    start_time <- Sys.time()
    one_run <- lapply(unq_inds, FUN = function(xx){
      #Process the values and dcast
      temp_sys <- unqs[xx, ]
      temp <- agedat %>% filter(System == temp_sys$System)
      temp1 <- temp %>% select(year, unq, ret_std) %>% dcast(year ~ unq, 
                                                             value.var = 'ret_std')
      
      #Filter out NAs
      if(sum(is.na(rowSums(temp1[, 2:5]))) > 0){
        temp1 <- temp1[-which(is.na(rowSums(temp1 %>% select(-year)))), ]  
      }
      
    
      cols <- paste0(names(temp1)[-1], collapse = " ")
      libs <- range(which(temp1$year < year_range[ii]))
      libs <- paste0(libs, collapse = " ")
      preds <- range(which(temp1$year > year_range[ii]))
      preds[1] <- preds[1] - maxE
      preds <- paste0(preds, collapse = " ")
      
      res <- Multiview(dataFrame = temp1, lib = libs, pred = preds, trainLib = T,
                       multiview = nmultiview, target = temp_sys$unq, E = maxE,
                       columns = cols)  
      
      #Process the results
      res$View$pred_unq <- temp_sys$unq
      res$View$pred_system <- temp_sys$System
      res$View$pred_year <- year_range[ii]
      res$Predictions$pred_unq <- temp_sys$unq
      res$Predictions$pred_system <- temp_sys$System
      res$Predictions$pred_year <- year_range[ii]
      res$Predictions$obs1 <- c(NA, res$Predictions$Observations[-nrow(res$Predictions)])
    
      return(res)  
    })
    out_view <- lapply(one_run, FUN = function(zz) zz[[1]])
    out_view <- ldply(out_view)
    
    out_preds <- lapply(one_run, FUN = function(zz) zz[[2]])
    out_preds <- ldply(out_preds)
  
    out1 <- list(View = out_view, preds = out_preds)
    run_time <- Sys.time() - start_time
    print(run_time)
    out[[ii]] <- out1
  }
  
  views <- lapply(out, FUN = function(ll) ll[[1]])
  views <- ldply(views)
  
  preds <- lapply(out, FUN = function(ll) ll[[2]])
  preds <- ldply(preds)
  
  all_res <- list(views = views, preds = preds)
  return(all_res)
}

#------------------------------------------------------------------------------------
#Try all values with dimension 2
time1 <- Sys.time()
all2 <- multiview_years(year_range = first_year:(last_year - 1), 
                      unq_inds=c(1:32), maxE = 2, nmultiview = 1)
time2 <- Sys.time() - time1; time2



time1 <- Sys.time()
all21 <- multiview_years(year_range = first_year:(last_year - 1), 
                        unq_inds=c(33:36), maxE = 2, nmultiview = 1)
time2 <- Sys.time() - time1; time2


all2$views <- rbind(all2$views, all21$views)
all2$preds <- rbind(all2$preds, all21$preds)

#age class predictions with E of 2
save(all2, file = "all2.Rdata")


res2 <- all2$preds %>% filter(year == pred_year)

res2 %>% group_by(pred_unq) %>%
  summarize(rho = cor.test(Observations, Predictions)$estimate,
            pval = cor.test(Observations, Predictions)$p.value,
            nnkmase = my_mase(Observations, Predictions, obs1)) %>%
  as.data.frame

#Back transform the predictions
res2$predictions <- res2$Predictions * sd(agedat_all$ret) + mean(agedat_all$ret)
res2$observations <- res2$Observations * sd(agedat_all$ret) + mean(agedat_all$ret)


ff <- res2 %>% select(year, pred_unq, predictions)
names(ff)[2] <- 'unq'


finalres <- agedat %>% left_join(ff, by = c("year", 'unq'))
finalres <- finalres %>% filter(!is.na(predictions))
finalres <- finalres %>% select(broodYr, retYr, System, age_class, ret, predictions)
finalres$model <- 'multiview'



edm_model <- finalres %>%
  rename(brood_year = broodYr,
         return_year = retYr,
         observed_returns = ret,
         Forecast = predictions) %>% 
  mutate(age_group = str_replace_all(age_class," ","_")) %>% 
  select(model,
         brood_year,
         return_year,
         System,
         age_group,
         observed_returns,
         Forecast) %>%
  mutate(Forecast = Forecast / 1e3) %>%
  rename("system" = "System", "predicted_returns" =
           "Forecast")


readr::write_csv(edm_model, path = file.path(results_dir,'edm_loo_results.csv'))
