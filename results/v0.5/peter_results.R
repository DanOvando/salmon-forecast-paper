#--------------------------------------------------------------------------------------
#set
options(max.print = 1000)
library(plyr)
library(reshape2)
library(tidyverse)
library(r4ss)
library(ggsidekick)
library(devtools)
library(doParallel)
library(ggridges)

setwd("C:/Users/peter.kuriyama/SynologyDrive/Research/postdoc/salmon-forecast-paper")

#--------------------------------------------------------------------------------------
#Source functions used below
#Requires creating a new column of lagged observations
my_mase <- function (y, y_hat, y_prev) 
{
  # print("make sure NAs filtered out of input")
  #Calculate the ae using the previous time steps
  den <- sum(ae(y, y_prev), na.rm = T)
  out <- sum(ae(y, y_hat)) / den
  return(out)
}

#Functions from tsensembler package
#Absolute error
ae <- function (y, y_hat, ...) 
{
  stopifnot(length(y) == length(y_hat), is.numeric(y), is.numeric(y_hat))
  abs(y - y_hat)
}

#Squared error
se <- function (y, y_hat, ...) 
{
  stopifnot(length(y) == length(y_hat), is.numeric(y), is.numeric(y_hat))
  (y - y_hat)^2
}

#Mean squared error
mse <- function(y, y_hat) mean(se(y, y_hat), na.rm = TRUE)

#Root mean squared error
rmse <- function (y, y_hat) sqrt(mse(y, y_hat))

#--------------------------------------------------------------------------------------
#Compile all the results
flz <- list.files("results/v0.5")
flz <- flz[grep("loo_results", flz)]

#Load in the results
res <- lapply(flz, FUN = function(xx) 
  temp <- read.csv(paste0("results/v0.5/", xx)))
res <- ldply(res)

#--------------------------------------------------------------------------------------
#Calculate mase for 
#Include lagged values
res <- res %>% group_by(model, system, age_group) %>%
    mutate(obs1 = c(NA, observed_returns[-length(age_group)])) %>%
    as.data.frame

#Calculate mase for each model, stream, age class
temp <- res %>% filter(system == "Igushik", age_group == "2_3", 
               model == "dlm",
               return_year >= 2000)

#Remove the lag_forecast and runmean_forecast from MASE calculations
res1 <- res %>% filter(model %in% c("lag_forecast", "runmean_forecast") == FALSE)

#Focus only on key age classes
res1 <- res1 %>% filter(age_group != "0_3", age_group != "1_4")

#--------------------------------------------------------------------------------------
#Look at total returns (across age classes) by river
tot_res <- res1 %>% group_by(model, return_year, system) %>%
  summarize(observed_returns = sum(observed_returns), 
            predicted_returns = sum(predicted_returns)) %>% as.data.frame

#add lagged observations for each river
tot_res <- tot_res %>% group_by(model, system) %>% 
  mutate(lag_obs = c(NA, observed_returns[-length(system)])) %>% as.data.frame

#Remove Alagnak and Togiak
tot_res <- tot_res %>% filter(system != "Alagnak", system != "Togiak")

#Look at MASE and RMSE, predictions from 2000 on
model_and_river <- tot_res %>% filter(return_year >= 2000) %>% group_by(model, system) %>% 
  summarize(mase_val = my_mase(observed_returns, predicted_returns, lag_obs),
            rmse_val = rmse(observed_returns, predicted_returns))

#Rank the rmse values and calculate percent worst higher than best
model_and_river <- model_and_river %>% group_by(system) %>% arrange(rmse_val) %>%
    mutate(rr = row_number())

outs <- tibble(system = unique(model_and_river$system))

for(ii in 2:6){
  tt <- model_and_river %>%  filter(rr %in% c(1, ii)) %>% group_by(system) %>%
    summarize(rmse_yy = round((rmse_val[2] - rmse_val[1])/rmse_val[1], digits = 2) * 100)
  outs <- cbind(outs, tt[, 2])
  names(outs)[ii] <- paste0("rmse_1to", ii)
}


model_and_river <- model_and_river %>% left_join(outs, by = 'system')
model_and_river <- model_and_river %>% as.data.frame


##Figure:
##Plot RMSE and MASE against each other, visualization of the level of error
##that results in model predictions beating lagged observation
model_and_river %>% ggplot(aes(x = rmse_val, y = mase_val)) + geom_point() + 
  theme_sleek() + geom_hline(yintercept = 1, lty = 2) + facet_wrap(~ system) + 
  xlab("RMSE") + ylab("MASE")

#Table of RMSE by model
model_and_river$rmse_val <- round(model_and_river$rmse_val, digits = 0)

model_and_river$rmse_val2 <- model_and_river$rmse_val

model_and_river[which(model_and_river$mase_val >= 1),
                'rmse_val2'] <- "-" 

nsigs <- model_and_river %>% group_by(model) %>% 
  mutate(nsigs = sum(mase_val < 1)) %>% distinct(model, nsigs) %>%
  arrange(nsigs)

#------------------------
#Table of Significant MASE and RMSE
model_and_river %>%
  arrange(model) %>% dcast(model ~ system, value.var = 'rmse_val2') %>%
  left_join(nsigs, by = 'model') %>% arrange(desc(nsigs)) %>%
  select(model, nsigs, Egegik, Ugashik, Wood, Igushik,
         Igushik, Kvichak, Naknek, Nushagak) %>%
  write.csv(file = "results/v0.5/river_mase_rmse.csv", row.names = FALSE)

#------------------------
#Figure of residuals
tot_res$resids <- tot_res$observed_returns - tot_res$predicted_returns 
tot_res %>% filter(return_year >= 2000) %>% 
  ggplot() + geom_hline(yintercept = 0) +
  geom_line(aes(x = return_year, y = resids, group = model,
                colour = model), alpha = .75) + 
  theme_sleek() + ylab("Residuals") +
  facet_wrap(~ system, scales = "free_y") + 
  theme(legend.position = c(.75, .15)) +
  ggsave(file = "results/v0.5/river_resids.png", width = 9, height = 7,
         units = 'in')

#--------------------------------------------------------------------------------------
#Which years were predictable?
#--------------------------------------------------------------------------------------
year_res <- tot_res %>% filter(return_year >= 2000) %>% 
  group_by(model, return_year) %>% 
  summarize(mase_val = my_mase(observed_returns,predicted_returns, lag_obs),
  rmse_val = rmse(observed_returns, predicted_returns)) 
  
year_res %>% 
  ggplot(aes(x = return_year, y = mase_val, colour = model,
             group = model)) + geom_line() + theme_sleek() +
  geom_point() +
  geom_hline(yintercept = 1) + xlab("Return year") + ylab("MASE") +
  theme(legend.position = c(.70, .8)) +
  ggsave("results/v0.5/year_sig.png", width = 9, height = 7, units = 'in')

#Years where all predictions are wrong
year_res %>% filter(mase_val < 1) %>% group_by(return_year) %>%
  summarize(nmods = length(model))
  
#Summed age classes 
#Which 

#--------------------------------------------------------------------------------------
#Look at age class predictions
#--------------------------------------------------------------------------------------

#--------------------------------------------------------------------------------------
#Calculate MASE values as metric for model performance
res1 <- res1 %>% filter(return_year >= 2000, system != "Alagnak",
                        system != "Togiak")

mase_res <- res1 %>% group_by(model, system, age_group) %>%
  summarize(mase_val = my_mase(observed_returns, predicted_returns, obs1),
            rmse_val = rmse(observed_returns, predicted_returns)) %>%
  as.data.frame

mase_res$mase_val <- round(mase_res$mase_val, digits = 2)
mase_res$rmse_val <- round(mase_res$rmse_val)


mase_res %>% group_by(model, age_group) %>% summarize(nsigs = sum(mase_val < 1)) %>%
  group_by(model) %>% mutate(sum_nsigs = sum(nsigs)) %>%
  dcast(model + sum_nsigs ~ age_group, value.var = 'nsigs') %>% 
  arrange(desc(sum_nsigs)) %>% write.csv(file = "results/v0.5/model_age_class.csv",
                                         row.names = FALSE)

#Look at year residuals for age classes
res1$resid <- res1$observed_returns - res1$predicted_returns

#1_2
res1 %>% filter(age_group == "1_2") %>% 
  ggplot(aes(x = return_year, y = resid, group = model,
                 colour = model)) + geom_line() + 
  geom_hline(yintercept = 0, lty = 2) +
  facet_wrap(~ system, scales = 'free_y') + theme_sleek() +
  theme(legend.position = c(.75, .15)) + ggtitle("Age Class 1.2") +
  xlab("Return Year") + ylab("Residuals") +
  ggsave("results/v0.5/residuals_1.2.png", width = 9, 
         height = 7)

#1_3
res1 %>% filter(age_group == "1_3") %>% 
  ggplot(aes(x = return_year, y = resid, group = model,
             colour = model)) + geom_line() + 
  geom_hline(yintercept = 0, lty = 2) +
  facet_wrap(~ system, scales = 'free_y') + theme_sleek() +
  theme(legend.position = c(.75, .15)) + ggtitle("Age Class 1.3") +
  xlab("Return Year") + ylab("Residuals") +
  ggsave("results/v0.5/residuals_1.3.png", width = 9, 
         height = 7)

#2_2
res1 %>% filter(age_group == "2_2") %>% 
  ggplot(aes(x = return_year, y = resid, group = model,
             colour = model)) + geom_line() + 
  geom_hline(yintercept = 0, lty = 2) +
  facet_wrap(~ system, scales = 'free_y') + theme_sleek() +
  theme(legend.position = c(.75, .15)) + ggtitle("Age Class 2.2") +
  xlab("Return Year") + ylab("Residuals") +
  ggsave("results/v0.5/residuals_2.2.png", width = 9, 
         height = 7)

#2_3
res1 %>% filter(age_group == "2_3") %>% 
  ggplot(aes(x = return_year, y = resid, group = model,
             colour = model)) + geom_line() + 
  geom_hline(yintercept = 0, lty = 2) +
  facet_wrap(~ system, scales = 'free_y') + theme_sleek() +
  theme(legend.position = c(.75, .15)) + ggtitle("Age Class 2.3") +
  xlab("Return Year") + ylab("Residuals") +
  ggsave("results/v0.5/residuals_2.3.png", width = 9, 
         height = 7)

           
