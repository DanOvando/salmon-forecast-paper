#------------------------------------------------------------------------------------
#Double Check Salmon rEDM
#------------------------------------------------------------------------------------
# options(max.print = 1000, device = 'windows')

library(plyr)
# library(rerddap)
library(reshape2)
library(tidyverse)
library(doParallel)
library(lubridate)
library(patchwork)
library(maps)
library(ggmap)
# library(ggsidekick)
library(devtools)
library(patchwork)
# devtools::install_github("SugiharaLab/rEDM")
library(rEDM)
states_map <- map_data("state")
world_map <- map_data("world")


# setwd("C://Users//peter.kuriyama//SynologyDrive//Research//noaa//salmon-forecast-paper")


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
agedat <- read.csv("data/2020.csv", stringsAsFactors = FALSE)
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
#999
#------------------------------------------------------------------------------------
#Multiview using only same river returns
#------------------------------------------------------------------------------------

#------------------------------------------------------------------------------------
#Multiview with annual returns
annualdat <- agedat %>% group_by(System, year) %>% summarize(ret = sum(ret))
annualdat <- annualdat %>% group_by(System) %>% mutate(ret_std = (ret - mean(ret)) / sd(ret))

annualdat <- annualdat %>% dcast(year ~ System, value.var = 'ret_std')
annualdat <- annualdat[11:nrow(annualdat), ]
eastside <- annualdat %>% select(year, Kvichak, Naknek, Alagnak, Egegik, Ugashik)
westside <- annualdat %>% select(-Kvichak, -Naknek, -Alagnak, -Egegik, -Ugashik)

nnk <- annualdat %>% select(year, Nushagak, Naknek, Kvichak, Wood)

#Define the function
system_multiview <- function(data_in, year_range, maxE,
                             nmultiview, targ){
  
  systempreds <- vector('list', length = length(year_range))
  for(ii in 1:length(year_range)){
    temp1 <- data_in
    cols <- paste0(names(temp1)[-1], collapse = " ")
    libs <- range(which(temp1$year < year_range[ii]))
    libs <- paste0(libs, collapse = " ")
    preds <- range(which(temp1$year > year_range[ii]))
    preds[1] <- preds[1] - maxE
    preds <- paste0(preds, collapse = " ")
    
    res <- Multiview(dataFrame = temp1, lib = libs, pred = preds, trainLib = T,
                     multiview = nmultiview, target = targ, E = maxE,
                     columns = cols)  
    
    #Process the results
    res$View$pred_system <- targ
    res$View$pred_year <- year_range[ii]
    res$Predictions$pred_system <- targ
    res$Predictions$pred_year <- year_range[ii]
    res$Predictions$obs1 <- c(NA, res$Predictions$Observations[-nrow(res$Predictions)])
    
    systempreds[[ii]] <- res
    print(ii)
  }

  outs1 <- lapply(systempreds, FUN = function(xx) xx[[1]])  
  outs1 <- ldply(outs1)
  outs2 <- lapply(systempreds, FUN = function(xx) xx[[2]])  
  outs2 <- ldply(outs2)
  outs <- list(View = outs1, Predictions = outs2)
  return(outs)
}

###------------------------
#Run with function

nnk2 <- lapply(names(nnk)[-1], FUN = function(zz){
  out <- system_multiview(data_in = nnk, year_range = 2000:2019,
                          maxE = 2, nmultiview = 2, targ = zz)
  return(out)
})

nnkres <- lapply(nnk2, FUN = function(qq) qq$Predictions)
nnkres <- ldply(nnkres)
# 
# 
# #group nushagak, naknek, wood, kvichak
# nnkres1 <- system_multiview(data_in = nnk, year_range = 2000:2019,
#                            maxE = 4, nmultiview = 1, targ = "Nushagak")
# nnkres2 <- system_multiview(data_in = nnk, year_range = 2000:2019,
#                             maxE = 4, nmultiview = 1, targ = "Naknek")
# nnkres3 <- system_multiview(data_in = nnk, year_range = 2000:2019,
#                             maxE = 4, nmultiview = 1, targ = "Wood")
# nnkres4 <- system_multiview(data_in = nnk, year_range = 2000:2019,
#                             maxE = 4, nmultiview = 1, targ = "Kvichak")

# nnkres <- rbind(nnkres1$Predictions, nnkres2$Predictions, nnkres3$Predictions,
#                 nnkres4$Predictions)
# save(nnkres, file = 'nnkres.Rdata')


p1 <- nnkres %>% filter(year == pred_year)
cor.test(p1$Observations, p1$Predictions)
my_mase(p1$Observations, p1$Predictions, p1$obs1)

p1 %>% group_by(pred_system) %>%
  summarize(rho = cor.test(Observations, Predictions)$estimate,
            pval = cor.test(Observations, Predictions)$p.value,
            nnkmase = my_mase(Observations, Predictions, obs1)) %>%
  as.data.frame


ggplot(p1, aes(x = year)) + 
  geom_point(aes(y = Observations)) +
  geom_line(aes(y = Predictions))+ 
  facet_wrap(~ pred_system)

#-----------------------------
#eastside
eastres <- lapply(names(eastside)[c(2, 3, 4, 5, 6)], FUN = function(yy){
  print(yy)
  tempout <- system_multiview(data_in = eastside, year_range = 2000:2019,
                              maxE = 2, nmultiview = 1, targ = yy)
  return(tempout)
})
# save(eastres, file = 'eastresE2.Rdata')
# save(eastres, file = 'eastres.Rdata')

load("eastres.Rdata")
eastresE3 <- eastres


eastpreds <- lapply(eastresE3, FUN = function(zz) zz$Predictions)
eastpreds <- ldply(eastpreds)

eastpreds %>% filter(year == pred_year)
p1 <- eastpreds %>% filter(year == pred_year)
cor.test(p1$Observations, p1$Predictions)
my_mase(p1$Observations, p1$Predictions, p1$obs1)

ggplot(p1, aes(x = year)) + geom_point(aes(y = Observations)) + 
  geom_line(aes(y = Predictions)) + facet_wrap(~ pred_system)

#-----------------------------
#westside
westside %>% melt(id.var = 'year') %>% ggplot(aes(x = year, y = value)) + geom_line() +
  facet_wrap(~ variable)

# westres <- lapply(names(westside)[c(2:5)], FUN = function(yy){
#   print(yy)
#   tempout <- system_multiview(data_in = westside, year_range = 2000:2019,
#                               maxE = 3, nmultiview = 1, targ = yy)
#   return(tempout)
# })
# save(westres, file = 'westresE3.Rdata')
load('westresE3.Rdata')
westpreds <- lapply(westres, FUN = function(zz) zz$Predictions)
westpreds <- ldply(westpreds)

westpreds %>% filter(year == pred_year)
p1 <- westpreds %>% filter(year == pred_year)
cor.test(p1$Observations, p1$Predictions)
my_mase(p1$Observations, p1$Predictions, p1$obs1)

ggplot(p1, aes(x = year)) + geom_point(aes(y = Observations)) + 
  geom_line(aes(y = Predictions)) + facet_wrap(~ pred_system)


#-----------------------
#Compare annual predictions to nnkres
nnkres$maxE <- 4
nnkres$predcols <- 'nnk'
nnkres$nnkpred <- nnkres$Predictions

eastpreds$maxE <- 3
eastpreds$predcols <- 'eastside'
eastpreds$eastpred <- eastpreds$Predictions

westpreds$maxE <- 3
westpreds$predcols <- 'westside'
westpreds$westpred <- westpreds$Predictions



p1 <- nnkres %>% select(year, Observations, obs1, nnkpred, pred_system, pred_year) 
p2 <- eastpreds %>% select(year, pred_system, pred_year, Predictions)
p3 <- westpreds %>% select(year, pred_system, pred_year, Predictions)


res <- p1 %>% left_join(rbind(p2, p3), by = c('year', 'pred_year', 'pred_system')) %>%
  filter(year == pred_year)

res %>% filter(pred_system == "Nushagak")

ggplot(res, aes(x = year)) + geom_point(aes(y = Observations)) +
  geom_line(aes(y = nnkpred), col = 'red') + facet_wrap(~ pred_system) +
  geom_line(aes(y = Predictions), col = 'black')


res %>% group_by(pred_system) %>%
  summarize(nnkrho = cor.test(Observations, nnkpred)$estimate,
            nnkpval = cor.test(Observations, nnkpred)$p.value,
            nnkmase = my_mase(Observations, nnkpred, obs1),
            siderho = cor.test(Observations, Predictions)$estimate,
            sidepval = cor.test(Observations, Predictions)$p.value,
            sidemase = my_mase(Observations, Predictions, obs1)) %>%
  as.data.frame




#------------------------------------------------------------------------------------
#Multiview function
#
# maxE <- 5
# year_range <- 2000:2001
# unq_inds = 1:6
# nmultiview = 5


multiview_years <- function(maxE = 5, year_range = 2000:2019, unq_inds = 1:6,
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
#999
#------------------------------------------------------------------------------------

time1 <- Sys.time()
E2 <- multiview_years(year_range = 2000:2019, 
                         unq_inds=1:4, maxE = 2, nmultiview = 1)
time2 <- Sys.time() - time1; time2

E2$preds %>% filter(year == pred_year)
save(E2, file = "Kvichak_E2.Rdata")

time1 <- Sys.time()
E3 <- multiview_years(year_range = 2000:2019, 
                      unq_inds=1:4, maxE = 3, nmultiview = 1)
time2 <- Sys.time() - time1; time2 #2.8 minutes
save(E3, file = "Kvichak_E3.Rdata")


time1 <- Sys.time()
E4 <- multiview_years(year_range = 2000:2019, 
                      unq_inds=1:4, maxE = 4, nmultiview = 1)
time2 <- Sys.time() - time1; time2 #10 minutes
save(E4, file = "Kvichak_E4.Rdata")

#------------------------------------------------------------------------------------
#Compare Kvichak results with different dimensions
load(file = "Kvichak_E2.Rdata")
load(file = "Kvichak_E3.Rdata")
load(file = "Kvichak_E4.Rdata")

E2$preds$E <- 2
E3$preds$E <- 3
E4$preds$E <- 4

kvi_preds <- rbind(E2$preds, E3$preds, E4$preds) %>% filter(year == pred_year)

lapply(2:4, FUN = function(xx){
  temp <- kvi_preds  %>% filter(E == xx)
  
  rho <- cor.test(temp$Observations, temp$Predictions)$estimate
  pval <- cor.test(temp$Observations, temp$Predictions)$p.value
  mase <- my_mase(temp$Observations, temp$Predictions, temp$obs1)
  out_ageclass <- data.frame(rho = round(rho, digits = 3), pval = round(pval, digits = 3),
                    mase = round(mase, digits = 3))
  out_ageclass$type <- 'age_specific'
  
  #Sum across year
  temp <- temp %>% group_by(year) %>% summarize(obs = sum(Observations), preds = sum(Predictions),
                                        obs1 = sum(obs1))
  
  rho <- cor.test(temp$obs, temp$preds)$estimate
  pval <- cor.test(temp$obs, temp$preds)$p.value
  mase <- my_mase(temp$obs, temp$preds, temp$obs1)
  out_year <- data.frame(rho = round(rho, digits = 3), pval = round(pval, digits = 3),
                         mase = round(mase, digits = 3))
  out_year$type <- 'year_sum'
  
  out <- rbind(out_ageclass, out_year)
  row.names(out) <- NULL
  return(out)
})



#------------------------------------------------------------------------------------
#Try all values with dimension 2
time1 <- Sys.time()
all2 <- multiview_years(year_range = 2000:2019, 
                      unq_inds=c(1:32), maxE = 2, nmultiview = 1)
time2 <- Sys.time() - time1; time2



time1 <- Sys.time()
all21 <- multiview_years(year_range = 2000:2019, 
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
finalres <- finalres %>% select(broodYr, retYr, System, age_class, obs, predictions)
finalres$model <- 'multiview'

write.csv(finalres, file = 'results/multiviewres.csv', row.names = FALSE)

#---------------------------------------
#compare results



names(finalres) <- c("brood_year", 'return_year', 'system', 'age_group', 
                     'multiviewobs', 'multiviewpred', 'model')
finalres$model <- NULL
finalres$age_group  <- gsub(" ", "_", finalres$age_group )

oldedm <- read.csv("results/v1.0.0.9000-2021/edm_loo_results.csv")
oldedm <- oldedm %>% left_join(finalres, by = c('brood_year', 'return_year',
                                                'system', 'age_group'))


oldedm <- oldedm[-which(is.na(oldedm$multiviewobs)), ]
oldedm$multiviewobs <- oldedm$multiviewobs * 1000
oldedm$multiviewpred <- oldedm$multiviewpred * 1000


oldedm %>% group_by(system) %>%
  summarize(old_rho = cor.test(observed_returns, predicted_returns)$estimate,
            old_pval = cor.test(observed_returns, predicted_returns)$p.value,
            new_rho = cor.test(observed_returns, multiviewpred)$estimate,
            new_pval = cor.test(observed_returns, multiviewpred)$p.value) %>% as.data.frame
            






oldedm <- oldedm %>% filter(!is.na(multiviewpred))

finalres <- finalres %>% select(names(oldedm))

oldedm <- oldedm %>% left_join(finalres, by = c(""))

head(finalres)






























plot(finalres$obs, finalres$predictions)




temp <- agedat
temp$ret_std <- NULL
temp$obs <- NULL

temp$ret_std <- (temp$ret - mean(temp$ret)) / sd(temp$ret)
m1 <- mean(agedat$ret)




agedat$obs <- agedat$ret_std * sd(agedat_all$ret) + mean(agedat_all$ret)
  



sd(agedat$ret)
agedat$ret_std <- (agedat$ret - mean(agedat$ret)) / sd(agedat$ret)




#----------------------
res2_summ <- res2 %>% group_by(year, pred_system) %>% 
  summarize(obs = sum(Observations), pred = sum(Predictions),
            obs1 = sum(obs1)) 

cor.test(res2_summ$obs, res2_summ$pred)

plot(res2_summ$obs, res2_summ$pred)
abline(a = 0, b = 1, lty = 2)


my_mase(res2_summ$obs, res2_summ$pred, res2_summ$obs1)

# %>% ggplot(aes(x = year)) +
#   geom_point(aes(y = obs)) + geom_line(aes(y = pred)) +
#   geom_line(aes(y = pred)) + facet_wrap(~ pred_system)
  


res2 %>% filter(pred_system == "Kvichak") %>% 
  ggplot(aes(x = year)) + geom_point(aes(y = Observations)) +
  geom_line(aes(y = Predictions)) + facet_wrap(~ pred_unq)

res2 %>% filter(pred_system == "Igushik") %>% 
  ggplot(aes(x = year)) + geom_point(aes(y = Observations)) +
  geom_line(aes(y = Predictions)) + facet_wrap(~ pred_unq)



time1 <- Sys.time()
all3 <- multiview_years(year_range = 2000:2019, 
                        unq_inds=c(1:32), maxE = 3, nmultiview = 1)
time2 <- Sys.time() - time1; time2

res3 <- all3$preds %>% filter(year == pred_year)
cor.test(res3$Observations, res3$Predictions)
my_mase(res3$Observations, res3$Predictions, res3$obs1)



nush2 <- multiview_years(year_range = 2000:2019, 
                         unq_inds=33:35, maxE = 2, nmultiview = 1)

agedat %>% filter(System == "Nushagak") %>%
  ggplot(aes(x = year, y = ret)) + geom_point() + geom_line() +
  facet_wrap(~ unq)


alldat[33:36,]

unqs[33:36,]

kvi_preds %>% fil








time1 <- Sys.time()
E4_top3 <- multiview_years(year_range = 2000:2019, 
                      unq_inds=1:4, maxE = 4, nmultiview = 3)
time2 <- Sys.time() - time1; time2 #10 minutes














rr <- E4_top3[[2]] %>% filter(pred_year == year)

rr %>% group_by(year, pred_system) %>% summarize(obs = sum(Observations),
                                                 preds = sum(Predictions), 
                                                 obs1 = sum(obs1)) %>%
  ggplot(aes(x = year)) + geom_point(aes(y = obs)) + geom_line(aes(y = preds))



my_mase(rr$Observations, rr$Predictions, rr$obs1)
plot(rr$Observations, rr$Predictions)
cor.test(rr$Observations, rr$Predictions)

for(ii in 1:length(unique(rr$pred_unq))){
  temp <- rr %>% filter(pred_unq == unique(rr$pred_unq)[ii])
  print(my_mase(temp$Observations, temp$Predictions, temp$obs1))
}

rr %>% filter(pred_unq == "Kvichak_1_3") %>% ggplot(aes(x = year)) +
  geom_point(aes(y = Observations)) + geom_line(aes(y = Predictions))

# save(res, file = "salmon_revisit_2000_2006_1_6_maxE5.Rdata")
# load(file = "salmon_revisit_2000_2006_1_6_maxE5.Rdata")

rr <- res[[2]] %>% filter(pred_year == year)


my_mase(rr$Observations, rr$Predictions, rr$obs1)



plot(rr$Observations, rr$Predictions)
cor.test(rr$Observations, rr$Predictions)





#Look at results

view5 <- lapply(multiview5, FUN = function(xx) xx[[1]])
view5 <- ldply(view5)

preds5 <- lapply(multiview5, FUN = function(xx) xx[[2]])
preds5 <- ldply(preds5)

preds5 %>% filter(year == pred_year, Predictions < 5) %>% ggplot(aes(x = Observations, y = Predictions)) +
  geom_point() + 
  geom_abline(slope = 1)



view5


cor.test(preds5$Observations, preds5$Predictions)





#Naknek


head(agedat$ret_std)

tail(agedat)

#Multiview using only same river returns
paste("Kvichak_1_2", "Kvichak_1_3", "Kvichak_2_2", "Kvichak_2_3", "Naknek_1_2",  "Naknek_1_3", 
      "Naknek_2_2",  "Naknek_2_3", collapse = " ")

#Multiview
res <- Multiview(dataFrame = nn, lib = "1 44", pred = "43 60", trainLib = T,
                 multiview = 10, target = "Naknek_2_2", E = 2,
                 columns = paste("Kvichak_1_2", "Kvichak_1_3", "Kvichak_2_2", "Kvichak_2_3", "Naknek_1_2",  "Naknek_1_3", 
                                 "Naknek_2_2",  "Naknek_2_3", collapse = " "))
plot(res$Predictions$time, res$Predictions$Observations)
lines(res$Predictions$time, res$Predictions$Predictions)


#Look at only the most common age classes
all_unqs <- agedat %>% filter(age_class %in% focus_ages) %>% distinct(unq) %>%
  pull(unq)

bestEs <- lapply(all_unqs, FUN = function(xx){
  temp <- agedat %>% filter(unq == xx)

  temp <- temp %>% select(retYr, ret_std)
  librange <- paste("1", as.character(nrow(temp)))
  E.opt <- EmbedDimension(dataFrame = temp, lib = librange,
                          pred = librange, columns = "ret_std",
                          target = 'ret_std', showPlot = FALSE)
  E.opt$unq <- xx
  E.opt$nvals <- nrow(temp)
  return(E.opt)
})

bestEs <- ldply(bestEs)

#Identify best rho values but also low E values for parsimony
bestEs <- bestEs %>% group_by(unq) %>% arrange(desc(rho), E) %>% slice(1:3) %>% 
  group_by(unq) %>% mutate(minE = min(E), maxE = max(E), rho_diff = max(rho) - 
                             min(rho), perc_diff = round(rho_diff / max(rho), digits = 2)) %>%
  as.data.frame
  
#Alagnak, Egegik, Igushik, Kvichak, Naknek, Nushagak, Togiak, Ugashi, Wood

alag <- bestEs[grep("Alagnak", bestEs$unq), ]
alag <- alag[c(2, 6, 7, 11), ]

egeg <- bestEs[grep("Egegik", bestEs$unq), ]
row.names(egeg) <- 1:nrow(egeg)
egeg <- egeg[c(1, 4, 7, 11), ]

igus <- bestEs[grep("Igushik", bestEs$unq), ]
row.names(igus) <- 1:nrow(igus)
igus <- igus[c(1, 4, 7, 10), ]

kvic <- bestEs[grep("Kvichak", bestEs$unq), ]
row.names(kvic) <- 1:nrow(kvic)
kvic <- kvic[c(1, 5, 8, 10), ]

nakn <- bestEs[grep("Naknek", bestEs$unq), ]
row.names(nakn) <- 1:nrow(nakn)
nakn <- nakn[c(1, 4, 8, 10), ]

nush <- bestEs[grep("Nushagak", bestEs$unq), ]
row.names(nush) <- 1:nrow(nush)
nush <- nush[c(1, 4, 7, 10), ]

togi <- bestEs[grep("Togiak", bestEs$unq), ]
row.names(togi) <- 1:nrow(togi)
togi <- togi[c(1, 4, 7, 10), ]


ugas <- bestEs[grep("Ugashik", bestEs$unq), ]
row.names(ugas) <- 1:nrow(ugas)
ugas <- ugas[c(2, 5, 7, 10), ]

wood <- bestEs[grep("Wood", bestEs$unq), ]
row.names(wood) <- 1:nrow(wood)
wood <- wood[c(1, 5, 7, 11), ]

bestEs <- rbind(alag, egeg, igus, kvic,
                nakn, nush, togi, ugas, wood)

#--------------------------------------------------
#Do S-map to find bests


 
#--------------------------------------------------
#Full run through with Naknek
nakn <- agedat %>% filter(System == "Naknek", 
                          age_class %in% c("2 2", "1 3", "1 2", "2 3"))

year <- 2000

#See if E changes over time

temp <- nakn %>% filter(age_class == "2 2")
temp1 <- temp %>% select(retYr, ret_std)

bestEs <- lapply(2000:2019, FUN = function(xx){
  librange <- paste(range(which(temp1$retYr < xx)), collapse = " ")
  temp1 <- temp %>% select(retYr, ret_std) %>% filter(retYr < xx)
  E.opt <- EmbedDimension(dataFrame = temp1, lib = librange,
                          pred = librange, columns = "ret_std",
                          target = 'ret_std', showPlot = FALSE)
  bestE <- E.opt %>% arrange(desc(rho)) %>% slice(1:3)
  bestE$year <- xx
  return(bestE)  
})

bestEs <- ldply(bestEs)


bestEs %>% group_by(year) %>% filter(rho == max(rho))

bestEs %>% group_by(year) %>% filter(rho == max(rho))


#bestE is 8 for Naknek 2 2
best_theta <- lapply(2000:2019, FUN = function(xx){
  temp1 <- temp %>% select(retYr, ret_std) %>% filter(retYr < xx)
  librange <- paste(range(which(temp1$retYr < xx)), collapse = " ")
  
  
  theta <- PredictNonlinear(dataFrame = temp1, lib = librange,
                            pred = librange, columns = 'ret_std',
                            target = 'ret_std', showPlot = FALSE, 
                            E = 8)
  
  # E.opt <- EmbedDimension(dataFrame = temp1, lib = librange,
  #                         pred = librange, columns = "ret_std",
  #                         target = 'ret_std', showPlot = FALSE)
  besttheta <- theta %>% arrange(desc(rho)) %>% slice(1:3)
  besttheta$year <- xx
  return(besttheta)  
})
best_theta <- ldply(best_theta)

table(best_theta$Theta)





#Now throw everything at Naknek
#---------------------------------------------
#Try to predict Naknek 2 2 from all other Nakneks
nn <- agedat %>% filter(age_class %in% c("2 2", "1 3", "1 2", "2 3"),
                        System %in% c("Naknek", "Kvichak")) %>% 
  dcast(retYr ~ unq, value.var = 'ret_std')
nn <- nn %>% filter(retYr >= 1956)
names(nn) <- gsub(" ", "_" , names(nn))
names(nn)[1] <- 'time'


#Try multivariate
SMap(dataFrame = nn, E = 8, theta = 1.5, target = "Naknek_2_2", 
     columns = c(2, 3, 4, 5))


#Try multivariate
nnblock <- MakeBlock(dataFrame = nn, E = 3, columns = names(nn)[-1])
# nnblock$time <- nn$time





s3dim <- Simplex(dataFrame = nnblock, E = 3,
                 columns = paste(names(nnblock)[c(7, 8, 9, 1)], collapse = " "),
                 target = "Naknek_2_2(t-0)", embedded = TRUE,
                 lib = "1 50", pred = '51 65')

paste("Kvichak_1_2", "Kvichak_1_3", "Kvichak_2_2", "Kvichak_2_3", "Naknek_1_2",  "Naknek_1_3", 
      "Naknek_2_2",  "Naknek_2_3", collapse = " ")

#Multiview
res <- Multiview(dataFrame = nn, lib = "1 44", pred = "43 60", trainLib = T,
          multiview = 10, target = "Naknek_2_2", E = 2,
          columns = paste("Kvichak_1_2", "Kvichak_1_3", "Kvichak_2_2", "Kvichak_2_3", "Naknek_1_2",  "Naknek_1_3", 
                          "Naknek_2_2",  "Naknek_2_3", collapse = " "))
plot(res$Predictions$time, res$Predictions$Observations)
lines(res$Predictions$time, res$Predictions$Predictions)


#--------------------------------------
#Check multiview with just one combination
test_kvi <- Multiview(dataFrame = nn, lib = "1 44", pred = "43 60", trainLib = T,
                     multiview = 1, target = "Naknek_2_2", E = 1,
                     columns = paste("Naknek_1_2",  "Naknek_1_3", 
                                     "Naknek_2_2",  "Naknek_2_3", collapse = " "))
test_kvi$Predictions
Simplex(dataFrame = nn, lib = "1 44", pred = '43 60', target = "Naknek_2_2", 
        columns = "Naknek_1_2 Naknek_1_3 Naknek_2_2 Naknek_2_3", E = 1)

#--------------------------------------
#Multiview for 20 years of out of sample predictions, average the top 10 for each run
#Process for 20 years of predictions
data_in <- 



start_time <- Sys.time()
res_kvi <- Multiview(dataFrame = nn, lib = "1 44", pred = "43 60", trainLib = T,
                     multiview = 10, target = "Naknek_2_2", E = 2,
                     columns = paste("Naknek_1_2",  "Naknek_1_3", 
                                     "Naknek_2_2",  "Naknek_2_3", collapse = " "))
run_time <- Sys.time() - start_time; run_time


res_kvi1 <- Multiview(dataFrame = nn, lib = "1 49", pred = "43 60", trainLib = T,
                     multiview = 10, target = "Naknek_2_2", E = 5,
                     columns = paste("Naknek_1_2",  "Naknek_1_3", 
                                     "Naknek_2_2",  "Naknek_2_3", collapse = " "))

res_kvi$Predictions %>% left_join(res_kvi1$Predictions, by = c("time", "Observations"))




cor.test(res$Predictions$Observations, res$Predictions$Predictions)


mview <- Multiview(dataFrame = nn, lib = "1 44", pred = "45 47", E = 3,
                   columns = "Naknek_1_2 Naknek_1_3 Naknek_2_2 Naknek_2_3",
          target = "Naknek_2_2")


mview$View %>% arrange(desc(rho)) %>% head


preds <- mview$Predictions %>% filter(time >= 2002, time <= 2020)
cor.test(preds$Observations, preds$Predictions)

Mview = Multiview(dataFrame = block_3sp, lib = "1 100", pred = "101 190", E = 3,
                  columns = "x_t y_t z_t", target = "x_t")










temp <- agedat %>% filter(unq == "Kvichak 0 2")

df = data.frame(yr = as.numeric(time(sunspot.year)), 
                sunspot_count = as.numeric(sunspot.year))
E.opt = EmbedDimension( dataFrame = df,    # input data
                        lib     = "1 280", # portion of data to train
                        pred    = "1 280", # portion of data to predict
                        columns = "sunspot_count",
                        target  = "sunspot_count" , showPlot = FALSE)


simplex = Simplex( dataFrame = df, 
                   lib     = "1   190", # portion of data to train
                   pred    = "191 287", # portion of data to predict
                   columns = "sunspot_count",
                   target  = "sunspot_count",
                   E       = 3 )
cor.test(simplex$Observations, simplex$Predictions)



plot( df$yr, df$sunspot_count, type = "l", lwd = 2,
      xlab = "year", ylab = "sunspots")
lines( simplex$yr, simplex$Predictions, col = "red", lwd = 2)
legend( 'topleft', legend = c( "Observed", "Predicted (year + 1)" ),
        fill = c( 'black', 'red' ), bty = 'n', cex = 1.3 )


E.opt = EmbedDimension( dataFrame = temp,    # input data
                        lib     = "1 68", # portion of data to train
                        pred    = "1 68", # portion of data to predict
                        columns = "ret_std",
                        target  = "ret_std" )

res <- simplex(temp$ret_std, E = 1:10)


res %>% filter(rho == max(rho))


agedat %>% filter(unq == "Kvichak 0 2")

agedat %>% group_by(unq) %>% summarize()


agedat$ret
