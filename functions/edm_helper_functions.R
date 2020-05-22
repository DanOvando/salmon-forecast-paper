#--------------------------------------------------------
#Helper Functions to perform different tasks
#--------------------------------------------------------
standardize <- function(input){
  output <- (input - mean(input, na.rm = T)) / sd(input, na.rm = T)
  return(output)
}

unstandardize <- function(input, mean_val = mean(tot_dat$ret), 
  sd_val = sd(tot_dat$ret)){
  output <- (input * sd_val) + mean_val
  return(output)
}

#MASE function
my_mase <- function (y, y_hat, y_prev) 
{
    # print("make sure NAs filtered out of input")
    #Calculate the ae using the previous time steps
    den <- sum(ae(y, y_prev), na.rm = T)
    out <- sum(ae(y, y_hat)) / den
    return(out)
}


composite_predict_one <- function(pred_location, input, years = 2000:2019,
  pred_column = 'ret', sync_data ){

  uni_dat <- input %>% filter(unq == pred_location)

  #=========
  copreds <- sync_data %>% filter(pred == pred_location) %>%
    distinct(lib) %>% pull
  comp_dat <- dat %>% filter(unq %in% c(copreds, xx))

  #=========
  uni_preds <- predict_one(input = uni_dat, lib_system = NA,
    pred_system = xx, years = years, targ_col = pred_column)  
  comp_preds <- predict_one(input = comp_dat, lib_system = copreds,
    pred_system = xx, years = years, targ_col = pred_column)

  #=========
  #Compare the observations to predictions
  obs <- dat %>% filter(unq == pred_location, retYr %in% years) %>% pull(ret)
  uni_preds$obs <- obs
  comp_preds$obs <- obs
  uni_preds$type <- 'univariate'
  comp_preds$type <- 'composite'
  prevs <- uni_dat %>% filter(retYr %in% (years - 1)) %>% 
    pull(pred_column)
  uni_preds$prevs <- prevs
  comp_preds$prevs <- prevs
  outs <- rbind(uni_preds, comp_preds)

  #=====
  #compute summary statistics
  outs <- outs %>% group_by(unq, type) %>% 
    mutate(simplex_rho = cor.test(obs, simplex_pred)$est,
      simplex_pval = cor.test(obs, simplex_pred)$p.val,
      simplex_mase = my_mase(obs, simplex_pred, prevs),
      smap_rho = cor.test(obs, smap_pred)$est,
      smap_pval = cor.test(obs, smap_pred)$p.val,
      smap_mase = my_mase(obs, smap_pred, prevs)) %>% as.data.frame
  return(outs)
  #Example script
# composite_predict_one(pred_location = locs[1],
#   input = dat, sync_data = best_copreds)

}


  
predict_one <- function(input = dat, lib_system, pred_system = NA,
  E_vals = 1:10, theta_vals = seq(0, 10, .5), years = 2000:2010,
  time_col = 'retYr', targ_col = 'ret_std'){

  #Update years in case there are missing years
  #Specify temp_all depending on if focusing on unique system
  #------------------------------------------
  #Sort out system conditions
  if(sum(is.na(c(lib_system, pred_system))) > 0) temp_all <- input
  if(sum(is.na(c(lib_system, pred_system))) == 0){
    temp_lib <- input %>% filter(unq %in% lib_system)
    temp_pred <- input %>% filter(unq == pred_system)
    temp_all <- rbind(temp_lib, temp_pred)
  } 

  #------------------------------------------
  # temp_all <- input %>% filter(unq == unique_system)
  yr_inds <- which(temp_all[, time_col] >= years[1] & 
    temp_all[, time_col] <= years[length(years)])
  years <- temp_all[yr_inds, time_col]
  years <- unique(years)

  #------------------------------------------
  #Run the prediction for each year
  preds <- lapply(1:length(years), FUN = function(ii){    
    temp <- temp_all[which(temp_all[, time_col] < years[ii]), ]
    # temp <- temp_all %>% filter(retYr < years[ii])
    
    if(sum(temp$ret == 0) >= .75 * nrow(temp)){
      preds <- data.frame(year = years[ii], unq = pred_system,
        simplex_pred = 999, smap_pred = 999)
      return(preds)
    }

    bestE <- simplex(temp[, targ_col], E = E_vals, silent = T) %>% 
      filter(is.na(rho) == FALSE) %>%
      filter(rho == max(rho))
    
    besttheta <- s_map(temp[, targ_col], E = bestE$E, silent = T,
      theta = theta_vals) %>% filter(rho == max(rho))    

    #Make prediction for next year
    lib1 <- temp[, targ_col]
    l1 <- c(1, length(lib1))
    p1 <- c(length(lib1) - bestE$E, length(lib1) + 1)
    simp_pred1 <- simplex(c(lib1, NA), E = bestE$E, silent = T, stats_only = F,
      lib = l1, pred = p1)$model_output[[1]]
    smap_pred1 <- s_map(c(lib1, NA), E = bestE$E, theta = besttheta$theta,
      silent = T, stats_only = F, lib = l1, pred = p1)$model_output[[1]]    

    outs <- data.frame(year = years[ii], unq = pred_system,
     simplex_pred = simp_pred1[nrow(simp_pred1) - 1, 'pred'],
     smap_pred = smap_pred1[nrow(smap_pred1) - 1, 'pred'],
     E = bestE$E, theta = besttheta$theta)

    return(outs)
  })

  preds <- plyr::ldply(preds)
  return(preds)
}


copred_regime <- function(input = dat, years, E_vals = 1:10, focus_col = "ret_std",
  min_nonzero = 5, ncores = detectCores() - 2){

  #Define the mymase function
  my_mase <- function (y, y_hat, y_prev) 
  {
    # print("make sure NAs filtered out of input")
    #Calculate the ae using the previous time steps
    den <- sum(ae(y, y_prev), na.rm = T)
    out <- sum(ae(y, y_hat)) / den
    return(out)
  }

  #Perhaps an arbitrary decision
  #Unq has to have at least 5 nonzero values
  rr <- input %>% filter(retYr > years[1] & retYr <= years[2])
  rr$ret_std <- round(rr$ret_std, digits = 10)
  rr1 <- rr %>% group_by(unq) %>% summarize(nvals = length(unique(ret_std))) %>% 
    arrange(desc(nvals)) %>% filter(nvals > min_nonzero)
  combos <- rr1 %>% pull(unq)

  combos <- expand.grid(combos, combos, stringsAsFactors = F)
  combos <- data.frame(lib = combos$Var1, pred = combos$Var2, stringsAsFactors = F)
  combos <- combos %>% filter(lib != pred)

  #Quantify coprediction within each site in parallel
  cl <- makeCluster(ncores)
  registerDoParallel(cl)
  
  start_time <- Sys.time()
  copreds <- foreach::foreach(yy = 1:nrow(combos),
# copreds <- foreach::foreach(yy = 1:100,
  .packages = c("plyr", "reshape2","tidyverse", "rEDM",
    "tsensembler")) %dopar% {
    temp1 <- input %>% filter(retYr >= years[1], retYr <= years[2],
      unq == combos[yy, 'lib'])
    temp2 <- input %>% filter(retYr >= years[1], retYr <= years[2],
      unq == combos[yy, 'pred'])
      
    temp <- rbind(temp1, temp2)

    #Find indices of lib and pred
    lib_inds <- range(which(temp$unq == combos[yy, 'lib']))
    pred_inds <- range(which(temp$unq == combos[yy, 'pred']))

    bestE <- simplex(temp[, focus_col], lib = lib_inds, pred = pred_inds, E = E_vals,
      silent = T, stats_only = F) %>% filter(mae == min(mae))
    bestE$copred <- paste(combos[yy, 'lib'], "to", combos[yy, 'pred'])
    bestE$lib_loc <- combos[yy, 'lib']
    bestE$pred_loc <- combos[yy, 'pred']
    
    #Calculate MASE
    mm <- bestE$model_output[[1]]
    mm$obs1 <- c(NA, mm$obs[-nrow(mm)])
    mm <- mm %>% filter(is.na(pred) == F)
    bestE$mase <- my_mase(mm$obs, mm$pred, mm$obs1)
    return(bestE)
  }
  stopCluster(cl)
  copreds <- plyr::ldply(copreds)
  copreds$max_year <- years[2]
  run_time <- Sys.time() - start_time; print(run_time)

  return(copreds)
}



summary_metrics <- function(the_obs, ests, previous_obs){
  res <- data.frame(rho = cor.test(the_obs, ests)$estimate,
    p_val = cor.test(the_obs, ests)$p.value,
    mase = my_mase(the_obs, ests, previous_obs))
  row.names(res) <- NULL
  res <- unlist(res)
  return(res)
}


#Function to make OOS predictions with regimes and data as input
oos_prediction <- function(regimes_in, dat_in, npreds = 5, focus_col = 'ret_std',
  E_vals = 1:10, ncores){
  r_combos <- regimes_in %>% distinct(lib_loc, max_year) #Regime combinations
  
  cl <- makeCluster(ncores)
  registerDoParallel(cl)
  
  start_time <- Sys.time()
  #Loop through the lib_loc and max_year values to predict one year into the future  
  preds <- foreach::foreach(rr = 1:nrow(r_combos),
    .packages = c("plyr", "reshape2", "tidyverse", "rEDM")) %dopar% {
  #Find the synchronous locations
    rtemp <- regimes_in %>% filter(lib_loc == r_combos[rr, "lib_loc"], 
      max_year == r_combos[rr, 'max_year']) %>% pull(pred_loc)
    rtemp <- c(rtemp, r_combos[rr, 'lib_loc'])
  
    #Filter the data corresponding to the locations and the years
    temp <- dat_in %>% filter(retYr <= r_combos[rr, 'max_year'],
      dat_in$unq %in% rtemp)
    temp <- temp %>% arrange(unq, retYr) 

    #Estimate dimension of attractor
    pred_inds <- range(which(temp$unq == r_combos[rr, "lib_loc"]))
    
    lib_E <- simplex(temp[, focus_col], E = E_vals, silent = T, 
                     stats_only = F, lib = c(1, nrow(temp)),
                     pred = pred_inds) %>% filter(num_pred != 0, 
                   mae == min(mae, na.rm = T)) %>% pull(E)  
    lib_theta <- s_map(temp[, focus_col], E = lib_E,
      silent = T, stats_only = F, lib = c(1, nrow(temp)),
      pred = pred_inds, theta = seq(0, 10, .5)) %>% 
      filter(num_pred != 0, mae == min(mae, na.rm = T)) %>% pull(theta)

    #------------------------------------
    #Simplex out of sample predictions

    simp_preds <- rep(NA, npreds)

    #Make sure that the last unq is the 
    temp_focus <- temp %>% filter(unq == r_combos[rr, "lib_loc"])
    temp <- temp %>% filter(unq != r_combos[rr, "lib_loc"])
    temp <- rbind(temp, temp_focus)
    
    simp_preds <- rep(NA, npreds)
    lib1 <- temp[, focus_col]

    for(ii in 1:npreds){
      l1 <- c(1, length(lib1) + (ii - 1))
      p1 <- c(length(lib1) - lib_E + (ii - 1), 
        length(lib1) + 1 + (ii - 1))
      pred1 <- simplex(c(lib1, simp_preds), E = lib_E, silent = T,
        stats_only = F, lib = l1, pred = p1)$model_output[[1]]
      simp_preds[ii] <- pred1$pred[lib_E + 1]
    }

    #------------------------------------
    #smap out of sample predictions
    smap_preds <- rep(NA, npreds)
  
    for(ii in 1:npreds){
      l1 <- c(1, length(lib1) + (ii - 1))
      p1 <- c(length(lib1) - lib_E + (ii - 1), length(lib1) + 1 + (ii - 1))  
      
      pred1 <- s_map(c(lib1, smap_preds), E = lib_E,
                     theta = lib_theta, silent = T,
                     stats_only = F, lib = l1,
                     pred = p1)$model_output[[1]]
  
      smap_preds[ii] <- pred1$pred[lib_E + 1]     
    }  

    #------------------------------------
    #Format the output
    temp_out <- dat_in %>% filter(unq == r_combos[rr, "lib_loc"])
    temp_out$obs1 <- c(NA, temp_out$ret_std[-length(temp_out$ret_std)])
    
    max_year_ind <- which(temp_out$retYr == r_combos[rr, 'max_year'])
    temp_out <- temp_out %>% 
      filter(retYr %in% temp_out[(max_year_ind + 1):(max_year_ind + 5), "retYr"])
    temp_out$simp_pred <- simp_preds
    temp_out$smap_pred <- smap_preds
    temp_out$max_year <- r_combos[rr, 'max_year']
  
    return(temp_out)
  }
  stopCluster(cl)
  run_time <- Sys.time() - start_time; print(run_time)
  preds <- plyr::ldply(preds)
  return(preds)
}


block_smap_pred <- function(predyr, input, pred_col, bestE = NA,
  besttheta = NA){
# browser()
  #Specify library indices
  lib_inds <- range(which(input$retYr < predyr))

  #Identify attractor characteristics
  if(is.na(bestE)){
    bestE <- simplex(input[, pred_col], E = 1:10, lib = lib_inds,
      pred = lib_inds, silent = TRUE) %>% filter(mae == min(mae)) %>% pull(E) 
  }

  if(is.na(besttheta)){
    besttheta <- s_map(input[, pred_col], E = bestE, 
      theta = seq(0, 10, .5), lib = lib_inds,
      pred = lib_inds, silent = TRUE) %>% filter(mae == min(mae)) %>% pull(theta) 
  }

  #Construct block with lagged prediction column and other age classes
if(bestE == 0) return('stop')

  block <- make_block(input[, pred_col], max_lag = bestE)
  temp <- cbind(input[, -which(names(input) == pred_col)], 
    block %>% select(-time))
  
  #Identify library columns; remove time from
  lib_cols <- which(names(temp) %in% c("retYr") == FALSE)


  #Use block lnlp to make smap prediction
  res_simplex <- block_lnlp(temp, lib = lib_inds, method = 'simplex',
    pred = c(max(lib_inds[2]) - bestE, nrow(input)),
    first_column_time = TRUE, silent = TRUE, stats_only = FALSE,
    target_column = "col1", columns = lib_cols)

  res_smap <- block_lnlp(temp, lib = lib_inds, method = 's-map',
    theta = besttheta, pred = c(max(lib_inds[2]) - bestE, nrow(input)),
    first_column_time = TRUE, silent = TRUE, stats_only = FALSE,
    target_column = "col1", columns = lib_cols)

  #Add in identifiers
  res_simplex$theta <- 999
  res_simplex <- res_simplex[, names(res_smap)]
  res <- rbind(res_simplex, res_smap)

  #Pull out prediction
  res$E <- bestE
  res$pred_col <- pred_col
  res$predyr <- predyr
  res$type <- c("simplex", 'smap')
  # res$lib_cols <- paste0(names(input), collapse = ';')
  return(res)
}

# do_onepred <- function(yr_vec = 2000:2018, p1, input = ones){

#   browser()

#   ones_res <- lapply(yr_vec, FUN = function(xx){
#     tt <- block_smap_pred(input, predyr = xx, pred_col = p1)
#     return(tt)
#   })
#   ones_res <- ldply(ones_res)
#   rr <- summarize_predictions(input = ones_res)
#   ones_res$rho <- rr$rho
#   ones_res$p_val <- rr$p_val
#   ones_res$mase <- rr$mase
#   return(ones_res)
# }

summarize_predictions <- function(input){
  #Pearson correlation, and p-value from Fisher z test
  # https://stats.stackexchange.com/questions/61026/can-p-values-for-pearsons-correlation-test-be-computed-just-from-correlation-co
  n <- nrow(input)
  r <- cor(input$obs, input$pred, method = 'pearson')
  z <- 0.5 * log((1+r)/(1-r))
  zse <- 1/sqrt(n-3)
  pval <- min(pnorm(z, sd=zse), pnorm(z, lower.tail=F, sd=zse)) 

  #Calculate mase values
  input$obs1 <- c(NA, input$obs[-n])
  mase_val <- my_mase(input$obs, input$pred, input$obs1)
  outs <- data.frame(rho = r, p_val = pval, mase = mase_val)
  return(outs)
}

#Input a combination of values
loop_preds <- function(year_vec = 2000:2019, systems = unique(dat$System),
  ages = c("1_2", "1_3", "2_2", "2_3")){
# browser()

  cl <- makeCluster(length(ages))
  registerDoParallel(cl)
  res <- foreach::foreach(ii = 1:length(ages),
    .packages = c("plyr", "tidyverse", "rEDM", "reshape2"), 
    .export = c('dat', "do_onepred", "block_smap_pred",
      "summarize_predictions", "my_mase",
      "ae")) %dopar% {

    temp1 <- dat %>% filter(Age == ages[ii]) %>% 
      dcast(retYr ~ System, value.var = 'ret_std')

    par_res <- lapply(1:length(systems), FUN = function(zz){
      do_onepred(yr_vec = year_vec, input = temp1, p1 = systems[zz])  
    })

    par_res <- plyr::ldply(par_res)
    par_res$age <- ages[ii]
    return(par_res)
    # res[[ii]] <- par_res
  }
  stopCluster(cl)  
 
  res <- plyr::ldply(res)
  return(res)
}

predict_from_group <- function(groups, pred_col1, yr_vec = 2000:2019, ...){
# browser()
  # temp <- dat %>% filter(Age %in% groups$Age,
   #  System %in% groups$System)
  temp <- dat %>% filter(unq %in% groups)
  temp <- temp %>% dcast(retYr ~ unq, value.var = 'ret_std')
  #Predict a particular column
  preds_by_year <- lapply(yr_vec, FUN = function(pp){
    res <- block_smap_pred(input = temp, predyr = pp, pred_col = pred_col1, ...)

    return(res)
    # res <- cbind(res, summ_stats) 
  })
  
  preds_by_year <- plyr::ldply(preds_by_year) 
}

#Specify groups and predcol and process results
multiple_pred_group <- function(in_group, in_col, ...){
  rr <- lapply(in_col, FUN = function(ii){
    temp <- predict_from_group(groups = in_group, pred_col1 = ii, ...)  
    print(ii)
    return(temp)
  })
  if(length(rr) > 1){rr <- plyr::ldply(rr)}

  future5 <- lapply(1:nrow(rr), FUN = function(yy){
      tt <- rr[yy, ]
      short_preds <- tt$model_output[[1]] %>% filter(time >= tt$predyr, 
      time < (tt$predyr + 5))
      out <- summarize_predictions(short_preds)
      return(out)
  })
  future5 <- plyr::ldply(future5)
  rr$rho5 <- future5$rho
  rr$p_val5 <- future5$p_val
  rr$mase5 <- future5$mase
  return(rr)
}