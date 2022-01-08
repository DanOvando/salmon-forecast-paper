fit_ml_salmon <- function(dep_age,
                          pred_system,
                          test_year,
                          data,
                          scalar = 10000,
                          freshwater_cohort = TRUE,
                          weight = FALSE,
                          trees = 1000,
                          log_returns = FALSE,
                          use_wide_cohorts = FALSE,
                          use_full_cohorts = FALSE,
                          use_spatial_enviro = TRUE,
                          use_years = TRUE,
                          omit_nas = TRUE,
                          delta_returns = FALSE,
                          model_type = "rand_forest",
                          n_mtry = 3,
                          initial_prop = 0.75,
                          assess = 3,
                          factor_years = FALSE,
                          produce = "summary",
                          forecast = TRUE,
                          p_done = "who knows how close to") {
  # message(paste0("Running ", model_type))
  
  options(dplyr.summarise.inform = FALSE)
  
  # age of the cohort being forecasted
  age <-
    sum(as.numeric(str_split(dep_age, "\\.", simplify = TRUE)))  + 1 # plus one to account for brood years
  
  
  if (forecast) { # if data are being used for forecast
    test_year <- test_year + 1
  }
  
  if (log_returns) {
    scalar <- 1
  }
  
  # deprecated for now
  if (use_spatial_enviro) {
    # data <- data %>%
    #   select(-contains("env_"))
    #
  } else {
    data <- data %>%
      select(-contains("x"))
  }
  
  data$ret = data$ret / scalar
  
  cohorts <- data %>% {
    if (freshwater_cohort == TRUE) {
      filter(., fw_age == as.numeric(str_split(dep_age, "\\.", simplify = TRUE)[1])) # only include as predictors fish that share the forecast cohorts freshwater age. I.e. if we are predicting 1.3, only include as predictors fish that also spent 1 year in freshwater. 
    } else {
      .
    }
  }
  
  # calculate the mean cumulative environmental conditions experienced by the cohort being forecasted
  env_cohorts <- cohorts %>%
    filter(fw_age == as.numeric(str_split(dep_age, "\\.", simplify = TRUE)[1])) %>%
    gather(env, value, contains("env")) %>%
    group_by(brood_yr, system, env) %>%
    arrange(ret_yr) %>%
    mutate(mean_env = cummean(value)) %>%
    select(-value) %>%
    spread(env, mean_env) %>%
    select(brood_yr, ret_yr, system, contains("env_")) %>%
    ungroup()
  
  if (forecast) {
    forecast_env <- env_cohorts %>%
      filter(brood_yr == max(data$brood_yr[data$age_group == dep_age]),
             ret_yr ==  max(data$ret_yr[data$age_group == dep_age])) %>%
      mutate(brood_yr = brood_yr + 1,
             ret_yr = ret_yr + 1)
    
    env_cohorts <- env_cohorts %>%
      bind_rows(forecast_env)
    
  }
  
  
  # trends in the cohorts. full_cohorts means the model treats each age group within a cohort as a predictor, where as use_full_cohorts == FALSE aggregates the cohort into total numbers rather than separating out by age group over time
  if (use_full_cohorts) {
    wide_cohorts <-  cohorts %>%
      group_by(ret_yr, brood_yr, system) %>%
      summarise(ret = sum(ret)) %>%
      mutate(sibling_age = ret_yr - brood_yr) %>%
      filter(sibling_age < age) %>%
      ungroup() %>%
      select(-ret_yr) %>%
      unite(age_system, sibling_age, system, sep = "_") %>%
      pivot_wider(
        names_from = age_system,
        names_prefix = "age_",
        values_from = ret,
        values_fill = list(ret = 0)
      ) %>%
      mutate(ret_yr = brood_yr + age) # this is the trick to aling historic returns of a cohort to the age group being forecast. What this does is create a data frame of the historic returns of that cohort, which can then be joined to the current returns in a given return year. So, for example, if the current age group is 5 years old, this will include as covariates the appropriate 4 year olds last year, 3 year olds two years ago, and so on and so forth
    
    long_cohorts <-  cohorts %>%
      group_by(ret_yr, brood_yr, system) %>%
      summarise(ret = sum(ret)) %>%
      mutate(sibling_age = ret_yr - brood_yr) %>%
      filter(sibling_age < age) %>%
      ungroup() %>%
      select(-ret_yr) %>%
      pivot_wider(
        names_from = sibling_age,
        values_from = ret,
        names_prefix = "age_",
        values_fill = list(ret = 0)
      ) %>%
      mutate(ret_yr = brood_yr + age)
    
  } else {
    long_cohorts <- cohorts %>%
      group_by(brood_yr, ret_yr, system) %>%
      summarise(cohort_returns = sum(ret, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(ret_yr = ret_yr + 1)
    
    wide_cohorts <- long_cohorts %>%
      tidyr::pivot_wider(names_from = "system", values_from = "cohort_returns") %>%
      ungroup()
  }
  
  
  # calculate metrics around the age group being forecast (lag returns and rolling means). Deprecatede
  age_groups <- data %>%
    group_by(age_group, ret_yr, system) %>%
    summarise(age_group_returns = sum(ret)) %>%
    ungroup() %>%
    filter(age_group == dep_age) %>%
    group_by(system) %>%
    nest() %>%
    ungroup()
  
  # cyclefoo <- function(x) {
  #
  #   decadal_cycles <- as.numeric(decompose(ts(x$age_group_returns, frequency = 10))$seasonal)
  #
  # }
  age_groups <- age_groups %>%
    # mutate(decadal_cycle = map(data,cyclefoo)) %>%
    unnest(cols = data) %>%
    group_by(age_group, system) %>%
    mutate(
      lag_returns = lag(age_group_returns, 1),
      rolling_mean = RcppRoll::roll_mean(
        # age_group_returns - decadal_cycle,
        age_group_returns,
        n = 4,
        align = 'right',
        fill = NA
      )
    ) %>%
    # mutate(noise = age_group_returns - decadal_cycle - rolling_mean) %>%
    mutate(ret_yr = ret_yr + 1) %>%  # lagging the returns to join to the dependent data. THIS IS THE CRITICAL STEP THAT 
    select(-age_group_returns) %>%
    ungroup()
  
  
  forecast_data <- data %>%
    filter(age_group == dep_age,
           ret_yr == max(ret_yr)) %>%
    mutate(ret_yr = ret_yr + 1,
           brood_yr = brood_yr + 1,
           ret = -999)
  
  salmon_data <- data %>% {
    if (forecast) {
      bind_rows(., forecast_data)
    } else{
      .
    }
  } %>%
    select(-contains("env_")) %>%
    filter(age_group == dep_age) %>% {
      if (omit_nas) {
        select(.,
               age_group,
               system,
               ret_yr,
               brood_yr,
               ret,
               contains("x"))
      } else{
        select(.,
               age_group,
               system,
               ret_yr,
               brood_yr,
               ret,
               contains("rugg_"),
               contains("x"))
      }
    } %>%
    {
      if (use_wide_cohorts) {
        left_join(., wide_cohorts, by = c("brood_yr", "ret_yr"))
      } else {
        left_join(., long_cohorts, by = c("brood_yr", "ret_yr", "system"))
      }
    } %>%
    left_join(env_cohorts, by = c("brood_yr", "ret_yr", "system")) %>%
    select(-age_group, -brood_yr) 
  
  salmon_data <-  salmon_data %>%
    group_by(system) %>%
    arrange(ret_yr) %>%
    mutate(spawner_strength = lag(ret, age)) %>%
    ungroup()
  
  # deal with deltas
  if (delta_returns) {
    ogret <- salmon_data$ret
    
    salmon_data <-  salmon_data %>%
      group_by(system) %>%
      mutate(ret = ret - lag(ret, 1)) %>%
      ungroup() %>%
      mutate(ret = ifelse(is.na(ret), 0, ret))
    
    
    
  }
  
  
  
  if (pred_system != "all"){
    salmon_data <- salmon_data %>% 
      filter(system == pred_system)  %>% 
      select(-system)
  }
  
  salmon_train <- salmon_data %>%
    filter(ret_yr < test_year)

  if (weight) {
    train_weights <-
      sqrt(salmon_train$ret) / sum(sqrt(salmon_train$ret))
    
  } else {
    train_weights <- rep(1, nrow(salmon_train)) / nrow(salmon_train)
    
  }
  
  # create cross validation splits
  
  salmon_train$year <-
    lubridate::ymd(paste(salmon_train$ret_yr, "07", "03"))
  
  annual_returns <-
    salmon_train %>% group_by(year) %>% nest() %>% ungroup()

  salmon_rolling_origin <- rsample::rolling_origin(
    annual_returns,
    initial =  round(nrow(annual_returns) * initial_prop),
    assess = assess,
    cumulative = TRUE
  )
  
  salmon_train <- salmon_train %>%
    select(-year)
  # tune model
  
  if (use_years == FALSE) {
    salmon_train <- salmon_train %>%
      select(-ret_yr)
  }
  
  # tune_grid <- tune_grid %>%
  #   slice(1)
  
  salmon_recipe <-
    recipe(ret ~ ., data = salmon_train) %>% {
      if (log_returns == TRUE) {
        step_log(., all_outcomes())
      } else {
        .
      }
    } %>% {
      if (!omit_nas) {
        # step_meanimpute(., all_predictors())
        step_knnimpute(., all_predictors(), impute_with = "ret_yr", neighbors = 4)
        
      } else{
        .
      }
    } %>%
    {
      if (factor_years) {
        step_mutate(., ret_yr = factor(
          ret_yr,
          ordered = TRUE,
          levels = unique(salmon_data$ret_yr)
        ))
      } else {
        .
      }
    } %>%
    # step_center(all_numeric(), -all_outcomes(),-contains("age_")) %>%
    # step_scale(all_numeric(), -all_outcomes(),-contains("age_")) %>%
    step_corr(all_numeric(),-all_outcomes(),-contains("age_")) %>% {
      if (use_spatial_enviro) {
        step_pca(., contains("lat"), num_comp = 5)
      } else {
        .
      }
    } %>% {
      if (use_years) {
        step_dummy(., all_nominal(),-ret_yr, one_hot = FALSE)
      } else {
        step_dummy(., all_nominal(), one_hot = FALSE)
      }
    } %>%
    step_nzv(all_predictors()) %>% 
    step_naomit(all_predictors())
  
  prepped_salmon <-
    prep(salmon_recipe, data = salmon_train, retain = TRUE)
  
  baked_salmon <-
    bake(prepped_salmon, new_data = salmon_train)
  
  if (model_type == "rand_forest") {
    
    tune_grid <- parameters(min_n(range(2, 10)),mtry(), trees(range(500, 2000)))  %>% 
      dials::finalize(mtry(), x = baked_salmon %>% select(-(1:2))) 
    
    ranger_grid <- grid_latin_hypercube(tune_grid, size = 10) %>% 
      mutate(grid_row = 1:nrow(.))
    
    tune_grid <- tidyr::expand_grid(grid_row = 1:nrow(ranger_grid), id = unique(salmon_rolling_origin$id)) %>% 
      left_join(ranger_grid, by = "grid_row") %>% 
      left_join(salmon_rolling_origin, by = "id")
    
  }
  
  if (model_type == "boost_tree") {
    
    tune_grid <-
      parameters(
        min_n(range(2, 10)),
        tree_depth(range(2, 15)),
        learn_rate(c(log10(.05),log10(.6))),
        mtry(),
        loss_reduction(range(-15,log10(sd(salmon_data$ret)))),
        sample_size(range(1, 1)),
        trees(range(500, 2000))
      ) %>%
      dials::finalize(mtry(), x = baked_salmon %>% select(-(1:2))) 
    
    xgboost_grid <- grid_latin_hypercube(tune_grid, size = 20) %>% 
      mutate(grid_row = 1:nrow(.)) #%>% 
      # mutate(trees = trees)
    tune_grid <- tidyr::expand_grid(grid_row = 1:nrow(xgboost_grid), id = unique(salmon_rolling_origin$id)) %>% 
      left_join(xgboost_grid, by = "grid_row") %>% 
      left_join(salmon_rolling_origin, by = "id")
    
  }
  
  if (model_type == "mars") {
    tune_grid <- tidyr::expand_grid(
      num_terms = ceiling(seq(2, ((
        ncol(baked_salmon) - 2
      )), length.out =  n_mtry)),
      prod_degree = c(1, 2, 3),
      prune_method = c("backward"),
      id = unique(salmon_rolling_origin$id)
    ) %>%
      left_join(salmon_rolling_origin, by = "id")
    
  }
  
  tune_pars <- tune_grid[, colnames(tune_grid) != "id"] %>%
    as.list()
  
  # a <- Sys.time()
  # fplan <- future::plan(future::multiprocess, workers = 2)
  
  # future::plan(future::multisession, workers = 2)
  
  # on.exit(plan(future::sequential), add = TRUE)
  
  tuning_fit <- future_pmap(
    tune_pars,
    tune_salmon,
    model_type = model_type,
    salmon_recipe = salmon_recipe,
    log_returns = log_returns,
    .progress = TRUE
  )
  
  tune_grid$tuning_fit <- tuning_fit
  
  best_params <- tune_grid %>%
    select(-splits) %>%
    unnest(cols = tuning_fit)

    tune_vars <-
    colnames(best_params)[!colnames(best_params) %in% c(".pred", "observed","id","grid_row")]
 
  best_params <- best_params %>%
    group_by(across({{tune_vars}})) %>%
    # yardstick::mae(observed, .pred) %>%
    yardstick::rmse(observed, .pred) %>%
    ungroup()

  # a = best_params %>%
  #   pivot_longer(min_n:trees, names_to = "dial", values_to = "value")
  # a %>%
  #   ggplot(aes(value, .estimate)) +
  #   geom_point() +
  #   facet_wrap(~dial, scales = "free_x")

  # browser()
  # best_params <- best_params %>%
  #   group_by(!!!rlang::parse_exprs(tune_vars)) %>%
  #   summarise(.estimate = mean(atan(abs((observed -.pred) / observed))))
  
  
  # best_params <- best_params %>%
  #   group_by(!!!rlang::parse_exprs(tune_vars)) %>%
  #   yardstick::mape(observed, .pred)
  # 
  # best_params <- best_params %>%
  #   group_by(!!!rlang::parse_exprs(tune_vars)) %>%
  #   summarise(.estimate = mean(atan(abs((observed -.pred) / observed))))
  
  # browser()
  # best_params %>%
  #   ggplot(aes(mtry, .estimate, color = factor(min_n))) +
  #   geom_point()
  # 
  # best_params %>%
  #   ggplot(aes(mtry, .estimate, color = splitrule)) +
  #   geom_point() +
  #   facet_wrap(~min_n)
  # browser()
  # best_params %>%
  #   ggplot(aes(trees, .estimate, color = (learn_rate))) +
  #   geom_point() +
  #   facet_wrap(~tree_depth)
  # 
  best <- best_params %>%
    filter(.estimate == min(.estimate)) %>%
    dplyr::slice(1)
  
  if (model_type == "rand_forest") {
    trained_model <-
      parsnip::rand_forest(
        mode = "regression",
        mtry = best$mtry,
        min_n = best$min_n,
        trees = trees
      ) %>%
      parsnip::set_engine("ranger",
                          importance = "none",
                          splitrule = best$splitrule) %>%
      parsnip::fit(formula(prepped_salmon), data = juice(prepped_salmon))
   
    
  } # close model_type == "random_forest"
  if (model_type == "boost_tree") {
    trained_model <-
      parsnip::boost_tree(
        mode = "regression",
        mtry = best$mtry,
        min_n = best$min_n,
        loss_reduction = best$loss_reduction ,
        sample_size = best$sample_size, 
        learn_rate = best$learn_rate,
        tree_depth = best$tree_depth,
        trees = best$trees
      ) %>%
      parsnip::set_engine("xgboost") %>%
      parsnip::fit(formula(prepped_salmon), data = juice(prepped_salmon))
  }
  if (model_type == "mars") {
    trained_model <-
      parsnip::mars(
        mode = "regression",
        num_terms = best$num_terms,
        prod_degree = best$prod_degree,
        prune_method = best$prune_method
      ) %>%
      parsnip::set_engine("earth") %>%
      parsnip::fit(formula(prepped_salmon), data = juice(prepped_salmon))
    
    
  } # close if model type is MARS
  
  # a =  bake(prepped_salmon, salmon_data)
  pred <-
    predict(trained_model, new_data = bake(prepped_salmon, salmon_data))
  
  # plot(a$ret, pred$.pred)
  
  if (delta_returns) {
    salmon_data$ret <- ogret
    
  }
  
  if (omit_nas) {
    salmon_data <- na.omit(salmon_data)
  }
  
  salmon_data$pred <-
    dplyr::case_when(log_returns == TRUE ~ exp(pred$.pred), TRUE ~  pred$.pred)
  
  if (pred_system != "all"){
    
    salmon_data$system <- pred_system
    
  }
  
  if (delta_returns) {
    salmon_data$pred[salmon_data$ret_yr < test_year] <-
      salmon_data$ret[salmon_data$ret_yr < test_year]
    
    for (y in test_year:max(salmon_data$ret_yr)) {
      for (s in unique(salmon_data$system)) {
        if (y == test_year) {
          salmon_data$pred[salmon_data$ret_yr == y &
                             salmon_data$system == s] <-
            pmax(0, salmon_data$ret[salmon_data$ret_yr == (y - 1) &
                                      salmon_data$system == s] + salmon_data$pred[salmon_data$ret_yr == y &
                                                                                    salmon_data$system == s])
          
        } else {
          salmon_data$pred[salmon_data$ret_yr == y &
                             salmon_data$system == s] <-
            pmax(0, salmon_data$pred[salmon_data$ret_yr == (y - 1) &
                                       salmon_data$system == s] + salmon_data$pred[salmon_data$ret_yr == y &
                                                                                     salmon_data$system == s])
          
          
        }
        
      } # close system loop
      
    } # close year loop
    
    # salmon_data %>%
    #   ggplot() +
    #   geom_point(aes(ret_yr, ret, color = system)) +
    #   geom_line(aes(ret_yr, pred, color = system))
    #
    #
  }
  
  salmon_data <- salmon_data %>%
    mutate(
      split = dplyr::case_when(
        ret_yr < test_year ~ "training",
        ret_yr <= max(data$ret_yr) ~ "testing",
        TRUE ~ "forecast"
      ),
      ret = ret * scalar,
      pred = pred * scalar
    )
  

  salmon_data$ret[salmon_data$split == "forecast"] <-  NA

  
  if (produce == "summary") {
    out <- list(salmon_data = salmon_data, best_params = best_params)
  } else {
    out <-
      list(salmon_data = salmon_data, trained_model = trained_model)
    
  }
  
  message(paste0(p_done," done"))
  
  return(out)
  
  
  
}