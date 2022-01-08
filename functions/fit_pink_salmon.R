fit_pink_salmon <- function(cohort_only,
                       test_year,
                       data,
                       scalar = 10000,
                       weight = FALSE,
                       trees = 1000,
                       log_returns = FALSE,
                       use_salmon_pca = TRUE,
                       omit_nas = TRUE,
                       delta_returns = FALSE,
                       model_type = "rand_forest",
                       n_mtry = 3,
                       initial_prop = 0.75,
                       assess = 3,
                       factor_years = FALSE,
                       produce = "summary",
                       forecast = TRUE) {
 
  
  lag_len <- 2 # spawner strength 
  
  if (forecast){
    test_year <- test_year + 1
  }
  
  if (cohort_only){
    data <- data %>% 
      filter(even == (test_year %% 2))
    
    lag_len <- 1
  }
  


  
  salmon_data <- data
  
  if (log_returns){
    
    scalar <- 1
    
    salmon_data$ret <- log(salmon_data$ret)
  }
  
  salmon_data$ret = salmon_data$ret / scalar
  
  
  if (forecast){
    forecast_data <- salmon_data %>%
      filter(ret_yr == max(ret_yr)) %>%
      mutate(ret_yr = test_year,
             ret = -999)
    
    salmon_data <- salmon_data %>% 
      bind_rows(forecast_data)
  }
  
  
  
  
  # deal with deltas
  if (delta_returns) {
    ogret <- salmon_data$ret
    # browser()
    salmon_data <-  salmon_data %>%
      group_by(region) %>%
      mutate(ret = ret - lag(ret, 1)) %>%
      ungroup() %>%
      mutate(ret = ifelse(is.na(ret), 0, ret))
    
    # test <- salmon_data %>%
    #   pivot_longer(cols = contains("lag"), names_to = "thing", values_to = "value") %>%
    #   group_by(thing) %>%
    #   arrange(ret_yr) %>%
    #   mutate(value = scale(value)) %>%
    #   group_by(region) %>%
    #   mutate(ret = ret - lag(ret, 1)) %>%
    #   mutate(ret = ifelse(is.na(ret), 0, ret)) %>%
    #   ungroup() %>%
    #   ggplot(aes(value, ret)) +
    #   geom_point() + 
    #   facet_wrap(~thing)
    # # 
    #   
    #   pivot_wider(names_from = "thing", values_from = "value")
    
    
    
  }
  
  salmon_data <- salmon_data %>% 
    group_by(region) %>% 
    arrange(ret_yr) %>% 
    mutate(spawner_strength = lag(ret,lag_len)) %>% 
    ungroup()
    
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
  
  
  salmon_recipe <-
    recipe(ret ~ ., data = salmon_train) %>% {
      if (omit_nas) {
        step_naomit(., all_predictors())
      } else {
        step_meanimpute(., all_predictors())
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
    step_center(all_numeric(),-all_outcomes()) %>%
    step_scale(all_numeric(),-all_outcomes()) %>%
    step_nzv(-all_outcomes()) %>%
    step_corr(all_numeric(), -all_outcomes()) %>%
    step_pca(contains("lat"), num_comp = 25) %>% {
      if (use_salmon_pca) {
        step_pca(.,contains("lag"), num_comp = 5, prefix = "salmonPC")
      } else {
        .
      }
    } %>%
    step_dummy(all_nominal(), -ret_yr)
  
  # browser()
  
  prepped_salmon <-
    prep(salmon_recipe, data = salmon_train, retain = TRUE)
  
  baked_salmon <-
    bake(prepped_salmon, new_data = salmon_train)

  if (model_type == "rand_forest") {
    tune_grid <- tidyr::expand_grid(
      splitrule = c("variance","extratrees"),
      mtry = ceiling(seq(2, ((
        ncol(baked_salmon) - 2
      )), length.out = n_mtry)),
      min_n = c(2),
      id = unique(salmon_rolling_origin$id)
    ) %>%
      left_join(salmon_rolling_origin, by = "id")
    
  }
  
  if (model_type == "boost_tree") {
    tune_grid <- tidyr::expand_grid(
      mtry = ceiling(seq(3, ((
        ncol(baked_salmon) - 2
      )), length.out = n_mtry)),
      tree_depth = c(2,6, 10,20),
      learn_rate = c(0.3, 0.1),
      id = unique(salmon_rolling_origin$id)
    ) %>%
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
  
  # View(juice(prepped_salmon))
  tuning_fit <- pmap(
    tune_pars,
    tune_salmon,
    trees = trees,
    model_type = model_type,
    salmon_recipe = salmon_recipe,
    log_returns = log_returns
  )
  
  tune_grid$tuning_fit <- tuning_fit
  
  
  best_params <- tune_grid %>%
    select(-splits) %>%
    unnest(cols = tuning_fit)
  
  tune_vars <-
    colnames(best_params)[!colnames(best_params) %in% c(".pred", "observed","id")]
  
  best_params <- best_params %>%
    group_by(!!!rlang::parse_exprs(tune_vars)) %>%
    yardstick::rmse(observed, .pred)
  
  # best_params %>%
  #   ggplot(aes(mtry, .estimate, color = factor(min_n))) +
  #   geom_point()
  # 
  # best_params %>%
  #   ggplot(aes(mtry, .estimate, color = splitrule)) +
  #   geom_point() +
  #   facet_wrap(~min_n)
  # 
  # best_params %>% 
  #   ggplot(aes(mtry, .estimate, color = factor(tree_depth))) +
  #   geom_point() +
  #   facet_wrap(~learn_rate, scales = "free_y")
  # 
  # best_params %>% 
  #   ggplot(aes(num_terms, .estimate, color = factor(prod_degree))) +
  #   geom_point() 


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
    
    # importance(trained_model$fit) %>%
    #   broom::tidy() %>%
    #   View()
    # ranger::importance(trained_model$fit) %>%
    #   broom::tidy() %>%
    #   filter(x > 0) %>%
    #   ggplot(aes(reorder(names,x),x)) +
    #   geom_col() +
    #   coord_flip()
    
    
    # importance <-  ranger::importance(trained_forest$fit) %>%
    #   broom::tidy() %>%
    #   mutate(var = fct_reorder(names, x))
    #
    # important <-
    #   importance$names[(importance$x > importance$x[importance$names == "rando"]) |
    #                      (importance$x >= importance$x[importance$names == "ret_yr"]) &
    #                      importance$names != "rando"]
    #
    # salmon_train <- salmon_train %>%
    #   select(ret, important)
    
  } # close model_type == "random_forest"
  if (model_type == "boost_tree") {
    # browser()
    trained_model <-
      parsnip::boost_tree(
        mode = "regression",
        mtry = best$mtry,
        learn_rate = best$learn_rate,
        tree_depth = best$tree_depth,
        trees = trees
      ) %>%
      parsnip::set_engine("xgboost") %>%
      parsnip::fit(formula(prepped_salmon), data = juice(prepped_salmon))
    
    # importance_matrix <- xgboost::xgb.importance(colnames(juice(prepped_salmon)), model = trained_model$fit)
    # 
    # 
    # xgboost::xgb.plot.importance(importance_matrix)
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
  
  if (delta_returns) {
    salmon_data$ret <- ogret
    
  }
  
  if (omit_nas) {
    salmon_data <- na.omit(salmon_data)
  }
  
  salmon_data$pred <-
    dplyr::case_when(log_returns == TRUE ~ exp(pred$.pred), TRUE ~  pred$.pred)
  
  if (delta_returns) {
    salmon_data$pred[salmon_data$ret_yr < test_year] <-
      salmon_data$ret[salmon_data$ret_yr < test_year]
    
    for (y in test_year:max(salmon_data$ret_yr)) {
      for (s in unique(salmon_data$region)) {
        if (y == test_year) {
          salmon_data$pred[salmon_data$ret_yr == y &
                             salmon_data$region == s] <-
            pmax(0, salmon_data$ret[salmon_data$ret_yr == (y - 1) &
                                      salmon_data$region == s] + salmon_data$pred[salmon_data$ret_yr == y &
                                                                                    salmon_data$region == s])
          
        } else {
          salmon_data$pred[salmon_data$ret_yr == y &
                             salmon_data$region == s] <-
            pmax(0, salmon_data$pred[salmon_data$ret_yr == (y - 1) &
                                       salmon_data$region == s] + salmon_data$pred[salmon_data$ret_yr == y &
                                                                                     salmon_data$region == s])
          
          
        }
        
      } # close region loop
      
    } # close year loop
    
    # salmon_data %>%
    #   ggplot() +
    #   geom_point(aes(ret_yr, ret, color = region)) +
    #   geom_line(aes(ret_yr, pred, color = region))
    #
    #
    # browser()
  }
  
  salmon_data <- salmon_data %>%
    mutate(
      split = dplyr::case_when(ret_yr < test_year ~ "training",
                               ret_yr <= max(data$ret_yr) ~ "testing",
                               TRUE ~ "forecast"),
      ret = ret * scalar,
      pred = pred * scalar
    )
  salmon_data$ret[salmon_data$split == "forecast"] <-  NA
  # 
  # test %>% 
  #   group_by(split, ret_yr) %>% 
  #   count() %>% 
  #   View()
  
  # salmon_data %>%
  #   ggplot(aes(ret, pred, color = factor(ret_yr))) +
  #   geom_abline(aes(intercept = 0, slope = 1)) +
  #   geom_smooth(method = "lm", show.legend = FALSE, se = FALSE) +
  #   geom_point(show.legend = TRUE) +
  #   facet_wrap(~split, scales = "free") +
  #   scale_color_viridis_d()
  
  if (produce == "summary") {
    out <- list(salmon_data = salmon_data, best_params = best_params)
  } else {
    out <-
      list(salmon_data = salmon_data, trained_model = trained_model)
    
  }
  
  return(out)
  
  
  
}