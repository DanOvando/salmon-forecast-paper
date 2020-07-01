# assess salmon forecast performance across models

# if not using an Rstudio project, manually set working directory 
# to the respository folder before sourcing this script, e.g. 
# in command line setwd("/Users/danovan/projects/salmon-forecast-paper")
# Once you do that here will work regardless of the presence of an rstudio project 
# (it will notice the git file)

functions <- list.files(here::here("functions"))

purrr::walk(functions, ~ source(here::here("functions", .x)))

prep_run(results_name = "v0.5.1", results_description = "draft publication with boost tree improvements loo starting in 1990",
         first_year = 1990, 
         last_year = 2019,
         min_year = 1963, 
         eval_year = 2000)

options(dplyr.summarise.inform = FALSE)

run_edm_forecast <- FALSE

run_dlm_forecast <- FALSE

run_ml_forecast <- FALSE

fit_statistical_ensemble <- TRUE


scalar <- 1000

extrafont::loadfonts()

pub_theme <-
  hrbrthemes::theme_ipsum(base_size = 10, axis_text_size = 12) +
  theme(
    panel.spacing = unit(1, "lines"),
    plot.margin = unit(rep(10, 4), units = "points")
  )

theme_set(pub_theme)


# run forecasts -----------------------------------------------------------

if (run_edm_forecast){
  
  source(here("scripts","run-edm-sockeye-forecast.R"))
}

if (run_dlm_forecast){
  
  source(here("scripts","run-dlm-sockeye-forecast.R"))
  
}

if (run_ml_forecast){
  
  source(here("scripts","run-ml-sockeye-forecast.R"))
  
  
}




# process forecasts -------------------------------------------------------

published_forecasts <-
  get_published_fcst(
    dir.pf = here(file.path("data", "preseasonForecast.dat")),
    dir.ids = here(file.path("data", "ID_Systems.csv")),
    years = first_year:last_year
  ) %>%
  rename(forecast = FRIfcst) %>%
  mutate(model = "fri") %>% 
  janitor::clean_names() %>% 
  rename(year = ret_yr) %>% 
  mutate(age = fw_age + o_age + 1) %>%
  unite(age_group, fw_age, o_age, sep = "_") %>% 
  mutate(brood_year = year - age) %>% 
  as_tibble() %>% 
  select(model, brood_year, year, system, age_group, forecast)



data <- read_csv(here::here("data", paste0(last_year, ".csv"))) %>%
  janitor::clean_names() %>%
  mutate(age_group = paste(fw_age, o_age, sep = "."))


data %>% 
  group_by(ret_yr) %>% 
  summarise(ret = sum(ret)) %>% 
  ggplot(aes(ret_yr, ret )) + 
  geom_point()

top_age_groups <- data %>%
  group_by(age_group) %>%
  summarise(tr = sum(ret)) %>%
  mutate(ptr = percent_rank(tr)) %>%
  arrange(desc(tr))

top_age_groups <- top_age_groups %>%
  top_n(4, tr) %>% {
    .$age_group
  }


top_age_groups <- str_replace_all(top_age_groups, "\\.",'_')

results <- list.files(results_dir)

results <- results[str_detect(results,"_loo_results.csv")]


observed_returns <- data %>% 
  unite(col = "age_group",fw_age, o_age, sep = '_') %>% 
  select(system, age_group, ret_yr, ret) %>% 
  rename(observed = ret)

wtf <- observed_returns %>% 
  group_by(ret_yr, age_group) %>% 
  summarise(observed = sum(observed)) %>% 
  ungroup() %>% 
  rename(year = ret_yr)
  
  
published_forecasts %>% 
  group_by(year,age_group) %>% 
  summarise(forecast = sum(forecast)) %>% 
  ungroup() %>% 
  filter(age_group == "1_3") %>% 
  left_join(wtf) %>% 
  ggplot(aes(year, forecast)) + 
  geom_area(aes(year, observed)) +
  geom_point()



# adding in observed data from scratch, since there are some rounding errors cropping up in the 
# observed data in each data frame
forecasts <- map_df(results, ~read_csv(file.path(results_dir,.x))) %>% 
  rename(year = return_year,
         forecast = predicted_returns,
         observed = observed_returns) %>% 
  select(-observed) %>% 
  bind_rows(published_forecasts) %>% 
  left_join(observed_returns, by = c("system", "year" = "ret_yr", "age_group")) %>% 
  filter(age_group %in% top_age_groups,
        !system %in% c("Alagnak","Togiak","Branch")) %>% 
  mutate(model = str_remove_all(model, "_forecast")) %>% 
  mutate(model = str_remove_all(model,"_one system top6 ages"))  %>% 
  mutate(observed = observed / 1000, 
         forecast = forecast / 1000)

forecasts %>% 
  filter(year >= first_year) %>% 
  group_by(year, model) %>% 
  summarise(observed = sum(observed),
            forecast = sum(forecast)) %>% 
  ggplot() + 
  geom_col(aes(year, observed)) + 
  geom_point(aes(year, forecast)) + 
  facet_wrap(~model)



# construct statistical ensemble ------------------------------------------

# what should an emsemble look like? Depends a bit on the scale, but for now let's try it at the
# finest resolution possible

# a = (forecasts %>% filter(year == 1990, system == "Naknek"))
# 
# b <- pivot_wider(a, names_from = "model", values_from = "forecast")


# ensemble_data <- forecasts %>% 
#   mutate(observed = observed / scalar,
#          forecast = forecast / scalar) %>% 
#   group_by(model, system, age_group) %>%
#   arrange(year) %>%
#   mutate(last_observed = lag(observed, 1)) %>%
#   filter(year > first_year, 
#          model != "fri") %>% 
#   ungroup() %>% 
#   pivot_wider(names_from = "model", values_from = "forecast") %>% 
#   group_by(system) %>% 
#   mutate(sys_weight = 1 / length(observed))%>% 
#   ungroup()
# mutate(return_rank = percent_rank(observed)) %>% 
# mutate(return_type = case_when(return_rank > 0.66 ~ "boom", return_rank < 0.33 ~ "bust", TRUE ~ "meh"))

# ensemble_data[is.na(ensemble_data)] <- -999

# ensemble_data <- ensemble_data %>% 
#   arrange(year)


ensemble_dep_data <- forecasts %>% 
  mutate(observed = observed / scalar,
         forecast = forecast / scalar) %>% 
  filter(year > first_year, 
         model != "fri") %>% 
  ungroup() %>% 
  unite("model_agegroup", model, age_group, sep  = '_') %>% 
  select(system, year, model_agegroup, forecast) %>% 
  pivot_wider(names_from = "model_agegroup", values_from = "forecast") 

ensemble_data <- forecasts %>%
  filter(model == "lag") %>% 
  group_by(year, system) %>% 
  summarise(observed = sum(observed) / scalar) %>% 
  ungroup() %>% 
  left_join(ensemble_dep_data, by = c("year", "system")) %>% 
  filter(year > first_year)


# mutate(return_rank = percent_rank(observed)) %>% 
# mutate(return_type = case_when(return_rank > 0.66 ~ "boom", return_rank < 0.33 ~ "bust", TRUE ~ "meh"))

ensemble_data[is.na(ensemble_data)] <- -999

ensemble_data <- ensemble_data %>% 
  arrange(year)


if (fit_statistical_ensemble){
  
  fit_ensemble <- function(test_year, ensemble_data){
    
    training_prop <- last(which(ensemble_data$year < test_year)) / nrow(ensemble_data)
    
    ensemble_split <- initial_time_split(ensemble_data, prop = training_prop)
    
    training_ensemble_data <- training(ensemble_split)
    
    testing_ensemble_data <- testing(ensemble_split)
    
    ensemble_splits <- rsample::group_vfold_cv(training_ensemble_data, group = year)
    
    tune_grid <- parameters(min_n(range(2,10)), tree_depth(range(4,15)), learn_rate(range = c(-2,0)), mtry(),
                            loss_reduction(),sample_prop(range = c(0.5,1)), trees(range = c(500,2000)))%>% 
      dials::finalize(mtry(), x = training_ensemble_data %>% select(-(1:2)))
    
    xgboost_grid <- grid_max_entropy(tune_grid, size = 20) 
    
    xgboost_model <-
      parsnip::boost_tree(
        mode = "regression",
        mtry = tune(),
        min_n = tune(),
        loss_reduction = tune(),
        sample_size =tune(), 
        learn_rate = tune(),
        tree_depth = tune(),
        trees =tune()
      ) %>%
      parsnip::set_engine("xgboost") 
    
    # ranger_workflow <- workflows::workflow() %>% 
    #   add_formula(observed ~ .) %>% 
    #   add_model(ranger_model)
    # 
    xgboost_workflow <- workflows::workflow() %>% 
      add_formula(observed ~ .) %>% 
      add_model(xgboost_model)
    
    
    set.seed(234)
    doParallel::registerDoParallel(cores = parallel::detectCores() - 2)
    # ranger_tuning <- tune_grid(
    #   ranger_workflow,
    #   resamples = ensemble_splits,
    #   grid = ranger_grid,
    #   control = control_grid(save_pred = TRUE)
    # )
    # ranger_tuning

    xgboost_tuning <- tune_grid(
      xgboost_workflow,
      resamples = ensemble_splits,
      grid = xgboost_grid,
      control = control_grid(save_pred = TRUE)
    )
    
    # xgboost_tuning
    
    # collect_metrics(ranger_tuning) %>% 
    #   select(mean, mtry:min_n, .metric) %>% 
    #   pivot_longer(mtry:min_n, names_to = "dial", values_to = "level") %>% 
    #   ggplot(aes(level, mean)) + 
    #   geom_point() + 
    #   facet_wrap(.metric ~ dial, scales = "free")
    
    # collect_metrics(xgboost_tuning) %>%
    #   select(mean, mtry:sample_size, .metric) %>%
    #   pivot_longer(mtry:sample_size, names_to = "dial", values_to = "level") %>%
    #   ggplot(aes(level, mean)) +
    #   geom_point() +
    #   facet_wrap(.metric ~ dial, scales = "free")
    # 
    # show_best(xgboost_tuning, "rmse")
    # 
    
    best_rmse <- tune::select_best(xgboost_tuning, metric = "rmse")
    
    # final_ranger_model <- finalize_workflow(
    #   ranger_workflow,
    #   best_rmse
    # )
    
    final_workflow <- finalize_workflow(
      xgboost_workflow,
      best_rmse
    )
    
    trained_ensemble <-
      parsnip::boost_tree(
        mode = "regression",
        mtry = best_rmse$mtry,
        min_n = best_rmse$min_n,
        loss_reduction = best_rmse$loss_reduction,
        sample_size = best_rmse$sample_size, 
        learn_rate = best_rmse$learn_rate,
        tree_depth = best_rmse$tree_depth,
        trees = best_rmse$trees
      ) %>%
      parsnip::set_engine("xgboost") %>%
      parsnip::fit(observed ~ ., data = training_ensemble_data)
    

    # trained_ensemble %>%
    #   vip::vi() %>% 
    #   vip::vip(geom = "point")

    # ensemble_fits <- last_fit(
    #   final_workflow,
    #   ensemble_split
    # )

    
    # ranger_ensemble_model <- ranger(observed ~ ., data = training_ensemble_data %>% select(-last_observed),
    #                                 importance = "impurity_corrected",
    #                                 mtry = best_rmse$mtry,
    #                                 min.node.size = best_rmse$min_n,
    #                                 num.trees = best_rmse$trees,
    #                                 case.weights = training_ensemble_data$sys_weight)
    
    # ranger_ensemble_model <- ranger(observed ~ ., data = training_ensemble_data %>% select(-last_observed),
    #                                 importance = "impurity_corrected")
    
    # vip::vi(ranger_ensemble_model) %>% 
    #   vip::vip()
    
    training_ensemble_data$ensemble_forecast <- predict(trained_ensemble, new_data = training_ensemble_data)$.pred
    
    testing_ensemble_data$ensemble_forecast <- predict(trained_ensemble, new_data = testing_ensemble_data)$.pred
    
    ensemble_forecasts <- training_ensemble_data %>% 
      bind_rows(testing_ensemble_data) %>% 
      mutate(set = ifelse(year >= test_year, "testing", "training"))
    
    # ensemble_forecasts %>% 
    #   ggplot(aes(observed, ensemble_forecast, color = set)) + 
    #   geom_point()
    
    return(ensemble_forecasts)
    
  }
  
  ensemble_fits <- tibble(test_year = eval_year:last_year) %>% 
    mutate(ensemble = map(test_year, fit_ensemble, ensemble_data = ensemble_data))
  
  write_rds(ensemble_fits, path = file.path(results_dir, "ensemble_fits.rds"))
  
  
} else {
  
  ensemble_fits <- read_rds(file.path(results_dir, "ensemble_fits.rds"))
  
}

temp <- ensemble_fits %>% 
  unnest(cols = ensemble)


forecasts <- forecasts %>% 
  filter(year >= eval_year)

ensemble_forecasts <- temp %>% 
  filter(year == test_year) %>% 
  mutate(model = "ensemble") %>% 
  mutate(observed = observed * scalar,
         ensemble_forecast = ensemble_forecast * scalar) 

ensemble_forecasts %>% 
  ggplot(aes(observed, ensemble_forecast)) + 
  geom_point() + 
  geom_abline(aes(slope = 1, intercept = 0)) +
  facet_wrap(~set)

total_ensemble_plot <- ensemble_forecasts %>% 
  group_by(year, set) %>% 
  summarise(observed = sum(observed),
            ensemble_forecast = sum(ensemble_forecast)) %>% 
  ungroup() %>% 
  ggplot(aes(fill = set)) +
  geom_col(aes(year, observed)) + 
  geom_point(aes(year, ensemble_forecast), shape = 21)


system_ensemble_plot <- ensemble_forecasts %>% 
  group_by(year, system, set) %>% 
  summarise(observed = sum(observed),
            ensemble_forecast = sum(ensemble_forecast)) %>% 
  # group_by(system) %>% 
  # mutate(observed = scale(observed),
  # ensemble_forecast = scale(ensemble_forecast)) %>% 
  ungroup() %>% 
  ggplot(aes(fill = set)) +
  geom_col(aes(year, observed)) + 
  geom_point(aes(year, ensemble_forecast),shape = 21) + 
  facet_wrap(~system, scales = "free_y")

system_ensemble_forecasts <- ensemble_forecasts %>% 
  rename(forecast = ensemble_forecast) %>% 
  select(year,system, model,observed, forecast) 
  

total_ensemble_forecasts <- ensemble_forecasts %>% 
  group_by(year, model) %>% 
  summarise(observed = sum(observed), forecast = sum(ensemble_forecast)) %>% 
  select(year, model,observed, forecast) %>% 
  ungroup()

# forecasts <- forecasts %>% 
#   bind_rows(ensemble_forecasts)


# create performance summaries

total_forecast <- forecasts %>% 
  filter(year >= first_year) %>% 
  group_by(year, model) %>% 
  summarise(observed = sum(observed),
            forecast = sum(forecast)) %>% 
  bind_rows(total_ensemble_forecasts) %>% 
  ungroup()

age_forecast <- forecasts %>% 
  filter(year >= first_year) %>% 
  group_by(year,age_group,model) %>% 
  summarise(observed = sum(observed),
            forecast = sum(forecast)) %>% 
  ungroup()

system_forecast <- forecasts %>% 
  filter(year >= first_year) %>% 
  group_by(year,system,model) %>% 
  summarise(observed = sum(observed),
            forecast = sum(forecast)) %>% 
  ungroup() %>% 
  bind_rows(system_ensemble_forecasts)


age_system_forecast <- forecasts %>% 
  filter(year >= first_year) %>% 
  group_by(year,age_group,system,model) %>% 
  summarise(observed = sum(observed),
            forecast = sum(forecast))


total_forecast_plot <- total_forecast %>% 
  ggplot() + 
  geom_point(aes(year, observed),size = 2) + 
  geom_line(aes(year, forecast, color = model), show.legend = FALSE) + 
  facet_wrap(~model)

total_forecast_plot

age_forecast_plot <- age_forecast %>% 
  ggplot() + 
  geom_point(aes(year, observed),size = 2) + 
  geom_line(aes(year, forecast, color = model), show.legend = FALSE) + 
  facet_grid(age_group~model, scales = "free_y") + 
  scale_x_continuous(guide = guide_axis(check.overlap = TRUE, n.dodge = 2))

age_forecast_plot


system_forecast_plot <- system_forecast %>% 
  ggplot() + 
  geom_point(aes(year, observed),size = 2) + 
  geom_line(aes(year, forecast, color = model), show.legend = FALSE) + 
  facet_grid(system~model, scales = "free_y") + 
  scale_x_continuous(guide = guide_axis(check.overlap = TRUE, n.dodge = 2))


system_forecast_plot

# evaluate performance

age_system_performance <- age_system_forecast %>% 
  group_by(age_group, system, model) %>% 
  arrange(year) %>% 
  summarise(rmse = yardstick::rmse_vec(truth = observed, estimate = forecast),
            r2 = yardstick::rsq_vec(truth = observed, estimate = forecast),
            ccc = yardstick::ccc_vec(truth = observed, estimate = forecast),
            mape = yardstick::mape_vec(truth = observed, estimate = forecast),
            mae = yardstick::mae_vec(truth = observed, estimate = forecast),
            mase = yardstick::mase_vec(truth = observed, estimate = forecast),
            bias = mean(forecast - observed)) %>% 
  ungroup()


total_performance <- total_forecast %>% 
  group_by(model) %>% 
  arrange(year) %>% 
  summarise(rmse = yardstick::rmse_vec(truth = observed, estimate = forecast),
            r2 = yardstick::rsq_vec(truth = observed, estimate = forecast),
            ccc = yardstick::ccc_vec(truth = observed, estimate = forecast),
            mape = yardstick::mape_vec(truth = observed, estimate = forecast),
            mae = yardstick::mae_vec(truth = observed, estimate = forecast),
            mase = yardstick::mase_vec(truth = observed, estimate = forecast),
            bias = mean(forecast - observed)) %>% 
  ungroup() %>% 
  arrange(mase)

recent <- total_forecast %>% 
  filter(year >= 2014) %>% 
  group_by(model) %>% 
  summarise(rmse = yardstick::rmse_vec(truth = observed, estimate = forecast),
            r2 = yardstick::rsq_vec(truth = observed, estimate = forecast),
            ccc = yardstick::ccc_vec(truth = observed, estimate = forecast),
            mape = yardstick::mape_vec(truth = observed, estimate = forecast),
            mae = yardstick::mae_vec(truth = observed, estimate = forecast),
            mase = yardstick::mase_vec(truth = observed, estimate = forecast),
            bias = mean(forecast - observed)) %>% 
  ungroup() %>% 
  arrange(mase)

system_performance <- system_forecast %>% 
  group_by(model, system) %>% 
  summarise(rmse = yardstick::rmse_vec(truth = observed, estimate = forecast),
            r2 = yardstick::rsq_vec(truth = observed, estimate = forecast),
            mae = yardstick::mae_vec(truth = observed, estimate = forecast),
            mase = yardstick::mase_vec(truth = observed, estimate = forecast),
            bias = mean(forecast - observed)) %>% 
  ungroup() %>% 
  arrange(mase)

age_performance <- age_forecast %>% 
  group_by(model, age_group) %>% 
  summarise(rmse = yardstick::rmse_vec(truth = observed, estimate = forecast),
            r2 = yardstick::rsq_vec(truth = observed, estimate = forecast),
            ccc = yardstick::ccc_vec(truth = observed, estimate = forecast),
            mape = yardstick::mape_vec(truth = observed, estimate = forecast),
            mae = yardstick::mae_vec(truth = observed, estimate = forecast),
            mase = yardstick::mase_vec(truth = observed, estimate = forecast),
            bias = mean(forecast - observed)) %>% 
  ungroup() %>% 
  arrange(mase)


total_performance_plot <- total_performance %>% 
  ggplot(aes(reorder(model, mase), mase)) + 
  geom_col()

total_performance_plot

age_performance_plot <- age_performance %>% 
  group_by(age_group) %>% 
  ggplot(aes(reorder(model, mase), mase)) + 
  geom_col() + 
  facet_wrap(~age_group) + 
  scale_x_discrete(guide = guide_axis(n.dodge = 2))

age_performance_plot


system_performance_plot <- system_performance %>% 
  group_by(system) %>% 
  ggplot(aes(reorder(model, mase), mase)) + 
  geom_col() + 
  facet_wrap(~system) + 
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) + 
  coord_flip()


system_performance_plot

# age_system_performance_plot <-  age_system_performance %>% 
#   group_by(age_group, system) %>% 
#   mutate(ml_improvement = (rmse[model == "lag"] - rmse[model == "boost_tree"]) /  rmse[model == "lag"],
#          ref_rmse =rmse[model == "lag"],
#          sd_rmse = sd(rmse)) %>% 
#   filter(r2 == min(r2))  %>%
#   ungroup() %>% 
#   mutate(scaled_rmse = -(ref_rmse - rmse) / ref_rmse,
#          fface = ifelse(model == "boost_tree", "italic","plain")) %>% 
#   ggplot(aes(system, age_group, label = model,color = scaled_rmse)) + 
#   geom_text(size = 7, aes(fontface = fface)) + 
#   theme_bw() + 
#   scale_color_gradient(low = "tomato", high = "steelblue", 
#                        labels = scales::percent,
#                        name = "% Reduction in RMSE",
#                        limits = c(-.75,0), 
#                        breaks = seq(-.75,0, by = 0.25))
# 
# age_system_performance_plot


# save things -------------------------------------------------------------



performance_files <- ls()[str_detect(ls(),"_performance")]

save(list = performance_files,file = file.path(results_dir,"performance_files.Rdata"))




# make plots --------------------------------------------------------------

salmon_data <- data %>% 
  rename(year = ret_yr) %>% 
  mutate(age_group = str_replace(age_group,"\\.","_"))

write_rds(salmon_data, path = file.path(results_dir,"salmon_data.rds"))

top_systems <- forecasts$system %>% unique()

salmon_data <- salmon_data %>% 
  filter(age_group %in% top_age_groups) %>% 
  mutate(ret = ret/ 1000)







## ----age-sys-return-plot------------------------------------------------------

age_sys_return_plot <- salmon_data %>% 
  ggplot(aes(year, ret, fill = age_group)) + 
  geom_col(alpha = 0.75) + 
  facet_wrap(~system, scales = "free_y") + 
  scale_fill_viridis_d(name = "Age Group") + 
  scale_y_continuous(name = "Returns (Millions of Salmon)") + 
  scale_x_continuous(name = '') +
  theme(legend.position = "top") 

age_sys_return_plot


## ----ml-pred-plot-------------------------------------------------------------

ml_pred_plot <- total_forecast %>% 
  filter(model == "boost_tree") %>% 
  ggplot() + 
  geom_col(aes(year, observed),alpha = 0.75) + 
  geom_line(aes(year, forecast),color = "tomato", linetype = 2) +
  geom_point(aes(year, forecast),fill = "tomato", size = 4, shape = 21) +
  scale_y_continuous(name = "Returns (millions)") + 
  scale_x_continuous(name = '')  + 
  labs(caption = "Red points are forecasts")

ml_pred_plot



## ----all-pred-plot------------------------------------------------------------
all_pred_plot <- total_forecast %>% 
  ggplot() + 
  geom_col(aes(year, observed),alpha = 0.75) + 
  geom_line(aes(year, forecast, color = model), linetype = 2) +
  geom_point(aes(year, forecast, fill = model), size = 4,shape = 21, alpha = 0.75) +
  scale_y_continuous(name = "Returns (millions)") + 
  scale_x_continuous(name = '')  + 
  facet_wrap(~model) +
  labs(caption = "Points are forecasts") + 
  scale_fill_ucscgb(guide = FALSE) + 
  scale_color_ucscgb(guide = FALSE) + 
  theme(axis.text = element_text(angle = 45, vjust = 0, hjust = 1, size = 10))


all_pred_plot



## ----all-age-pred-plot--------------------------------------------------------

all_age_pred_plot <- age_forecast %>%
  filter(model != "runmean") %>%
  ggplot() +
  geom_col(aes(year, observed),alpha = 0.75) +
  geom_line(aes(year, forecast, color = model), linetype = 1, size = 1) +
  geom_point(aes(year, forecast, fill = model), size = 4,shape = 21) +
  scale_y_continuous(name = "Returns (millions)") +
  scale_x_continuous(name = '')  +
  facet_grid(age_group~model, scales = "free_y") +
  scale_fill_ucscgb(guide = FALSE) +
  scale_color_ucscgb(guide = FALSE) +
  theme(axis.text = element_text(angle = 45, vjust = 0, hjust = 1, size = 10))


all_age_pred_plot


## -----------------------------------------------------------------------------

naive_ensemble <- age_system_performance %>%
  filter(model != "fri") %>% 
  group_by(age_group, system) %>%
  filter(mase == min(mase, na.rm = TRUE)) %>%
  select(age_group, system, model) %>%
  rename(best_model = model)

naive_ensemble_forecasts <- forecasts %>%
  left_join(naive_ensemble, by = c("age_group", "system")) %>%
  filter(model == best_model) 

forecasts %>% 
  filter(year > 2010) %>% 
  group_by(year,model) %>% 
  summarise(r = sum(observed)) %>% 
  ggplot(aes(year, r, color= model)) + 
  geom_line() + 
  geom_vline(aes(xintercept = 2019)) +
  facet_wrap(~model)



naive_ensemble_forecasts_plot <- naive_ensemble_forecasts %>%
  group_by(year) %>%
  summarise(observed = sum(observed),
            forecast = sum(forecast)) %>%
  ungroup() %>%
  filter(year >= 2000) %>%
  ggplot() +
  geom_col(aes(year, observed), alpha = 0.75) +
  geom_line(aes(year, forecast), color = "tomato", linetype = 2) +
  geom_point(aes(year, forecast), fill = "tomato", size = 4, shape = 21) +
  scale_y_continuous(name = "Returns (millions)") +
  scale_x_continuous(name = '')  +
  labs(caption = "Red points are naive-ensemble based forecasts")

naive_ensemble_forecasts_plot



## ----include=TRUE-------------------------------------------------------------
system_naive_ensemble_forecasts_plot <-  naive_ensemble_forecasts %>%
  group_by(year, system) %>%
  summarise(observed = sum(observed),
            forecast = sum(forecast)) %>%
  ungroup() %>%
  filter(year >= 2000) %>%
  ggplot() +
  geom_col(aes(year, observed), alpha = 0.75) +
  geom_line(aes(year, forecast), color = "tomato", linetype = 2, size = 1) +
  geom_point(aes(year, forecast), fill = "tomato", size = 4, shape = 21) +
  facet_wrap(~system, scale = "free_y") +
  scale_y_continuous(name = "Returns (millions)") +
  scale_x_continuous(name = '')  +
  labs(caption = "Red points are naive-ensemble based forecasts")

system_naive_ensemble_forecasts_plot



## -----------------------------------------------------------------------------


total_performance %>%
  arrange(mase) %>% 
  gt() %>%
  fmt_number(columns = which(colnames(.) != "model"),
             decimals = 1)



## -----------------------------------------------------------------------------


age_performance %>%
  # filter(model %in% c("fri","ml")) %>% 
  group_by(age_group) %>% 
  arrange(mase) %>% 
  gt() %>%
  fmt_number(columns = which(map_lgl(., is.numeric)),
             decimals = 2)



## -----------------------------------------------------------------------------


system_performance %>%
  # filter(model %in% c("fri","ml")) %>% 
  group_by(system) %>% 
  arrange(mase) %>% 
  gt() %>%
  fmt_number(columns = which(map_lgl(., is.numeric)),
             decimals = 2)



# figure 1 ----------------------------------------------------------------



alaska <- rnaturalearth::ne_states( country = "united states of america", returnclass = "sf") %>% 
  filter(name == "Alaska") %>% 
  sf::st_transform(crs = 3467)

alaska_bbox <- sf::st_bbox(alaska)

bristol_bay_plot <- ggplot() + 
  geom_sf(data = alaska) + 
  coord_sf(xlim = alaska_bbox[c("xmin", "xmax")],
           ylim = alaska_bbox[c("ymin", "ymax")]) +
  ggthemes::theme_map() 

total_return_plot <- salmon_data %>% 
  group_by(year) %>% 
  summarise(ret = sum(ret)) %>% 
  ggplot(aes(year, ret)) + 
  geom_area(alpha = 1) + 
  scale_y_continuous(name = "Returns (millions)", expand = expansion()) + 
  scale_x_continuous(name = '') + 
  theme(axis.text.x = element_blank(),
        plot.margin = unit(c(0,0,0,0), units = "lines")) + 
  labs(subtitle = "A")

total_return_plot



system_return_plot <- salmon_data %>% 
  filter(system %in% top_systems) %>% 
  group_by(year, system) %>% 
  summarise(ret = sum(ret)) %>% 
  ggplot(aes(year, ret, fill = reorder(system,ret))) + 
  geom_area(alpha = 1) + 
  scale_y_continuous(name = "", expand = expansion()) + 
  scale_x_continuous(name = '') +
  scale_fill_viridis_d(name = 'System') + 
  theme(axis.text.x = element_blank(),
        plot.margin = unit(c(0,0,0,0), units = "lines")) + 
  labs(subtitle = "B")


system_return_plot

age_return_plot <- salmon_data %>% 
  filter(system %in% top_systems) %>% 
  group_by(year, age_group) %>% 
  summarise(ret = sum(ret)) %>% 
  ggplot(aes(year, ret, fill = (age_group))) + 
  geom_area(alpha = 1) + 
  scale_y_continuous(name = "", expand = expansion()) + 
  scale_x_continuous(name = 'Year') +
  scale_fill_viridis_d(option = "plasma", name = 'Age Group') + 
  labs(subtitle = "C") +
  theme(plot.margin = unit(c(0,0,0,0), units = "lines"))


age_return_plot

# 
# dd <- data.frame(x=LETTERS[1:3], y=1:3)
# pie <- ggplot(dd, aes(x=1, y, fill=x)) + geom_bar(stat="identity", width=1) + coord_polar(theta="y") +
#   theme_void() + theme(legend.position="none") + theme_transparent()

# df <- tibble(x = sample(2:9),
#              y = sample(2:9),
#              width = sample(seq(0.5, 3, length.out=length(x))),
#              pie = list(pie))
# 
# 
# p <- ggplot(data=data.frame(x=c(0, 10), y=c(0, 10)), aes(x, y))+geom_blank()
# p + geom_subview(aes(x=x, y=y, subview=pie, width=width, height=width), data=df)


# df <- tibble(x = 180,
#              y = 55,
#              width = sample(seq(0.5, 3, length.out=length(x))),
#              pie = list(pie))
# 
# bristol_bay_plot + geom_subview(aes(x=x, y=y, subview=pie, width=width, height=width), data=df)

return_plot <-   (total_return_plot / system_return_plot / age_return_plot)
return_plot



# plot_area <- (alaska_bbox['xmax'] - alaska_bbox['xmin']) *  (alaska_bbox['ymax'] - alaska_bbox['ymin'])
# 
# df <- tibble(x = alaska_bbox["xmin"] * 0.9,
#              y = alaska_bbox["ymin"] * 3,
#              width = 2e6,
#              height = 2e6,
#              pie = list(return_plot))
# 
# bristol_bay_plot + theme_classic() + geom_subview(aes(x=x, y=y, subview=pie, width=width, height=height), data=df)


# figure 2 ----------------------------------------------------------------

# placeholder for summary of historic performance?



# figure 3 ----------------------------------------------------------------


# forecasts %>% 
#   group_by(year, model) %>% 
#   summarise(r = sum(observed)) %>% View()

pal <- pnw_palette("Winter",n = n_distinct(system_performance$model))

top_models <- system_performance %>% 
  group_by(system) %>% 
  # filter(model == "dlm") %>%
  filter(mase == min(mase)) %>% 
  mutate(combo = paste(system, model, sep = "_"))

next_best <- system_performance %>% 
  group_by(system) %>% 
  mutate(model_rank = rank(mase)) %>% 
  filter(model_rank < 3) %>% 
  arrange(system)  %>% 
  summarise(model = model[model_rank == 1],
            percent_improvement = abs(mase[model_rank == 1] / mase[model_rank == 2] - 1),
            mase = mase[model_rank == 1])


top_system_forecast <- system_forecast %>% 
  mutate(combo = paste(system, model, sep = "_")) %>% 
  # filter(model %in% "boost_tree") %>%
  filter(combo %in% top_models$combo) %>%
  left_join(next_best, by = c("model", "system"))

system_forecast_figure <- top_system_forecast %>% 
  ggplot() + 
  geom_area(aes(year, observed), fill = "darkgray") + 
  geom_point(aes(year, forecast, fill = model, alpha = mase), shape = 21, size = 3) +
  facet_wrap(~system, scales = "free_y") + 
  fishualize::scale_fill_fish_d(option = "Trimma_lantana") + 
  fishualize::scale_color_fish_d(option = "Trimma_lantana") + 
  scale_alpha_continuous(range = c(1,0.25), name = "MASE") +
  scale_x_continuous(name = '') + 
  scale_y_continuous(expand = expansion(c(0,.05)), name = "Returns (Millions of Salmon)")

system_forecast_figure


# figure 4 ----------------------------------------------------------------

top_models <- age_performance %>% 
  group_by(age_group) %>% 
  filter(mase == min(mase)) %>% 
  mutate(combo = paste(age_group, model, sep = "_")) %>% 
  ungroup()

top_age_forecast <- age_forecast %>% 
  mutate(combo = paste(age_group, model, sep = "_")) %>% 
  filter(combo %in% top_models$combo)

age_forecast_figure <- top_age_forecast %>% 
  ggplot() + 
  geom_area(aes(year, observed)) + 
  geom_line(aes(year, forecast, color = model)) +
  geom_point(aes(year, forecast, fill = model), shape = 21, size = 4) +
  facet_wrap(~age_group, scales = "free_y")

next_best <- age_performance %>% 
  group_by(age_group) %>% 
  mutate(model_rank = rank(mase)) %>% 
  filter(model_rank < 3) %>% 
  arrange(age_group)  %>% 
  summarise(model = model[model_rank == 1],
            percent_improvement = abs(mase[model_rank == 1] / mase[model_rank == 2] - 1),
            mase = mase[model_rank == 1])


top_age_forecast <- age_forecast %>% 
  mutate(combo = paste(age_group, model, sep = "_")) %>% 
  filter(combo %in% top_models$combo) %>% 
  left_join(next_best, by = c("model", "age_group"))


age_forecast_figure <- top_age_forecast %>% 
  ggplot() + 
  geom_area(aes(year, observed), fill = "darkgray") + 
  geom_point(aes(year, forecast, fill = model, alpha = mase), shape = 21, size = 3) +
  facet_wrap(~age_group, scales = "free_y") + 
  fishualize::scale_fill_fish_d(option = "Trimma_lantana") + 
  fishualize::scale_color_fish_d(option = "Trimma_lantana") + 
  scale_alpha_continuous(range = c(1,0.25), labels = percent, name = "MASE") + 
  scale_y_continuous(expand = expansion(c(0,.05)), name = "Returns (Millions of Salmon)") + 
  scale_x_continuous(name = '')
  


age_forecast_figure

# figure n ----------------------------------------------------------------

top_models <- total_performance %>% 
  filter(mase == min(mase)) %>% 
  mutate(combo = paste(model, sep = "_")) %>% 
  ungroup()

next_best <- total_performance %>% 
  mutate(model_rank = rank(mase)) %>% 
  filter(model_rank < 3) %>% 
  summarise(model = model[model_rank == 1],
            percent_improvement = abs(mase[model_rank == 1] / mase[model_rank == 2] - 1),
            mase = mase[model_rank == 1])


top_total_forecast <- total_forecast %>% 
  mutate(combo = paste(model, sep = "_")) %>% 
  filter(combo %in% top_models$combo) %>% 
  left_join(next_best, by = c("model"))


total_forecast_figure <- top_total_forecast %>% 
  ggplot() + 
  geom_area(aes(year, observed), fill = "darkgray") + 
  geom_point(aes(year, forecast, fill = model, alpha = mase), shape = 21, size = 3) +
  fishualize::scale_fill_fish_d(option = "Trimma_lantana") + 
  fishualize::scale_color_fish_d(option = "Trimma_lantana") + 
  scale_alpha_continuous(range = c(1,0.25), name = "MASE") + 
  scale_y_continuous(expand = expansion(c(0,.05)), name = "Returns (Millions of Salmon)") + 
  scale_x_continuous(name = '') 


total_forecast_figure


## -----------------------------------------------------------------------------


run_makeup <- data %>% 
  filter(ret_yr > 2010) %>% 
  mutate_if(is.factor, as.character) %>% 
  group_by(age_group, system) %>% 
  summarise(mean_ret = mean(ret)) %>% 
  ungroup() %>% 
  mutate(p_ret = mean_ret / sum(mean_ret))

age_system_performance_figure <-  age_system_performance %>% 
  group_by(age_group, system) %>% 
  mutate(ml_improvement = (rmse[model == "lag"] - rmse[model == "boost_tree"]) /  rmse[model == "lag"],
         ref_rmse =rmse[model == "fri"],
         sd_rmse = sd(rmse)) %>% 
  filter(rmse == min(rmse))  %>%
  ungroup() %>% 
  left_join(run_makeup) %>% 
  mutate(scaled_rmse = -(ref_rmse - rmse) / ref_rmse,
         fface = ifelse(model == "ml", "italic","plain"),
         model = fct_recode(model,rmean = "runmean")) %>% 
  ggplot(aes(system, age_group, label = model,color = scaled_rmse)) + 
  geom_text(aes(fontface = fface, size = sqrt(p_ret))) + 
  scale_color_gradient(low = "tomato", high = "steelblue", 
                       labels = scales::percent,
                       name = "% Change in Error Relative to FRI",
                       limits = c(-.75,0), 
                       breaks = seq(-.75,0, by = 0.25),
                       guide = guide_colorbar(ticks.colour = "black",frame.colour = "black",barwidth = unit(15, units = "lines")))+ 
  scale_x_discrete(name = '') + 
  scale_y_discrete(name = '')+
  scale_size(range = c(4,12), guide = FALSE) +
  theme(legend.position = "top") + 
  labs(caption = "Text indicates best performing model from 2000-2019")

age_system_performance_figure


# repeat but split in two time periods

performance_break <- 2010

age_system_performance_pre <- age_system_forecast %>% 
  filter(year < performance_break) %>% 
  group_by(age_group, system, model) %>% 
  summarise(rmse = yardstick::rmse_vec(truth = observed, estimate = forecast),
            r2 = yardstick::rsq_vec(truth = observed, estimate = forecast),
            # ccc = yardstick::ccc_vec(truth = observed, estimate = forecast),
            mape = yardstick::mape_vec(truth = observed, estimate = forecast),
            bias = mean(forecast - observed)) %>% 
  ungroup() %>% 
  group_by(age_group, system) %>% 
  filter(rmse == min(rmse)) %>% 
  rename(pre_model = model) %>% 
  ungroup()



age_system_performance_post <- age_system_forecast %>% 
  filter(year >= performance_break) %>% 
  group_by(age_group, system, model) %>% 
  summarise(rmse = yardstick::rmse_vec(truth = observed, estimate = forecast),
            r2 = yardstick::rsq_vec(truth = observed, estimate = forecast),
            # ccc = yardstick::ccc_vec(truth = observed, estimate = forecast),
            mape = yardstick::mape_vec(truth = observed, estimate = forecast),
            bias = mean(forecast - observed)) %>% 
  ungroup() %>% 
  group_by(age_group, system) %>% 
  filter(rmse == min(rmse)) %>% 
  rename(post_model = model) %>% 
  ungroup()


age_system_concordance <- age_system_performance_pre %>% 
  left_join(age_system_performance_post, by = c("age_group", "system")) %>%
  select(pre_model, post_model, everything()) %>% 
  ungroup() %>% 
  summarise(concordance = mean(pre_model == post_model))


age_pre <- age_forecast %>% 
  filter(year < performance_break) %>% 
  group_by(age_group, model) %>% 
  summarise(rmse = yardstick::rmse_vec(truth = observed, estimate = forecast),
            r2 = yardstick::rsq_vec(truth = observed, estimate = forecast),
            # ccc = yardstick::ccc_vec(truth = observed, estimate = forecast),
            mape = yardstick::mape_vec(truth = observed, estimate = forecast),
            bias = mean(forecast - observed)) %>% 
  ungroup() %>% 
  group_by(age_group) %>% 
  filter(rmse == min(rmse)) %>% 
  rename(pre_model = model) %>% 
  ungroup()

age_post <- age_forecast %>% 
  filter(year >= performance_break) %>% 
  group_by(age_group, model) %>% 
  summarise(rmse = yardstick::rmse_vec(truth = observed, estimate = forecast),
            r2 = yardstick::rsq_vec(truth = observed, estimate = forecast),
            # ccc = yardstick::ccc_vec(truth = observed, estimate = forecast),
            mape = yardstick::mape_vec(truth = observed, estimate = forecast),
            bias = mean(forecast - observed)) %>% 
  ungroup() %>% 
  group_by(age_group) %>% 
  filter(rmse == min(rmse)) %>% 
  rename(post_model = model) %>% 
  ungroup()


age_concordance <- age_pre %>% 
  left_join(age_post, by = c("age_group")) %>% 
  ungroup() %>% 
  summarise(concordance = mean(pre_model == post_model))




# assess performance against lags -----------------------------------------

#Calculate mase for 
#Include lagged values
forecasts <- forecasts %>%
  group_by(model, system, age_group) %>%
  arrange(year) %>% 
  mutate(lag_observed = lag(observed)) 

#Look at total returns (across age classes) by river
system_forecast <- system_forecast %>% 
  group_by(model, system) %>% 
  arrange(year) %>% 
  mutate(lag_observed = lag(observed)) 

total_forecast <- total_forecast %>% 
  group_by(model) %>% 
  arrange(year) %>% 
  mutate(lag_observed = lag(observed)) 


age_forecast <- age_forecast %>% 
  group_by(model, age_group) %>% 
  arrange(year) %>% 
  mutate(lag_observed = lag(observed)) 


system_mase <-
  system_forecast %>% filter(year >= 2000) %>% group_by(model, system) %>%
  summarize(
    mase_val = my_mase(observed, forecast, lag_observed),
    rmse_val = yardstick::rmse_vec(observed, forecast)
  )

system_mase <-
  system_mase %>% group_by(system) %>% arrange(rmse_val) %>%
  mutate(rr = row_number())

outs <- tibble(system = unique(system_mase$system))

system_mase <- system_mase %>% 
  group_by(system) %>% 
  mutate(rmse_yy = (rmse_val - min(rmse_val)) / min(rmse_val))



system_mase %>% 
  ggplot(aes(x = rmse_val, y = mase_val)) + 
  geom_point() + 
  geom_hline(yintercept = 1, lty = 2) + 
  facet_wrap(~ system) +
  xlab("RMSE") + ylab("MASE")

system_mase$rmse_val <- round(system_mase$rmse_val, digits = 0)

system_mase$rmse_val2 <- system_mase$rmse_val

system_mase[which(system_mase$mase_val >= 1),
                'rmse_val2'] <- NA 

nsigs <- system_mase %>% group_by(model) %>% 
  mutate(nsigs = sum(mase_val < 1)) %>% distinct(model, nsigs) %>%
  arrange(nsigs)

system_mase %>%
  arrange(model) %>% dcast(model ~ system, value.var = 'rmse_val2') %>%
  left_join(nsigs, by = 'model') %>% arrange(desc(nsigs)) %>%
  select(model,nsigs, everything()) %>%
  write_csv(path = file.path(results_dir,"river_mase_rmse.csv"))


system_forecast$resids <-
  system_forecast$observed - system_forecast$forecast

system_resid_plot <- system_forecast %>% filter(year >= 2000) %>%
  ggplot() + geom_hline(yintercept = 0) +
  geom_line(aes(
    x = year,
    y = resids,
    group = model,
    colour = model
  ),
  alpha = .75) + ylab("Residuals") +
  facet_wrap( ~ system, scales = "free_y") +
  theme(legend.position = c(.75, .15))

  
  #Which years were predictable?
  
# number of models with MASE < 1 by year


yearly_struggles <- total_forecast %>%
  filter(model != "lag", !is.na(lag_observed)) %>%
  group_by(model, year) %>%
  summarize(
    mase_val = abs(observed - forecast) / abs(observed - lag_observed),
    mape_val = yardstick::mape_vec(observed, forecast)
  ) %>%
  group_by(year) %>%
  summarise(p_beat_lag = mean(mase_val < 1),
            mean_mape = mean(mape_val))


yearly_struggles_figure <- yearly_struggles %>%
  ggplot(aes(year, mean_mape / 100, fill = p_beat_lag)) +
  geom_hline(aes(yintercept = 0)) +
  geom_col() + 
  scale_y_continuous(labels = scales::label_percent(accuracy = 1), name = "Mean Absolute Percent Error",
                     expand = c(0,NA)) + 
  scale_x_continuous(name = '') +
  scale_fill_viridis_c(labels = percent, name = "% Beating Lag")


yearly_system_struggles <- system_forecast %>%
  filter(model != "lag", !is.na(lag_observed)) %>%
  group_by(model, year, system) %>%
  summarize(
    mase_val = abs(observed - forecast) / abs(observed - lag_observed),
    mape_val = yardstick::mape_vec(observed, forecast)
  ) %>%
  group_by(year, system) %>%
  summarise(p_beat_lag = mean(mase_val < 1),
            mean_mape = mean(mape_val))


yearly_system_struggles_figure <- yearly_system_struggles %>%
  ggplot(aes(year, mean_mape / 100, fill = p_beat_lag)) +
  geom_hline(aes(yintercept = 0)) +
  geom_col() + 
  scale_y_continuous(labels = scales::label_percent(accuracy = 1), name = "Mean Absolute Percent Error",
                     expand = c(0,NA)) + 
  scale_x_continuous(name = '') +
  scale_fill_viridis_c(labels = percent, name = "% Beating Lag")+ 
  facet_wrap(~system, scales = "free_y")


yearly_age_struggles <- age_forecast %>%
  filter(model != "lag", !is.na(lag_observed)) %>%
  group_by(model, year, age_group) %>%
  summarize(
    mase_val = abs(observed - forecast) / abs(observed - lag_observed),
    mape_val = yardstick::mape_vec(observed, forecast)
  ) %>%
  group_by(year, age_group) %>%
  summarise(p_beat_lag = mean(mase_val < 1),
            mean_mape = mean(mape_val))


yearly_age_struggles_figure <- yearly_age_struggles %>%
  ggplot(aes(year, mean_mape / 100, fill = p_beat_lag)) +
  geom_hline(aes(yintercept = 0)) +
  geom_col() + 
  scale_y_continuous(labels = scales::label_percent(accuracy = 1), name = "Mean Absolute Percent Error",
                     expand = c(0,NA)) + 
  scale_x_continuous(name = '') +
  scale_fill_viridis_c(labels = percent, name = "% Beating Lag")+ 
  facet_wrap(~age_group, scales = "free_y")

  year_residual_plot <- year_res %>%
    ggplot(aes(
      x = year,
      y = mase_val - 1,
      fill = model
    )) + 
    geom_col(position = "dodge") +
    geom_hline(yintercept = 0) + xlab("Return year") + ylab("MASE") +
    theme(legend.position = c(.70, .9), legend.direction = "horizontal")
  
  #Years where all predictions are wrong
  year_res %>% filter(mase_val < 1) %>% group_by(year) %>%
    summarize(nmods = length(model))
  
  
  # repeat but by age classes
  
  age_system_mase <- forecasts %>%
    group_by(model, age_group, system) %>%
    summarize(
      mase_val = my_mase(observed, forecast, lag_observed),
      rmse_val = yardstick::rmse_vec(observed, forecast)
    )
  
  age_system_mase %>% group_by(model, age_group) %>% summarize(nsigs = sum(mase_val < 1)) %>%
    group_by(model) %>% mutate(sum_nsigs = sum(nsigs)) %>%
    dcast(model + sum_nsigs ~ age_group, value.var = 'nsigs') %>%
    arrange(desc(sum_nsigs)) %>% write_csv(path = file.path(results_dir, "model_age_class.csv"))
  

  forecasts$resid <- forecasts$observed - forecasts$forecast
  
  #1_2
  resids_1_2_plot <- forecasts %>% filter(age_group == "1_2") %>% 
    ggplot(aes(x = year, y = resid, group = model,
               colour = model)) + geom_line() + 
    geom_hline(yintercept = 0, lty = 2) +
    facet_wrap(~ system, scales = 'free_y')+
    theme(legend.position = c(.75, .15)) + ggtitle("Age Class 1_2") +
    xlab("Return Year") + ylab("Residuals")
    
  
  #1_3
    resids_1_3_plot <- forecasts %>% filter(age_group == "1_3") %>% 
      ggplot(aes(x = year, y = resid, group = model,
                 colour = model)) + geom_line() + 
      geom_hline(yintercept = 0, lty = 2) +
      facet_wrap(~ system, scales = 'free_y')+
      theme(legend.position = c(.75, .15)) + ggtitle("Age Class 1_3") +
      xlab("Return Year") + ylab("Residuals")
  
  #2_2
    resids_2_2_plot <- forecasts %>% filter(age_group == "2_2") %>% 
      ggplot(aes(x = year, y = resid, group = model,
                 colour = model)) + geom_line() + 
      geom_hline(yintercept = 0, lty = 2) +
      facet_wrap(~ system, scales = 'free_y')+
      theme(legend.position = c(.75, .15)) + ggtitle("Age Class 2_2") +
      xlab("Return Year") + ylab("Residuals")
  
  #2_3
    resids_2_3_plot <- forecasts %>% filter(age_group == "2_3") %>% 
      ggplot(aes(x = year, y = resid, group = model,
                 colour = model)) + geom_line() + 
      geom_hline(yintercept = 0, lty = 2) +
      facet_wrap(~ system, scales = 'free_y')+
      theme(legend.position = c(.75, .15)) + ggtitle("Age Class 2_3") +
      xlab("Return Year") + ylab("Residuals")
  
  # retrospective bias plot
    
    
    if (file.exists(file.path(results_dir, "parsnip_loo_preds.rds"))){
      
      parsnip_loo_preds <- read_rds(file.path(results_dir, "parsnip_loo_preds.rds"))
      
      loo_preds <- parsnip_loo_preds %>%
        mutate(pred = map(pred,c("salmon_data"))) %>% 
        unnest(cols = pred)
      
      retrospective_bias_plot <- loo_preds %>%
        group_by(ret_yr, test_year, model_type, system) %>%
        summarise(ret = sum(ret) / 1000,
                  pred = sum(pred) / 1000) %>%
        ungroup() %>%
        filter(test_year %% 5 == 0, model_type == "rand_forest") %>%
        ggplot() +
        geom_col(aes(ret_yr, ret),alpha = 0.5) +
        # geom_line(aes(ret_yr, pred, color = ret_yr >= test_year),show.legend = FALSE,size = 1) +
        geom_point(aes(ret_yr, pred, fill = ret_yr >= test_year),show.legend = FALSE,size = 1, shape = 21) +
        scale_x_continuous(name = '') +
        scale_y_continuous(name = "Returns (millions)")+
        facet_grid(system~test_year, scales = "free_y") +
        theme(axis.text = element_text(angle = 45, vjust = 0, hjust = 1, size = 10))
      
    }
    


    # retrospective_bias_plot
    # 
    
    
# save things -------------------------------------------------------------



plots <- ls()[str_detect(ls(), "(_plot)|(_figure)")]
    
save(list = plots, file = file.path(results_dir, "plots.RData"))

fig_path <- file.path(results_dir,"figs")

if (!dir.exists(fig_path)){
  dir.create(fig_path)
}

plotfoo <- function(x,height = 6, width = 9 , device = "pdf",path){
  
  ggsave(
    filename = file.path(path, paste(x, device, sep = '.')),
    plot = get(x),
    height = height,
    width = width
  )
  
}

walk(plots, plotfoo, path = fig_path, device = "png")
