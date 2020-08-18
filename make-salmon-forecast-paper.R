# assess salmon forecast performance across models

# if not using an Rstudio project, manually set working directory 
# to the respository folder before sourcing this script, e.g. 
# in command line setwd("/Users/danovan/projects/salmon-forecast-paper")
# Once you do that here will work regardless of the presence of an rstudio project 
# (it will notice the git file)

functions <- list.files(here::here("functions"))

purrr::walk(functions, ~ source(here::here("functions", .x)))

prep_run(results_name = "v0.5.4", results_description = "draft publication with boost tree improvements loo starting in 1990 on abalone",
         first_year = 1990, 
         last_year = 2019,
         min_year = 1963, 
         eval_year = 2000)

message(
  "This analysis takes about 15 hours to run on 12 cores - get it running and go have a nice weekend"
)

options(dplyr.summarise.inform = FALSE)

run_edm_forecast <- FALSE

run_dlm_forecast <- FALSE

run_ml_forecast <- FALSE

fit_statistical_ensemble <- FALSE

run_importance <- TRUE

scalar <- 1000

extrafont::loadfonts()

pub_theme <-
  hrbrthemes::theme_ipsum(base_size = 10, axis_text_size = 8, axis_title_size = 12) +
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

ensemble_data[is.na(ensemble_data)] <- -999

ensemble_data <- ensemble_data %>% 
  arrange(year)

if (fit_statistical_ensemble){
  
  fit_ensemble <- function(test_year, ensemble_data){
    
    message(glue::glue("fitting ensemble through {test_year}"))
    
    training_prop <- last(which(ensemble_data$year < test_year)) / nrow(ensemble_data)
    
    ensemble_split <- initial_time_split(ensemble_data, prop = training_prop)
    
    training_ensemble_data <- training(ensemble_split)
    
    testing_ensemble_data <- testing(ensemble_split)
    
    ensemble_splits <- rsample::group_vfold_cv(training_ensemble_data, group = year)
    
    tune_grid <-
      parameters(
        min_n(range(1, 10)),
        tree_depth(range(2, 15)),
        learn_rate(range = c(log10(.1), log10(.6))),
        mtry(),
        loss_reduction(range(-10,-5)),
        sample_prop(range = c(1, 1)),
        trees(range = c(500, 2000))
      ) %>%
      dials::finalize(mtry(), x = training_ensemble_data %>% select(-(1:2)))
    
    xgboost_grid <- grid_latin_hypercube(tune_grid, size = 20) 

    
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
    
    doParallel::stopImplicitCluster()
    set.seed(234)
    doParallel::registerDoParallel(cores = parallel::detectCores() - 2)

    xgboost_tuning <- tune_grid(
      xgboost_workflow,
      resamples = ensemble_splits,
      grid = xgboost_grid,
      control = control_grid(save_pred = TRUE)
    )
    
    best_vals <- tune::select_best(xgboost_tuning, metric = "rmse")

    final_workflow <- finalize_workflow(
      xgboost_workflow,
      best_vals
    )
    
    trained_ensemble <-
      parsnip::boost_tree(
        mode = "regression",
        mtry = best_vals$mtry,
        min_n = best_vals$min_n,
        loss_reduction = best_vals$loss_reduction,
        sample_size = best_vals$sample_size, 
        learn_rate = best_vals$learn_rate,
        tree_depth = best_vals$tree_depth,
        trees = best_vals$trees
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
  mutate(model = "boost_tree_ensemble") %>% 
  mutate(observed = observed * scalar,
         ensemble_forecast = ensemble_forecast * scalar) 

total_ensemble_plot <- ensemble_forecasts %>% 
  group_by(year, set) %>% 
  summarise(observed = sum(observed),
            ensemble_forecast = sum(ensemble_forecast)) %>% 
  ungroup() %>% 
  ggplot(aes(fill = set)) +
  geom_col(aes(year, observed)) + 
  geom_point(aes(year, ensemble_forecast), shape = 21)


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
mase_foo <- function(observed, forecast, lag_mae){
  mase <- mean(abs(observed- forecast)) / mean(lag_mae)
  
}


age_system_performance <- age_system_forecast %>% 
  group_by(age_group, system) %>% 
  mutate(lag_mae = mean(abs(observed[model == "lag"] - forecast[model == "lag"]))) %>% 
  group_by(age_group, system, model) %>% 
  arrange(year) %>% 
  summarise(rmse = yardstick::rmse_vec(truth = observed, estimate = forecast),
            r2 = yardstick::rsq_vec(truth = observed, estimate = forecast),
            ccc = yardstick::ccc_vec(truth = observed, estimate = forecast),
            mape = yardstick::mape_vec(truth = observed, estimate = forecast),
            mae = yardstick::mae_vec(truth = observed, estimate = forecast),
            mase = mase_foo(observed = observed, forecast = forecast, lag_mae = lag_mae),
            bias = mean(forecast - observed)) %>% 
  ungroup() %>% 
  group_by(age_group, system) %>% 
  mutate(srmse = rmse / rmse[model == "lag"]) %>% 
  ungroup()


total_performance <- total_forecast %>% 
  ungroup() %>% 
  mutate(lag_mae = mean(abs(observed[model == "lag"] - forecast[model == "lag"]))) %>% 
  group_by(model) %>% 
  arrange(year) %>% 
  summarise(rmse = yardstick::rmse_vec(truth = observed, estimate = forecast),
            r2 = yardstick::rsq_vec(truth = observed, estimate = forecast),
            ccc = yardstick::ccc_vec(truth = observed, estimate = forecast),
            mape = yardstick::mape_vec(truth = observed, estimate = forecast),
            mae = yardstick::mae_vec(truth = observed, estimate = forecast),
            mase = mase_foo(observed = observed, forecast = forecast, lag_mae = lag_mae),
            bias = mean(forecast - observed)) %>% 
  ungroup() %>% 
  arrange(mase) %>% 
  mutate(srmse = rmse / rmse[model == "lag"]) %>% 
  ungroup()

system_performance <- system_forecast %>% 
  group_by(system) %>% 
  mutate(lag_mae = mean(abs(observed[model == "lag"] - forecast[model == "lag"]))) %>% 
  group_by(model, system) %>% 
  summarise(rmse = yardstick::rmse_vec(truth = observed, estimate = forecast),
            r2 = yardstick::rsq_vec(truth = observed, estimate = forecast),
            mae = yardstick::mae_vec(truth = observed, estimate = forecast),
            mase = mase_foo(observed = observed, forecast = forecast, lag_mae = lag_mae),
            bias = mean(forecast - observed)) %>% 
  ungroup() %>% 
  arrange(mase) %>% 
  group_by(system) %>% 
  mutate(srmse = rmse / rmse[model == "lag"]) %>% 
  ungroup()


age_performance <- age_forecast %>% 
  group_by(age_group) %>% 
  mutate(lag_mae = mean(abs(observed[model == "lag"] - forecast[model == "lag"]))) %>% 
  group_by(model, age_group) %>% 
  summarise(rmse = yardstick::rmse_vec(truth = observed, estimate = forecast),
            r2 = yardstick::rsq_vec(truth = observed, estimate = forecast),
            ccc = yardstick::ccc_vec(truth = observed, estimate = forecast),
            mape = yardstick::mape_vec(truth = observed, estimate = forecast),
            mae = yardstick::mae_vec(truth = observed, estimate = forecast),
            mase = mase_foo(observed = observed, forecast = forecast, lag_mae = lag_mae),
            bias = mean(forecast - observed)) %>% 
  ungroup() %>% 
  arrange(mase) %>% 
  group_by(age_group) %>% 
  mutate(srmse = rmse / rmse[model == "lag"]) %>% 
  ungroup()


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



salmon_data <- data %>% 
  rename(year = ret_yr) %>% 
  mutate(age_group = str_replace(age_group,"\\.","_"))

write_rds(salmon_data, path = file.path(results_dir,"salmon_data.rds"))

top_systems <- forecasts$system %>% unique()

salmon_data <- salmon_data %>% 
  filter(age_group %in% top_age_groups) %>% 
  mutate(ret = ret/ 1000)




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
  filter(srmse == min(srmse, na.rm = TRUE)) %>%
  select(age_group, system, model) %>%
  rename(best_model = model)

naive_ensemble_forecasts <- forecasts %>%
  left_join(naive_ensemble, by = c("age_group", "system")) %>%
  filter(model == best_model) 


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

return_plot <-   (total_return_plot / system_return_plot / age_return_plot)
return_plot


bb_img <- here("imgs", "bbay.png") %>%
  image_read() 

bb_plot <- ggdraw() + 
  draw_image(bb_img)


return_plot <- bb_plot + return_plot

age_system_return_plot <- salmon_data %>% 
  filter(system %in% top_systems) %>% 
  group_by(year, age_group, system) %>% 
  summarise(ret = sum(ret)) %>% 
  ggplot(aes(year, ret, fill = (age_group))) + 
  geom_area(alpha = 1) + 
  scale_y_continuous(name = "", expand = expansion()) + 
  scale_x_continuous(name = 'Year') +
  scale_fill_viridis_d(option = "plasma", name = 'Age Group') + 
  labs(subtitle = "C") +
  theme(plot.margin = unit(c(0,0,0,0), units = "lines")) + 
  facet_wrap(~system, scales = "free_y")


age_system_return_plot 


# figure 3 ----------------------------------------------------------------

pal <- pnw_palette("Winter",n = n_distinct(system_performance$model))

top_models <- system_performance %>% 
  group_by(system) %>% 
  filter(!model %in% c('boost_tree_ensemble','fri')) %>%
  filter(srmse == min(srmse)) %>% 
  mutate(combo = paste(system, model, sep = "_"))

next_best <- system_performance %>% 
  filter(!model %in% c('boost_tree_ensemble','fri')) %>%
  group_by(system) %>% 
  mutate(model_rank = rank(srmse)) %>% 
  filter(model_rank < 3) %>% 
  arrange(system)  %>% 
  summarise(model = model[model_rank == 1],
            percent_improvement = abs(srmse[model_rank == 1] / srmse[model_rank == 2] - 1),
            srmse = srmse[model_rank == 1])


top_system_forecast <- system_forecast %>% 
  mutate(combo = paste(system, model, sep = "_")) %>% 
  # filter(model %in% "boost_tree") %>%
  filter(combo %in% top_models$combo) %>%
  left_join(next_best, by = c("model", "system"))

system_forecast_figure <- top_system_forecast %>% 
  ggplot() + 
  geom_area(aes(year, observed), fill = "darkgray") + 
  geom_point(aes(year, forecast, fill = model, alpha = srmse), shape = 21, size = 2) +
  facet_wrap(~system, scales = "free_y") + 
  fishualize::scale_fill_fish_d(name = '',option = "Trimma_lantana") + 
  fishualize::scale_color_fish_d(name = '',option = "Trimma_lantana") + 
  scale_alpha_continuous(range = c(1,0.25), name = "SRMSE") +
  scale_x_continuous(name = '') + 
  scale_y_continuous(expand = expansion(c(0,.05)), name = "Returns (Millions of Salmon)")

system_forecast_figure


# figure 4 ----------------------------------------------------------------

top_models <- age_performance %>% 
  filter(!model %in% c('boost_tree_ensemble','fri')) %>%
  group_by(age_group) %>% 
  filter(srmse == min(srmse)) %>% 
  mutate(combo = paste(age_group, model, sep = "_")) %>% 
  ungroup()

top_age_forecast <- age_forecast %>% 
  mutate(combo = paste(age_group, model, sep = "_")) %>% 
  filter(combo %in% top_models$combo)

next_best <- age_performance %>% 
  filter(!model %in% c('boost_tree_ensemble','fri')) %>%
  group_by(age_group) %>% 
  mutate(model_rank = rank(srmse)) %>% 
  filter(model_rank < 3) %>% 
  arrange(age_group)  %>% 
  summarise(model = model[model_rank == 1],
            percent_improvement = abs(srmse[model_rank == 1] / srmse[model_rank == 2] - 1),
            srmse = srmse[model_rank == 1])


top_age_forecast <- age_forecast %>% 
  mutate(combo = paste(age_group, model, sep = "_")) %>% 
  filter(combo %in% top_models$combo) %>% 
  left_join(next_best, by = c("model", "age_group"))


age_forecast_figure <- top_age_forecast %>% 
  ggplot() + 
  geom_area(aes(year, observed), fill = "darkgray") + 
  geom_point(aes(year, forecast, fill = model, alpha = srmse), shape = 21, size = 2) +
  facet_wrap(~age_group, scales = "free_y") + 
  fishualize::scale_fill_fish_d(name = '', option = "Trimma_lantana") + 
  fishualize::scale_color_fish_d(name = '',option = "Trimma_lantana") + 
  scale_alpha_continuous(range = c(1,0.25), labels = percent, name = "SRMSE") + 
  scale_y_continuous(expand = expansion(c(0,.05)), name = "Returns (Millions of Salmon)") + 
  scale_x_continuous(name = '')
  


age_forecast_figure

# figure n ----------------------------------------------------------------

top_models <- total_performance %>% 
  filter(!model %in% c('boost_tree_ensemble','fri')) %>%
  filter(srmse == min(srmse)) %>% 
  mutate(combo = paste(model, sep = "_")) %>% 
  ungroup()

next_best <- total_performance %>% 
  filter(!model %in% c('boost_tree_ensemble','fri')) %>%
  mutate(model_rank = rank(srmse)) %>% 
  filter(model_rank < 3) %>% 
  summarise(model = model[model_rank == 1],
            percent_improvement = abs(srmse[model_rank == 1] / srmse[model_rank == 2] - 1),
            srmse = srmse[model_rank == 1])


top_total_forecast <- total_forecast %>% 
  mutate(combo = paste(model, sep = "_")) %>% 
  filter(combo %in% top_models$combo) %>% 
  left_join(next_best, by = c("model"))


total_forecast_figure <- top_total_forecast %>% 
  ggplot() + 
  geom_area(aes(year, observed), fill = "darkgray") + 
  geom_point(aes(year, forecast, fill = model, alpha = srmse), shape = 21, size = 2) +
  fishualize::scale_fill_fish_d(option = "Trimma_lantana") + 
  fishualize::scale_color_fish_d(option = "Trimma_lantana") + 
  scale_alpha_continuous(range = c(1,0.25), name = "SRMSE") + 
  scale_y_continuous(expand = expansion(c(0,.05)), name = "Returns (Millions of Salmon)") + 
  scale_x_continuous(name = '') 


total_forecast_figure



# ensemble performance


pal <- pnw_palette("Winter",n = n_distinct(system_performance$model))

top_ensemble <- system_performance %>% 
  group_by(system) %>% 
  filter(model %in% c('boost_tree_ensemble','fri')) %>%
  filter(srmse == min(srmse)) %>% 
  select(model, system, srmse) %>% 
  rename(ens_srmse = srmse) %>% 
  ungroup() %>% 
  mutate(combo = paste(system, model, sep = "_"))
  

top_non_ensemble <- system_performance %>% 
  group_by(system) %>% 
  filter(!model %in% c('boost_tree_ensemble','fri')) %>%
  filter(srmse == min(srmse)) %>% 
  select(system, srmse) %>%
  ungroup()
  
ensemble_performance <- top_ensemble %>% 
  left_join(top_non_ensemble, by = "system") %>% 
  mutate(ens_improvement = 1 - ens_srmse / srmse)


top_ensemble_system_forecast <- system_forecast %>% 
  mutate(combo = paste(system, model, sep = "_")) %>% 
  filter(combo %in% top_ensemble$combo) %>% 
  left_join(ensemble_performance, by = c("system","model"))

system_ensemble_forecast_figure <- top_ensemble_system_forecast %>% 
  mutate(model = snakecase::to_title_case(model)) %>% 
  ggplot() + 
  geom_area(aes(year, observed), fill = "darkgray") + 
  geom_point(aes(year, forecast, shape = model, fill = ens_improvement), size = 2, alpha = 0.95) +
  scale_fill_gradient2(low = "tomato", high = "steelblue", mid = "white", midpoint = 0,labels = label_percent(accuracy = 1),
                       guide = guide_colorbar(frame.colour = "black", ticks.colour = "black"),
                       name = "% Ensemble Improvement") +
  # fishualize::scale_fill_fish_d(option = "Trimma_lantana") + 
  # fishualize::scale_color_fish_d(option = "Trimma_lantana") + 
  # scale_alpha_continuous(range = c(1,0.25), name = "MASE") +
  scale_shape_manual(values = c(24,21), name = '') +
  scale_x_continuous(name = '') + 
  scale_y_continuous(expand = expansion(c(0,.05)), name = "Returns (Millions of Salmon)") + 
  theme(legend.direction = "horizontal", 
        legend.position = c(.7,.1)) + 
  facet_wrap(~system, scales = "free_y")
  

  
# choose best by system from ensemble or FRI

# make shape model type
# make color percent difference in MASE between ensemble and the next best non-ensemble method
# where blue is an improvement and red is a loss and white means no change

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




# system residuals --------------------------------------------------------

yearly_system_resid_struggles_figure <- system_forecast %>% 
  mutate(resid = forecast - observed) %>% 
  group_by(system) %>% 
  mutate(scaled_resid = scale(resid)) %>% 
  filter(!model %in% c("boost_tree_ensemble","fri")) %>% 
  ggplot() + 
  geom_ribbon(aes(year, ymin = 1, ymax = 4), fill = "tomato", alpha = 0.5) +
  geom_ribbon(aes(year, ymin = -4, ymax = -1), fill = "tomato", alpha = 0.5) +
  geom_hline(aes(yintercept = 0)) +
  geom_line(aes(year, scaled_resid, color = model)) + 
  facet_wrap(~system) + 
  # scale_color_simpsons(name = '') +
  fishualize::scale_color_fish_d(option = "Trimma_lantana", name = '') +
  theme(legend.position = c(0.7, .15), 
        legend.direction = "horizontal") + 
  scale_x_continuous(name = "") + 
  scale_y_continuous(name = "Standardized Residuals", limits = c(-4,4))
  # scale_color_discrete(name = '')

yearly_system_resid_struggles_figure
# VOI plot ----------------------------------------------------------------

    if (file.exists(file.path(results_dir, "next_forecast.rds"))){
      
      next_forecast <- read_rds(file.path(results_dir, "next_forecast.rds"))
      
      latest_forecast <- next_forecast %>%
        mutate(pred = map(pred,"salmon_data")) %>%
        unnest(cols = pred) %>% 
        ungroup() %>% 
        filter(ret_yr == max(ret_yr))
      
      
      latest_forecast %>% 
        group_by(model_type) %>% 
        summarise(forecast = sum(pred))
      
      raw_forecast_table <- latest_forecast %>% 
        ungroup() %>% 
        filter(ret_yr == max(ret_yr)) %>% 
        rename(forecast = pred,
               age_group = dep_age,
               year = ret_yr,
               model = model_type) %>% 
        ungroup() %>% 
        mutate(
          age_group = str_replace(age_group, "\\.","_"),
          age_group= forcats::fct_relevel(age_group, c("1_2","1_3","2_2","2_3"))) %>% 
        select(year, system, age_group, forecast, model) %>% 
        bind_rows(forecasts %>% select(year, system, age_group, forecast, model)) %>% 
        group_by(system, year, model) %>%
        mutate(forecast = forecast) %>% 
        mutate(Totals = sum(forecast)) %>%
        ungroup() %>% 
        arrange(year, age_group) %>% 
        pivot_wider(names_from = age_group, values_from = forecast) %>% 
        select(dplyr::everything(),-Totals, Totals) %>% 
        arrange(desc(year))
      
      # raw_forecast_table %>%
      #   pivot_longer(contains("_"), names_to = "age_group", values_to = "forecast") %>%
      #   group_by(year, model) %>%
      #   summarise(total_forecast = sum(forecast)) %>%
      #   ungroup() %>%
      #   ggplot(aes(year, total_forecast, color  = model)) +
      #   geom_line()
      # 
      write_csv(raw_forecast_table %>% mutate_if(is.numeric,round), file.path(results_dir, "raw-machine-learning-forecast-table.csv"))
      
      total_vars <- colnames(raw_forecast_table)
      
      total_vars <- total_vars[str_detect(total_vars,"(\\.)|(Totals)")]
      
      forecast_table <- raw_forecast_table %>%
        filter(model %in% c("rand_forest", "boost_tree"), year == 2020) %>%
        group_by(year, model) %>%
        gt(rowname_col = "system") %>%
        summary_rows(
          groups = TRUE,
          columns = vars(total_vars) ,
          fns = list(Totals = "sum"),
          formatter = fmt_number,
          decimals = 0,
          use_seps = TRUE
          
        ) %>%
        gt::fmt_number(columns = total_vars, decimals = 0) %>%
        gt::tab_spanner(label = "Age Group",
                        columns = (total_vars[total_vars != "Totals"])) %>%
        gt::tab_style(
          style = cell_text(weight = "bold"),
          locations = list(cells_summary(),
                           cells_body(columns = vars(Totals)))
        )
      
      
        
        if (run_importance == TRUE) {
        
          boost_forecast <- next_forecast %>% 
        filter(model_type == "boost_tree")

      a <- boost_forecast %>% 
        mutate(pred = map(pred, "salmon_data")) %>% 
        unnest(cols = pred) %>% 
        group_by(test_year, model_type) %>% 
        summarise(pred = sum(pred))
            
      boost_forecast <- boost_forecast %>% 
        mutate(model_fit = map(pred, c("trained_model","fit")))

      get_importance <- function(fit){
      
      importance_matrix <- xgboost::xgb.importance(fit$feature_names, model =fit)
      
      # xgboost::xgb.ggplot.importance(importance_matrix)
      # 
      
      }
      
      boost_forecast <- boost_forecast %>% 
        mutate(importance = map(model_fit, safely(get_importance)))
      
      has_importance <- map_lgl(map(boost_forecast$importance,"error"), is.null)
      
      var_importance <- boost_forecast %>% 
        filter(has_importance) %>% 
        mutate(importance = map(importance, "result")) %>% 
        select(-pred,-model_fit) %>% 
        unnest(cols = importance) %>% 
        mutate(short_feature = str_replace_all(Feature,"age_\\d_","past_")) %>% 
        mutate(short_feature = str_remove_all(short_feature,"rugg_"))
      
      
      system_importance <- var_importance %>% 
        group_by(pred_system, short_feature) %>% 
        summarise(mean_importance = mean(Gain))
      
    
      system_varimportance_figure <- system_importance %>% 
        mutate(short_feature = case_when(short_feature == "ret_yr" ~ "Return Year", short_feature == "env_sst" ~ "SST", short_feature == "env_slp" ~"SLP", TRUE ~ short_feature)) %>% 
        mutate(short_feature = snakecase::to_title_case(short_feature)) %>% 
        filter(mean_importance > 0.05) %>% 
        ggplot(aes(reorder(short_feature,mean_importance), mean_importance)) + 
        geom_hline(aes(yintercept = 0)) +
        geom_col() + 
        facet_wrap(~ pred_system, scales = "free_y") + 
        scale_x_discrete(name = "", guide = guide_axis(check.overlap = FALSE)) +
        scale_y_continuous(name = "Mean Variable Importance") + 
        coord_flip() + 
        theme(axis.text.y = element_text(size = 8))
        
      system_varimportance_figure
  
        } # close importance
      
      }
      
    

# retrospective bias plot -------------------------------------------------

    
    
    
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


performance <- ls()[str_detect(ls(), "_performance$")]
    
forecasts <- ls()[str_detect(ls(), "_forecast$") & !str_detect(ls(), "run_") & !str_detect(ls(), "next_forecast")]
        
plots <- ls()[str_detect(ls(), "(_plot)|(_figure)")]
    
save(list = performance, file = file.path(results_dir, "performance.RData"))

save(list = forecasts, file = file.path(results_dir, "forecasts.RData"))

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

