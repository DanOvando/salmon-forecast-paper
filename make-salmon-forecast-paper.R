# assess salmon forecast performance across models

# if not using an Rstudio project, manually set working directory 
# to the respository folder before sourcing this script, e.g. 
# in command line setwd("/Users/danovan/projects/salmon-forecast-paper")
# Once you do that here will work regardless of the presence of an rstudio project 
# (it will notice the git file)

functions <- list.files(here::here("functions"))

purrr::walk(functions, ~ source(here::here("functions", .x)))

prep_run(results_name = "v0.7", results_description = "draft publication with boost tree improvements loo starting in 1990")

first_year <- 1990

last_year <- 2019

run_edm_forecast <- TRUE

run_dlm_forecast <- TRUE

run_ml_forecast <- TRUE

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
    years = 1990:last_year
  ) %>%
  rename(forecast = FRIfcst) %>%
  mutate(model = "fri_forecast") %>% 
  janitor::clean_names() %>% 
  rename(year = ret_yr) %>% 
  mutate(age_group = paste(fw_age, o_age, sep = "_")) %>% 
  as_tibble()



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

forecasts <- map_df(results, ~read_csv(file.path(results_dir,.x))) %>% 
  rename(year = return_year,
         forecast = predicted_returns,
         observed = observed_returns) %>% 
  filter(age_group %in% top_age_groups,
         system != "Alagnak", system != "Togiak") %>% 
  mutate(model = str_remove_all(model, "_forecast")) %>% 
  mutate(model = str_remove_all(model,"_one system top6 ages"))

forecasts %>% 
  filter(year >= first_year) %>% 
  group_by(year, model) %>% 
  summarise(observed = sum(observed),
            forecast = sum(forecast)) %>% 
  ggplot() + 
  geom_col(aes(year, observed)) + 
  geom_point(aes(year, forecast)) + 
  facet_wrap(~model)

# create performance summaries

total_forecast <- forecasts %>% 
  filter(year >= first_year) %>% 
  group_by(year, model) %>% 
  summarise(observed = sum(observed),
            forecast = sum(forecast))

age_forecast <- forecasts %>% 
  filter(year >= first_year) %>% 
  group_by(year,age_group,model) %>% 
  summarise(observed = sum(observed),
            forecast = sum(forecast))

system_forecast <- forecasts %>% 
  filter(year >= first_year) %>% 
  group_by(year,system,model) %>% 
  summarise(observed = sum(observed),
            forecast = sum(forecast))


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
  arrange(rmse)

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
  arrange(rmse)


system_performance <- system_forecast %>% 
  group_by(model, system) %>% 
  summarise(rmse = yardstick::rmse_vec(truth = observed, estimate = forecast),
            r2 = yardstick::rsq_vec(truth = observed, estimate = forecast),
            mae = yardstick::mae_vec(truth = observed, estimate = forecast),
            mase = yardstick::mase_vec(truth = observed, estimate = forecast),
            bias = mean(forecast - observed)) %>% 
  ungroup() %>% 
  arrange(rmse)

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
  arrange(rmse)


total_performance_plot <- total_performance %>% 
  ggplot(aes(reorder(model, mape), rmse)) + 
  geom_col()

total_performance_plot

age_performance_plot <- age_performance %>% 
  group_by(age_group) %>% 
  ggplot(aes(reorder(model, r2), r2)) + 
  geom_col() + 
  facet_wrap(~age_group) + 
  scale_x_discrete(guide = guide_axis(n.dodge = 2))

age_performance_plot


system_performance_plot <- system_performance %>% 
  group_by(system) %>% 
  ggplot(aes(reorder(model, r2), r2)) + 
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
  scale_y_continuous(name = "Returns (millions)") + 
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



# age system pred plot ----------------------------------------------------

age_sys_pred_plot <- age_system_forecast %>%
  filter(model != "runmean") %>%
  ggplot() +
  geom_col(aes(year, observed), position = "dodge",alpha = 0.75) +
  geom_line(aes(year, forecast, color = model), linetype = 1, size = 1) +
  # geom_point(aes(year, forecast, fill = model), size = 4,shape = 21) +
  scale_y_continuous(name = "Returns (millions)") +
  scale_x_continuous(name = '')  +
  facet_grid(system~age_group, scales = "free_y") +
  scale_fill_ucscgb() +
  scale_color_ucscgb() +
  theme(axis.text = element_text(angle = 45, vjust = 0, hjust = 1, size = 10))


age_sys_pred_plot



## -----------------------------------------------------------------------------


run_makeup <- data %>% 
  filter(ret_yr > 2010) %>% 
  mutate_if(is.factor, as.character) %>% 
  group_by(age_group, system) %>% 
  summarise(mean_ret = mean(ret)) %>% 
  ungroup() %>% 
  mutate(p_ret = mean_ret / sum(mean_ret))

age_system_performance_plot <-  age_system_performance %>% 
  group_by(age_group, system) %>% 
  mutate(ml_improvement = (rmse[model == "lag"] - rmse[model == "boost_tree"]) /  rmse[model == "lag"],
         ref_rmse =rmse[model == "lag"],
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
                       name = "% Change in Error Relative to rmean",
                       limits = c(-.75,0), 
                       breaks = seq(-.75,0, by = 0.25),
                       guide = guide_colorbar(ticks.colour = "black",frame.colour = "black",barwidth = unit(15, units = "lines")))+ 
  scale_x_discrete(name = '') + 
  scale_y_discrete(name = '')+
  scale_size(range = c(4,12), guide = FALSE) +
  theme(legend.position = "top") + 
  labs(caption = "Text indicates best performing model from 2000-2019")

age_system_performance_plot


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


# system

system_pre <- system_forecast %>% 
  filter(year < performance_break) %>% 
  group_by(system, model) %>% 
  summarise(rmse = yardstick::rmse_vec(truth = observed, estimate = forecast),
            r2 = yardstick::rsq_vec(truth = observed, estimate = forecast),
            # ccc = yardstick::ccc_vec(truth = observed, estimate = forecast),
            mape = yardstick::mape_vec(truth = observed, estimate = forecast),
            bias = mean(forecast - observed)) %>% 
  ungroup() %>% 
  group_by(system) %>% 
  filter(rmse == min(rmse)) %>% 
  rename(pre_model = model) %>% 
  ungroup()

system_post <- system_forecast %>% 
  filter(year >= performance_break) %>% 
  group_by(system, model) %>% 
  summarise(rmse = yardstick::rmse_vec(truth = observed, estimate = forecast),
            r2 = yardstick::rsq_vec(truth = observed, estimate = forecast),
            # ccc = yardstick::ccc_vec(truth = observed, estimate = forecast),
            mape = yardstick::mape_vec(truth = observed, estimate = forecast),
            bias = mean(forecast - observed)) %>% 
  ungroup() %>% 
  group_by(system) %>% 
  filter(rmse == min(rmse)) %>% 
  rename(post_model = model) %>% 
  ungroup()


system_concordance <- system_pre %>% 
  left_join(system_post, by = c("system")) %>% 
  ungroup() %>% 
  summarise(concordance = mean(pre_model == post_model))




## -----------------------------------------------------------------------------

naive_ensemble <- age_system_performance %>%
  filter(model != "fri") %>% 
  group_by(age_group, system) %>%
  filter(rmse == min(rmse, na.rm = TRUE)) %>%
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
  arrange(rmse) %>% 
  gt() %>%
  fmt_number(columns = which(colnames(.) != "model"),
             decimals = 2)



## -----------------------------------------------------------------------------


age_performance %>%
  # filter(model %in% c("fri","ml")) %>% 
  group_by(age_group) %>% 
  arrange(rmse) %>% 
  gt() %>%
  fmt_number(columns = which(map_lgl(., is.numeric)),
             decimals = 2)



## -----------------------------------------------------------------------------


system_performance %>%
  # filter(model %in% c("fri","ml")) %>% 
  group_by(system) %>% 
  arrange(rmse) %>% 
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
  ggthemes::theme_map() + 
  labs(caption = "Placeholder for a map if needed; add in river systems?")


total_return_plot <- salmon_data %>% 
  group_by(year) %>% 
  summarise(ret = sum(ret)) %>% 
  ggplot(aes(year, ret)) + 
  geom_area(alpha = 1) + 
  scale_y_continuous(name = "Returns (millions)", expand = expansion()) + 
  scale_x_continuous(name = '') + 
  theme(axis.text.x = element_blank()) + 
  labs(title = "A")

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
  theme(axis.text.x = element_blank()) + 
  labs(title = "B")


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
  labs(title = "C")


age_return_plot

figure_1 <-  (total_return_plot / system_return_plot / age_return_plot)


# figure 2 ----------------------------------------------------------------

# placeholder for summary of historic performance?



# figure 3 ----------------------------------------------------------------

pal <- pnw_palette("Winter",n = n_distinct(system_performance$model))

top_models <- system_performance %>% 
  group_by(system) %>% 
  # filter(model == "dlm") %>%
  filter(rmse == min(rmse)) %>% 
  mutate(combo = paste(system, model, sep = "_"))

next_best <- system_performance %>% 
  group_by(system) %>% 
  mutate(model_rank = rank(rmse)) %>% 
  filter(model_rank < 3) %>% 
  arrange(system)  %>% 
  summarise(model = model[model_rank == 1],
            percent_improvement = abs(rmse[model_rank == 1] / rmse[model_rank == 2] - 1))


top_system_forecast <- system_forecast %>% 
  mutate(combo = paste(system, model, sep = "_")) %>% 
  # filter(model %in% "boost_tree") %>%
  filter(combo %in% top_models$combo) %>%
  left_join(next_best, by = c("model", "system"))

system_forecast_figure <- top_system_forecast %>% 
  ggplot() + 
  geom_area(aes(year, observed), fill = "darkgray") + 
  geom_point(aes(year, forecast, fill = model, alpha = percent_improvement), shape = 21, size = 3) +
  facet_wrap(~system, scales = "free_y") + 
  fishualize::scale_fill_fish_d(option = "Trimma_lantana") + 
  fishualize::scale_color_fish_d(option = "Trimma_lantana") + 
  scale_alpha_continuous(range = c(0.25,1), labels = percent, name = "% Improvement on 2nd Best Model") + 
  scale_y_continuous(expand = expansion(c(0,.05)), name = "Returns")

system_forecast_figure


# figure 4 ----------------------------------------------------------------

top_models <- age_performance %>% 
  # filter(model == "boost_tree") %>% 
  group_by(age_group) %>% 
  filter(rmse == min(rmse)) %>% 
  mutate(combo = paste(age_group, model, sep = "_")) %>% 
  ungroup()


next_best <- age_performance %>% 
  group_by(age_group) %>% 
  mutate(model_rank = rank(rmse)) %>% 
  filter(model_rank < 3) %>% 
  arrange(age_group)  %>% 
  summarise(model = model[model_rank == 1],
            percent_improvement = abs(rmse[model_rank == 1] / rmse[model_rank == 2] - 1))


top_age_forecast <- age_forecast %>% 
  mutate(combo = paste(age_group, model, sep = "_")) %>% 
  filter(combo %in% top_models$combo) %>% 
  left_join(next_best, by = c("model", "age_group"))


age_forecast_figure <- top_age_forecast %>% 
  ggplot() + 
  geom_area(aes(year, observed), fill = "darkgray") + 
  geom_point(aes(year, forecast, fill = model, alpha = percent_improvement), shape = 21, size = 3) +
  facet_wrap(~age_group, scales = "free_y") + 
  fishualize::scale_fill_fish_d(option = "Trimma_lantana") + 
  fishualize::scale_color_fish_d(option = "Trimma_lantana") + 
  scale_alpha_continuous(range = c(0.25,1), labels = percent, name = "% Improvement on 2nd Best Model") + 
  scale_y_continuous(expand = expansion(c(0,.05)), name = "Returns")



age_forecast_figure
# save things -------------------------------------------------------------



plots <- ls()[str_detect(ls(), "(_plot)|(_figure)")]

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
