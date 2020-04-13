#########################
### mechasalmon ########
########################
# author: Dan Ovando
# purpose: develop and test machine learning approaches to salmon forecasting
#


# load ------------------------------------------------------------------------
library(tidyverse)
library(keras)
library(ggridges)
library(recipes)
library(here)
library(ranger)
library(rerddap)
library(tidymodels)
library(furrr)
library(ggthemes)
library(ggsci)
library(ggrepel)

functions <- list.files(here("machine-learning", "functions"))

purrr::walk(functions, ~ source(here::here("machine-learning", "functions", .x)))

#aha,

# https://stackoverflow.com/questions/9944246/setting-defaults-for-geoms-and-scales-ggplot2?rq=1
# scale_colour_discrete <-
#   function(...)
#     scale_colour_brewer(..., palette = "Set2")
# scale_fill_discrete <-
#   function(...)
#     scale_fill_brewer(... , palette = "Set2")

# set options -------------------------------------------------------------

fit_forests <- FALSE

run_query_erddap <-  FALSE

stride <- 4

weight_returns <- FALSE

cores <- 14

trees <- 500

freshwater_cohort <- TRUE

run <- "v0.6p"

description <- "testing"

rundir <- here("machine-learning", "results", run)

if (!dir.exists(rundir)) {
  dir.create(rundir, recursive = TRUE)
  
  dir.create(file.path(rundir, "figs"), recursive = TRUE)
  
  dir.create(file.path(rundir, "fits"), recursive = TRUE)
  
  write(description, file = file.path(rundir, "description.txt"))
  
}


last_year <- 2019 # the final year in the data

min_year <- 1954 # only include data greater than or equal this year

age_groups <- 4 #number of top age groups to include

first_year <- 2000 # the first year splitting the test and training data

scalar <- 100
#10000 # number do divide the total returns by just to get the data a bit closer to the -1 to 1 range

min_lat <-  52

max_lat <- 60

min_lon <- -178

max_lon <- -156

min_year <- 1960

max_year <- 2015



# load data ---------------------------------------------------------------


source(here("machine-learning","scripts","load-data.R"))

last_year <- max(pink_data$year) # the final year in the data




pinks <- pink_data %>%
  filter(species == "pink",
         region %in% c("wak", "spen", "kod", "ci", "pws", "seak")) %>%
  ungroup()

pinks %>% 
  ggplot(aes(year, returns, color = region)) + 
  geom_line(show.legend = FALSE, size = 1.5) + 
  scale_color_colorblind() + 
  facet_wrap(~region, scales = "free_y") + 
  labs(x = '', y = "Returns")

rest <- pink_data %>% 
  # filter(species != "pink") %>% 
  mutate(year = year + 1) %>% 
  mutate(combo = paste(species, region, sep = '_')) %>% 
  select(-species,-region) %>%
  group_by(combo) %>% 
  na.omit() %>% 
  pivot_wider(names_from = combo, values_from =  returns)

rest_lag <- pink_data %>% 
  filter(!str_detect(region,"(japan)|(korea)")) %>%
  mutate(year = year + 2) %>% 
  mutate(combo = paste(species, region, sep = '_lag_')) %>% 
  select(-species,-region) %>%
  group_by(combo) %>% 
  # mutate(returns = scale(returns)) %>% 
  ungroup() %>% 
  na.omit() %>% 
  pivot_wider(names_from = combo, values_from =  returns)


data <- pinks %>% 
  # right_join(rest, by = "year") %>% 
  right_join(rest_lag, by = "year") %>% 
  select(-species,-contains("_korea")) %>% 
  filter(!is.na(returns)) %>% 
  rename(ret_yr = year,
         ret = returns) %>% 
  mutate(even = ret_yr %% 2) %>% 
  left_join(wide_spatial_env %>% mutate(year = year + 1), by = c("ret_yr" = "year")) # assume environment is most prevalant in first year of life

data %>% 
  ggplot(aes(ret_yr, ret, color = region, shape = factor(even))) + 
  geom_line() + 
  geom_point() +
  facet_wrap(~even)

# jsut curious, how correlated are the tow runs anyway

  lm(
    ret ~ lag_ret + region,
    data =  data %>% group_by(region) %>% arrange(ret_yr) %>% mutate(lag_ret = lag(ret, 1))
  ) %>% 
    summary()
  
 a =  lm(
    ret ~ lag_ret + region,
    data =  data %>% group_by(region) %>% arrange(ret_yr) %>% mutate(lag_ret = lag(ret, 2))
  ) 
  
  wtf <- broom::augment(a)


  plot(wtf$lag_ret, wtf$ret)
  
# run leave-one-out time analysis -----------------------------------------

looframe <-
  tidyr::expand_grid(
    cohort_only = c(TRUE),
    test_year = first_year:last_year,
    model_type = c("rand_forest","boost_tree"),
    # model_type = c("mars"),
    use_salmon_pca = c(TRUE, FALSE),
    factor_years = c(TRUE),
    log_returns = c(FALSE),
    assess = c(1),
    delta_returns = c(FALSE),
    omit_nas = c(TRUE)
  ) %>% 
  filter(!(log_returns == TRUE & delta_returns == TRUE))


future::plan(future::multiprocess, workers = cores)

if (fit_forests == TRUE){
  
  
  a <- Sys.time()
  set.seed(42)
  loo_preds <- looframe %>%
    # filter(model_type == "mars") %>%
    # sample_n(10) %>%
    # slice(2) %>%
    mutate(pred = future_pmap(
      list(
        cohort_only = cohort_only,
        test_year = test_year,
        model_type = model_type,
        log_returns = log_returns,
        delta_returns = delta_returns,
        omit_nas = omit_nas,
        assess = assess,
        factor_years = factor_years,
        use_salmon_pca = use_salmon_pca
      ),
      safely(fit_pink_salmon),
      data = data,
      scalar = scalar,
      weight = weight_returns,
      trees = trees,
      n_mtry = 8,
      initial_prop = 0.5,
      .progress = TRUE,
      forecast = FALSE    ))
  Sys.time() - a
  
  
  
  # 
  write_rds(loo_preds, path = file.path(rundir, "pink_loo_preds.rds"))
  
} else {
  loo_preds <- readr::read_rds(file.path(rundir, "pink_loo_preds.rds"))
}


olp <- loo_preds

# loo_preds <- olp

# a = loo_preds %>% 
#   filter(model_type == "boost_tree")

loo_preds <- loo_preds %>%
  filter(map_lgl(map(pred, "error"), is.null)) %>%
  mutate(pred = map(pred,c("result","salmon_data")))

# process leave-one-out analysis --------------------------------------------

rids <- rlang::parse_exprs(colnames(looframe)[!colnames(looframe) %in% c("dep_age","test_year")])

ogloo <- loo_preds

loo_results <- loo_preds %>% 
  unnest(cols = pred) %>% 
  filter(ret_yr == test_year,
         assess == min(assess)) %>% 
  mutate(id = paste(!!!rids, sep ='-'),
         pred = pmax(0, pred))

run_ids <- rlang::parse_exprs(c("id",colnames(looframe)[!colnames(looframe) %in% c("dep_age","test_year")]))



loo_results %>%
  ggplot(aes(ret, pred, color = assess)) +
  geom_point() +
  geom_abline(aes(slope = 1, intercept = 0)) +
  facet_grid(log_returns ~ delta_returns,
             labeller = "label_both")

even_loo_results <- loo_results %>% 
  group_by(ret_yr, even,!!!run_ids) %>% 
  summarise(observed = sum(ret),
            predicted = sum(pred))


region_loo_results <- loo_results %>% 
  group_by(ret_yr,region, !!!run_ids) %>% 
  summarise(observed = sum(ret),
            predicted = sum(pred))

total_loo_results <- loo_results %>% 
  group_by(ret_yr, !!!run_ids) %>% 
  summarise(observed = sum(ret),
            predicted = sum(pred))

even_region_loo_results <- loo_results %>% 
  group_by(ret_yr,region, even, model_type,!!!run_ids) %>% 
  summarise(observed = sum(ret),
            predicted = sum(pred))


total_loo_results %>% 
  ggplot() + 
  geom_col(dat = data, aes(ret_yr,ret)) + 
  geom_point(aes(ret_yr, predicted, fill = log_returns), shape = 21, size= 2, alpha = 0.85) +
  theme_minimal() + 
  scale_x_continuous(limits = c(first_year - 1, NA))

performance_summary <- total_loo_results %>%
  group_by(!!!run_ids) %>%
  yardstick::rmse(truth = observed, estimate = predicted) %>%
  arrange(.estimate)


# performance_summary <- total_loo_results %>%
#   group_by(id,model_type, log_returns, delta_returns, use_spatial_enviro) %>%
#   yardstick::rmse(truth = observed, estimate = predicted) %>%
#   arrange(.estimate)

best_performer <- performance_summary %>% 
  # filter(model_type == "boost_tree") %>% 
  filter(.estimate == min(.estimate))


complete_best_loo_preds <- loo_preds %>% 
  unnest(cols = pred) %>% 
  mutate(id = paste(!!!rids, sep ='-'),
         pred = pmax(0, pred)) %>% 
  filter(ret_yr > 1995) %>% 
  filter(id == best_performer$id)


loo_evo_results <- complete_best_loo_preds %>% 
  group_by(ret_yr, test_year) %>% 
  summarise(ret = sum(ret),
            pred = sum(pred)) %>% 
  ggplot() + 
  geom_point(aes(ret_yr, ret)) + 
  geom_line(aes(ret_yr, pred, color = ret_yr >= test_year)) + 
  facet_wrap(~test_year)


total_loo_results %>% 
  filter(id == best_performer$id) %>% 
  ggplot() + 
  geom_col(dat = data, aes(ret_yr,ret)) + 
  geom_point(aes(ret_yr, predicted, color = "forecast"), shape = 21, size= 4,fill = "tomato", alpha = 0.85) +
  scale_x_continuous(limits = c(first_year - 1, NA)) + 
  labs(x = "Year", y = "Alaskan Pink Returns (Millions of fish)",
       caption = "Bars are observed returns, points machine learning forecasts") + 
  theme(axis.text = element_text(size = 16)) +
  scale_color_discrete(name = '')


best_loo_summary <- loo_preds %>%
  unnest(cols = pred) %>% 
  group_by(ret_yr, region) %>% 
  summarise(ret = sum(ret),
            pred = sum(pred)) %>% 
  group_by(region) %>% 
  summarise(rmse = yardstick::rmse_vec(ret, pred),
            mae = yardstick::mae_vec(ret, pred),
            rsq = yardstick::rsq_vec(ret, pred),
            ccc = yardstick::ccc_vec(ret, pred),
            median_bias = median((pred - ret), na.rm = TRUE)) %>% 
  ungroup() %>% 
  arrange(desc(rsq))



# run prediciton for next year --------------------------------------------

# pred_data <- data %>% 
#   filter(ret_yr == (max(ret_yr))) %>% 
#   mutate(ret_yr = last_year + 1) %>% 
#   bind_rows(data)

# last_year <- 2014
# 
# pred_data <- data %>% 
#   filter(ret_yr == 2014) %>% 
#   mutate(ret_yr = last_year + 1) %>% 
#   bind_rows(data %>% filter(ret_yr < 2015))

# 
# best_loo_preds %>% 
#   select(-loo_plot) %>% 
#   unnest(cols = data) %>% 
#   group_by(ret_yr) %>% 
#   summarise(pred = sum(pred)) %>% 
#   ggplot(aes(ret_yr, pred)) + 
#   geom_line() + 
#   geom_vline(aes(xintercept = 2015))

predframe <-
  tidyr::expand_grid(test_year =  max(pink_data$year))


  set.seed(22)
  forecast_fit <- predframe %>%
    mutate(
      pred = pmap(
        list(test_year = test_year),
        fit_pink_salmon,
        cohort_only = best_performer$cohort_only,
        model_type = best_performer$model_type,
        log_returns = best_performer$log_returns,
        use_salmon_pca = best_performer$use_salmon_pca,
        delta_returns = best_performer$delta_returns,
        omit_nas = best_performer$omit_nas,
        assess = best_performer$assess,
        factor_years = best_performer$factor_years,
        data = data,
        scalar = scalar,
        weight = weight_returns,
        trees = trees,
        n_mtry = 8,
        forecast = TRUE
      )
    )

next_forecast <- forecast_fit %>%
  mutate(pred = map(pred,"salmon_data")) %>%
  unnest(cols = pred) 


next_forecast %>% 
  group_by(ret_yr) %>% 
  summarise(observed = sum(ret),
            predicted = sum(pred)) %>% 
  ggplot() + 
  geom_point(aes(ret_yr, observed)) + 
  geom_line(aes(ret_yr, predicted), color = "red")


next_forecast %>% 
  group_by(ret_yr,region) %>% 
  summarise(observed = sum(ret),
            predicted = sum(pred)) %>% 
  ggplot() + 
  geom_point(aes(ret_yr, observed)) + 
  geom_line(aes(ret_yr, predicted), color = "red") + 
  facet_wrap(~region, scales = "free_y")


next_forecast %>% 
  filter(ret_yr >= 2000) %>% 
  group_by(ret_yr, region) %>% 
  summarise(observed = sum(ret),
            predicted = sum(pred)) %>% 
  ggplot() + 
  geom_point(aes(ret_yr, observed)) + 
  geom_line(aes(ret_yr, predicted), color = "red") + 
  facet_wrap(~region)


# compare predictions -----------------------------------------------------



# pull out "truth"

observed <- data %>%
  select(ret_yr, region, ret) %>%
  rename(observed = ret) 

observed %>%
  group_by(ret_yr) %>%
  summarise(o = sum(observed)) %>%
  ggplot(aes(ret_yr, o)) +
  geom_point()



# create naive forecasts

simple_forecast <- observed %>%
  group_by(region) %>%
  arrange(ret_yr) %>%
  mutate(
    lag_forecast = observed,
    runmean_forecast = RcppRoll::roll_meanr(observed, 4)
  ) %>% 
  mutate(ret_yr = ret_yr + 2) %>% 
  pivot_longer(cols = contains("_forecast"), names_to = "model", values_to = "forecast") %>% 
  select(ret_yr, region, model, forecast)


# process ml forecast

ml_forecast <- loo_results %>%
  filter(id == best_performer$id) %>%
  select(ret_yr, region, pred) %>%
  rename(forecast = pred) %>%
  mutate(model = "ml_forecast", ret_yr = ret_yr)

# aggregate forecasts

forecasts <- ml_forecast %>% 
  bind_rows(simple_forecast) %>% 
  left_join(observed, by = c("ret_yr","region")) %>% 
  na.omit() %>% 
  mutate(observed =  observed,
         forecast = forecast) %>% 
  mutate(model = str_replace(model, "_forecast",""))

# create performance summaries

total_forecast <- forecasts %>% 
  filter(ret_yr >= first_year) %>% 
  group_by(ret_yr, model) %>% 
  summarise(observed = sum(observed),
            forecast = sum(forecast))


system_forecast <- forecasts %>% 
  filter(ret_yr >= first_year) %>% 
  group_by(ret_yr,region,model) %>% 
  summarise(observed = sum(observed),
            forecast = sum(forecast))


total_forecast_plot <- total_forecast %>% 
  ggplot() + 
  geom_point(aes(ret_yr, observed),size = 2) + 
  geom_line(aes(ret_yr, forecast, color = model), show.legend = FALSE) + 
  facet_wrap(~model)

total_forecast_plot


system_forecast_plot <- system_forecast %>% 
  ggplot() + 
  geom_point(aes(ret_yr, observed),size = 2) + 
  geom_line(aes(ret_yr, forecast, color = model), show.legend = FALSE) + 
  facet_grid(region~model, scales = "free_y") + 
  theme_dark()

system_forecast_plot



# evaluate performance



total_performance <- total_forecast %>% 
  group_by(model) %>% 
  summarise(rmse = yardstick::rmse_vec(truth = observed, estimate = forecast),
            r2 = yardstick::rsq_vec(truth = observed, estimate = forecast),
            ccc = yardstick::ccc_vec(truth = observed, estimate = forecast),
            mape = yardstick::mape_vec(truth = observed, estimate = forecast),
            bias = mean(forecast - observed)) %>% 
  ungroup() %>% 
  arrange(rmse)

recent <- total_forecast %>% 
  filter(ret_yr >= 2014) %>% 
  group_by(model) %>% 
  summarise(rmse = yardstick::rmse_vec(truth = observed, estimate = forecast),
            r2 = yardstick::rsq_vec(truth = observed, estimate = forecast),
            ccc = yardstick::ccc_vec(truth = observed, estimate = forecast),
            mape = yardstick::mape_vec(truth = observed, estimate = forecast),
            bias = mean(forecast - observed)) %>% 
  ungroup() %>% 
  arrange(rmse)


system_performance <- system_forecast %>% 
  group_by(model, region) %>% 
  summarise(rmse = yardstick::rmse_vec(truth = observed, estimate = forecast),
            r2 = yardstick::rsq_vec(truth = observed, estimate = forecast),
            ccc = yardstick::ccc_vec(truth = observed, estimate = forecast),
            mape = yardstick::mape_vec(truth = observed, estimate = forecast),
            bias = mean(forecast - observed)) %>% 
  ungroup() %>% 
  arrange(rmse)


total_performance_plot <- total_performance %>% 
  ggplot(aes(reorder(model, mape), mape)) + 
  geom_col()

total_performance_plot


system_performance_plot <- system_performance %>% 
  group_by(region) %>% 
  ggplot(aes(reorder(model, rmse),fill = model == "ml", rmse)) + 
  geom_col() + 
  facet_wrap(~region, scales = "free_y") + 
  labs(y = "Root Mean Squared Error (millions of fish)")

system_performance_plot



# make plots --------------------------------------------------------------

salmon_data <- data

return_plot <- salmon_data %>% 
  group_by(ret_yr) %>% 
  summarise(ret = sum(ret)) %>% 
  ggplot(aes(ret_yr, ret)) + 
  geom_col(alpha = 0.75) + 
  scale_y_continuous(name = "Returns (millions of fish)") + 
  scale_x_continuous(name = '') 

reg_return_plot <- salmon_data %>% 
  group_by(ret_yr, region) %>% 
  summarise(ret = sum(ret)) %>% 
  ggplot(aes(ret_yr, ret, fill = region)) + 
  geom_col(size = 1.5, alpha = 0.5) + 
  scale_fill_d3(name = "") + 
  scale_y_continuous(name = "Returns (millions of fish)") + 
  scale_x_continuous(name = '') +
  theme(legend.position = "top") 

reg_return_plot


ml_pred_plot <- total_forecast %>% 
  filter(model == "ml") %>% 
  ggplot() + 
  geom_col(aes(ret_yr, observed),alpha = 0.75) + 
  geom_line(aes(ret_yr, forecast),color = "tomato", linetype = 2) +
  geom_point(aes(ret_yr, forecast),fill = "tomato", size = 4, shape = 21) +
  scale_y_continuous(name = "Returns (millions of fish)") + 
  scale_x_continuous(name = '')  + 
  labs(caption = "Red points are forecasts")

ml_pred_plot

all_pred_plot <- total_forecast %>% 
  ggplot() + 
  geom_col(aes(ret_yr, observed),alpha = 0.75) + 
  geom_line(aes(ret_yr, forecast, color = model), linetype = 2) +
  geom_point(aes(ret_yr, forecast, fill = model), size = 4,shape = 21, alpha = 0.75) +
  scale_y_continuous(name = "Returns (millions of fish)") + 
  scale_x_continuous(name = '')  + 
  facet_wrap(~model) +
  labs(caption = "Points are forecasts") + 
  scale_fill_ucscgb(guide = FALSE) + 
  scale_color_ucscgb(guide = FALSE) + 
  theme(axis.text = element_text(angle = 45, vjust = 0, hjust = 1, size = 10))


all_pred_plot


all_reg_pred_plot <- system_forecast %>%
  ggplot() +
  geom_col(aes(ret_yr, observed),alpha = 0.75) +
  geom_line(aes(ret_yr, forecast, color = model), linetype = 1, size = 1) +
  geom_point(aes(ret_yr, forecast, fill = model), size = 4,shape = 21) +
  scale_y_continuous(name = "Returns (millions of fish)") +
  scale_x_continuous(name = '')  +
  facet_grid(region~model, scales = "free_y") +
  scale_fill_ucscgb(guide = FALSE) +
  scale_color_ucscgb(guide = FALSE) +
  theme(axis.text = element_text(angle = 45, vjust = 0, hjust = 1, size = 10))


all_reg_pred_plot

region_performance_plot <-  system_performance %>% 
  group_by(region) %>% 
  mutate(ml_improvement = (rmse[model == "lag"] - rmse[model == "ml"]) /  rmse[model == "lag"],
         ref_rmse =rmse[model == "lag"],
         sd_rmse = sd(rmse)) %>% 
  filter(rmse == min(rmse))  %>%
  ungroup() %>% 
  mutate(scaled_rmse = -(ref_rmse - rmse) / ref_rmse,
         fface = ifelse(model == "ml", "italic","plain"),
         model = fct_recode(model,rmean = "runmean")) %>% 
  ggplot(aes(region,model, label = model,color = scaled_rmse)) + 
  geom_text(aes( size = -scaled_rmse)) + 
  scale_color_gradient(low = "tomato", high = "steelblue", 
                       labels = scales::percent,
                       name = "% Change in Error",
                       guide = guide_colorbar(ticks.colour = "black",frame.colour = "black",barwidth = unit(15, units = "lines")))+ 
  scale_x_discrete(name = '') + 
  scale_y_discrete(name = '')+
  scale_size(range = c(6,12), guide = FALSE) +
  theme(legend.position = "top")

region_performance_plot

naive_ensemble <- system_performance %>%
  filter(model != "fri") %>% 
  group_by(region) %>%
  filter(rmse == min(rmse)) %>%
  select(region, model) %>%
  rename(best_model = model)

naive_ensemble_forecasts <- forecasts %>%
  left_join(naive_ensemble, by = c("region")) %>%
  filter(model == best_model) 



naive_ensemble_forecasts_plot <- naive_ensemble_forecasts %>%
  group_by(ret_yr) %>%
  summarise(observed = sum(observed),
            forecast = sum(forecast)) %>%
  ungroup() %>%
  filter(ret_yr >= 2000) %>%
  ggplot() +
  geom_col(aes(ret_yr, observed), alpha = 0.75) +
  geom_line(aes(ret_yr, forecast), color = "tomato", linetype = 2) +
  geom_point(aes(ret_yr, forecast), fill = "tomato", size = 4, shape = 21) +
  scale_y_continuous(name = "Returns (millions of fish)") +
  scale_x_continuous(name = '')  +
  labs(caption = "Red points are naive-ensemble based forecasts")

naive_ensemble_forecasts_plot

system_naive_ensemble_forecasts_plot <-  naive_ensemble_forecasts %>%
  group_by(ret_yr, region, model) %>%
  summarise(observed = sum(observed),
            forecast = sum(forecast)) %>%
  ungroup() %>%
  filter(ret_yr >= 2000) %>%
  ggplot() +
  geom_col(aes(ret_yr, observed), alpha = 0.75) +
  geom_line(aes(ret_yr, forecast, color = model),linetype = 2, size = 1) +
  geom_point(aes(ret_yr, forecast, fill = model), size = 4, shape = 21) +
  facet_wrap(~region, scale = "free_y") +
  scale_y_continuous(name = "Returns (millions of fish)") +
  scale_x_continuous(name = '')  +
  labs(caption = "Points are naive-ensemble based forecasts")

system_naive_ensemble_forecasts_plot


total_next_forecast <-  next_forecast %>% 
  filter(ret_yr == max(ret_yr)) %>% 
  group_by(ret_yr) %>% 
  summarise(observed = sum(ret), forecast = sum(pred))

total_past_forecast <- total_forecast %>% 
  filter(model == "ml") %>% 
  select(-model) %>% 
  bind_rows(total_next_forecast)

total_past_forecast %>%
  ungroup() %>%
  mutate(label = ifelse(ret_yr == max(ret_yr), paste("2020 Forecast:", round(forecast),"Million"),'')) %>% 
  ggplot() +
  geom_col(aes(ret_yr, observed), alpha = 0.75) +
  geom_line(
    aes(ret_yr, forecast, color = ret_yr == max(ret_yr)),
    linetype = 2,
    show.legend = FALSE
  ) +
  geom_label_repel(aes(
    x = max(ret_yr),
    y = last(forecast),
    label = label
  ),
  nudge_y = 10,
  size = 5,
  direction = "y",
  force = 5,
  padding = 2,
  seed = 42) +
  geom_point(
    aes(ret_yr, forecast, fill = ret_yr == max(ret_yr)),
    size = 4,
    shape = 21,
    show.legend = FALSE
  ) +
  scale_y_continuous(name = "Returns (millions of fish)", limits = c(0,NA)) +
  scale_x_continuous(name = '', limits = c(NA, 2023)) 


# save things -------------------------------------------------------------



performance_files <- ls()[str_detect(ls(),"_performance")]

forecast_files <- ls()[str_detect(ls(),"forecast")]

loo_files <- ls()[str_detect(ls(),"_loo")]

env_files <-  ls()[str_detect(ls(),"env")]

save(list = performance_files,file = file.path(rundir,"performance_files.RData"))

save(list = loo_files,file = file.path(rundir,"loo_files.RData"))

save(list = forecast_files,file = file.path(rundir,"forecast_files.RData"))

save(list = env_files,file = file.path(rundir,"env_files.RData"))

salmon_data <- data

save(salmon_data, file = file.path(rundir,"salmon_data.RData"))

plots <- ls()[str_detect(ls(), "_plot")]


plotfoo <- function(x,height = 6, width = 9 , device = "pdf",path){
  
  ggsave(filename = file.path(path,"figs",paste(x,device, sep = '.')),plot = get(x),height = height, width = width)
  
}

walk(plots, plotfoo, path = rundir)



