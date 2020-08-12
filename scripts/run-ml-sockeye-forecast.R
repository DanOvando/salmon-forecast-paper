#########################
### mechasalmon ########
########################
# author: Dan Ovando
# purpose: develop and test machine learning approaches to salmon forecasting
#


# load ------------------------------------------------------------------------

# functions <- list.files(here::here("functions"))
#
# purrr::walk(functions, ~ source(here::here("functions", .x)))
#
# prep_run(results_name = "v0.5", results_description = "testing machine learning")

# if (!dir.exists(file.path(results_dir,"figs"))) {
#   
#   dir.create(file.path(results_dir, "figs"), recursive = TRUE)
#   
#   dir.create(file.path(results_dir, "fits"), recursive = TRUE)
#   
# }
set.seed(42)

functions <- list.files(here::here("functions"))

purrr::walk(functions, ~ source(here::here("functions", .x)))

prep_run(results_name = "v0.5.2", results_description = "draft publication with boost tree improvements loo starting in 1990",
         first_year = 1990,
         last_year = 2019,
         min_year = 1963,
         eval_year = 2000)


#aha,

# https://stackoverflow.com/questions/9944246/setting-defaults-for-geoms-and-scales-ggplot2?rq=1
# scale_colour_discrete <-
#   function(...)
#     scale_colour_brewer(..., palette = "Set2")
# scale_fill_discrete <-
#   function(...)
#     scale_fill_brewer(... , palette = "Set2")

# set options -------------------------------------------------------------

# first_year <- 1990 # the first year splitting the test and training data

# last_year <- 2019 # the final year in the data

# min_year <- 1963 # only include data greater than or equal this year

fit_parsnip_models <- FALSE

fit_rnn_models <- FALSE

run_query_erddap <-  FALSE

run_next_forecast <- FALSE

by_system <- TRUE

stride <- 4 #stride for errdaap data

weight_returns <- FALSE

cores <- parallel::detectCores() - 2

trees <- 1000

freshwater_cohort <- TRUE #leave as true

age_groups <- 4 #number of top age groups to include

scalar <- 10000
  #10000 # number do divide the total returns by just to get the data a bit closer to the -1 to 1 range

min_lat <-  52

max_lat <- 60

min_lon <- -178

max_lon <- -156

max_year <- 2019

# load data ---------------------------------------------------------------



data <- read_csv(here::here("data", paste0(last_year, ".csv"))) %>%
  janitor::clean_names() %>%
  mutate(age_group = paste(fw_age, o_age, sep = "."))

ruggerone_data <-
  tidyxl::xlsx_cells(
    here(
      "data",
      "mcf210023-sup-0001-tables1-s24 6.00.07 PM.xlsx"
    ),
    sheet = "ST 1-4 Nat-orig ret (nos)52-15"
  )


# find blank columns to indicate where the spaces are between the tables
# https://nacnudus.github.io/spreadsheet-munging-strategies/small-multiples-with-all-headers-present-for-each-multiple.html

seperators <- ruggerone_data %>%
  group_by(col) %>%
  summarise(is_sep = all(is_blank == TRUE)) %>%
  filter(is_sep == TRUE)

species <- ruggerone_data %>%
  filter(str_detect(character, "Natural-origin"))

exent <-
  ruggerone_data$local_format_id[ruggerone_data$row == unique(species$row)]

subtables <-
  ruggerone_data %>%
  filter(str_detect(character, "Year")) %>%
  select(row, col)

ruggerone_data <- ruggerone_data %>%
  filter(!col %in% seperators$col,
         col < seperators$col[3])


partitions <- unpivotr::partition(ruggerone_data, subtables) %>%
  mutate(species = species$character)


foo <- function(cells) {
  cells %>%
    unpivotr::behead("N", header) %>%
    select(row, data_type, header, numeric) %>%
    unpivotr::spatter(header) %>%
    select(-row, -Total) %>%
    janitor::clean_names() %>%
    gather(region, returns, -year)
}

rugg_salmon = partitions %>%
  mutate(cells = map(cells, foo)) %>%
  unnest(cols = cells) %>%
  select(-corner_row,-corner_col) %>%
  mutate(species = str_trim(tolower(
    str_replace_all(species, "(Natural-origin)|(Salmon)", '')
  ))) %>%
  filter(species != "sockeye")

pink_data <- partitions %>%
  mutate(cells = map(cells, foo)) %>%
  unnest(cols = cells) %>%
  select(-corner_row,-corner_col) %>%
  mutate(species = str_trim(tolower(
    str_replace_all(species, "(Natural-origin)|(Salmon)", '')
  ))) 






# filter data ------------------------------------------------------------------

data <- data %>%
  filter(ret_yr >= min_year,
         system != "Alagnak", system != "Togiak") # getting rid of alagnak due to problems in data, apparently sporadic observation tower?

# identify top system in terms of total returns
top_system <- data %>%
  group_by(system) %>%
  summarise(tr = sum(ret)) %>%
  top_n(1, tr)

# identify top age groups to reduce the number of models a bit
top_age_groups <- data %>%
  group_by(age_group) %>%
  summarise(tr = sum(ret)) %>%
  arrange(desc(tr))

top_age_groups <- data %>%
  group_by(age_group) %>%
  summarise(tr = sum(ret)) %>%
  mutate(ptr = percent_rank(tr)) %>%
  arrange(desc(tr))

viable_age_groups <- top_age_groups %>%
  filter(ptr > 0.5) %>% {
    .$age_group
  }

top_age_groups <- top_age_groups %>%
  top_n(4, tr) %>% {
    .$age_group
  }

# data <- data %>%
#   filter(age_group %in% viable_age_groups) # filter down to top 4 age groups

# load environmental data -------------------------------------------------

# bbay <- sf::st_read(here("data","salmon_districts.kml"))
# bbay_ll <- sf::st_coordinates(bbay) %>%
#   as_tibble() %>%
#   rename(long = X, lat = Y) %>%
#   mutate(DistrictID = as.numeric(as.character(bbay$Name)))

# 
# location_ids <- readxl::read_xlsx(here("machine-learning","data","ID_Systems.xlsx"))
# 
# bbay_ll <- bbay_ll %>% 
#   left_join(location_ids, by ="DistrictID") %>% 
#   filter(System %in% data$system) %>% 
#   janitor::clean_names() %>% 
#   select(system, lat,long) %>% 
#   group_by(system) %>% 
#   summarise(lat = mean(lat),
#             long = mean(long))


# enter ocean sometime in spring, median over that period

# the non-gridded data, aka the salinity, aren't working right
# so ignoring for now

erddap_data <-
  readxl::read_xlsx(here::here("data", "Environment", "ERDDAP Datasets of Interest.xlsx")) %>%
  janitor::clean_names() %>%
  mutate(gridded = gridded == "Y") %>%
  filter(dataset_id != "jplAquariusSSSDailyV5",
         gridded == TRUE)

if (run_query_erddap == TRUE) {
  erddap <- map2(
    erddap_data$dataset_id,
    erddap_data$gridded,
    query_erddap,
    min_lat = min_lat,
    max_lat = max_lat,
    min_lon = min_lon,
    max_lon = max_lon,
    min_year = min_year - 10,
    max_year = max_year,
    stride = 2
  ) %>%
    set_names(erddap_data$dataset_id)
  
  
  pdo <-
    read_table(
      "http://research.jisao.washington.edu/pdo/PDO.latest",
      na = c("-99.99", "99.99", '-99',"-9,90"),
      skip = 29,
      n_max = lubridate::year(Sys.time()) - 1900,
      col_names = c("year", 1:12)
    ) %>%
    slice(-1) %>% 
    gather(month, pdo,-year) %>%
    mutate(month = as.double(month),
           date = lubridate::ymd(paste(year, month, '01', sep = '-'))) %>%
    filter(month %in% (5:8)) %>% 
    mutate(year = str_replace_all(year, "\\D",'') %>%as.numeric(),
           pdo = as.numeric(pdo)) %>% 
    group_by(year) %>%
    summarise(env_pdo = mean(pdo, na.rm = TRUE)) 
  
  
  # pdo <-
  #   read_table(
  #     "https://www.esrl.noaa.gov/psd/gcos_wgsp/Timeseries/Data/pdo.long.data",
  #     na = c("-99.99", "99.99", '-99',"-9,90"),
  #     skip = 2,
  #     n_max = lubridate::year(Sys.time()) - 1900,
  #     col_names = c("year", 1:12)
  #   ) %>%
  #   gather(month, pdo,-year) %>%
  #   mutate(month = as.double(month),
  #          date = lubridate::ymd(paste(year, month, '01', sep = '-'))) %>%
  #   filter(month %in% (5:8)) %>% 
  #   group_by(year) %>%
  #   summarise(env_pdo = mean(pdo)) 
  # 
  write_rds(pdo, here("data", "pdo.rds"))
  
  write_rds(erddap, here("data", "erddap-data.rds"))
} else {
  erddap <-
    read_rds(here("data", "erddap-data.rds"))
  
  pdo <- read_rds(here("data", "pdo.rds"))
  
  
}

# wtf <- sf::st_join(temp , bbay)

# temp <- erddap$esrlIcoads1ge$data %>% 
#   group_by(lat,lon) %>% 
#   summarise(mean_sst = mean(sst, na.rm = TRUE)) %>% 
#   ungroup() %>% 
#   # sf::st_transform(crs = a) %>% 
#   select(mean_sst)

# ggplot() + 
#   geom_sf(data =temp, aes(color = mean_sst)) + 
#   geom_sf(data = bbay) +
#   scale_color_viridis_c()

# a = sf::st_intersection(sst, bbay)

sst <- erddap$erdHadISST$data %>%
  mutate(year = lubridate::year(time),
         month = lubridate::month(time)) %>%
  as.data.frame() %>%
  filter(month %in% (5:8)) %>% 
  group_by(year) %>%
  summarise(env_sst = median(sst, na.rm = TRUE)) %>%
  ungroup() 


sea_ice <-  erddap$erdHadISSTIce$data %>%
  mutate(year = lubridate::year(time),
         month = lubridate::month(time)) %>%
  as.data.frame() %>%
  filter(month %in% (5:8)) %>% 
  group_by(year) %>%
  summarise(env_sic = median(sic, na.rm = TRUE)) %>%
  ungroup()


upstr <- erddap$esrlIcoads1ge$data %>%
  mutate(year = lubridate::year(time),
         month = lubridate::month(time)) %>%
  as.data.frame() %>%
  filter(month %in% (5:8)) %>% 
  group_by(year) %>%
  summarise(env_upstr = median(upstr, na.rm = TRUE)) %>%
  ungroup() 

sea_level_pressure <- erddap$esrlIcoads1ge$data %>%
  mutate(year = lubridate::year(time),
         month = lubridate::month(time)) %>%
  as.data.frame() %>%
  filter(month %in% (5:8)) %>% 
  group_by(year) %>%
  summarise(env_slp = median(slp, na.rm = TRUE)) %>%
  ungroup() 

# redo with spatial

spatial_sst <- erddap$esrlIcoads1ge$data %>%
  mutate(year = lubridate::year(time),
         month = lubridate::month(time)) %>%
  as.data.frame() %>%
  filter(month %in% (5:8)) %>% 
  group_by(year, lat, lon) %>%
  summarise(env_sst = median(sst, na.rm = TRUE)) %>%
  ungroup() 


spatial_sea_ice <-  erddap$erdHadISSTIce$data %>%
  mutate(year = lubridate::year(time),
         month = lubridate::month(time)) %>%
  as.data.frame() %>%
  filter(month %in% (5:8)) %>% 
  group_by(year, lat, lon) %>%
  summarise(env_sic = median(sic, na.rm = TRUE)) %>%
  ungroup()


spatial_upstr <- erddap$esrlIcoads1ge$data %>%
  mutate(year = lubridate::year(time),
         month = lubridate::month(time)) %>%
  as.data.frame() %>%
  filter(month %in% (5:8)) %>% 
  group_by(year, lat, lon) %>%
  summarise(env_upstr = median(upstr, na.rm = TRUE)) %>%
  ungroup() 

spatial_sea_level_pressure <- erddap$esrlIcoads1ge$data %>%
  mutate(year = lubridate::year(time),
         month = lubridate::month(time)) %>%
  as.data.frame() %>%
  filter(month %in% (5:8)) %>% 
  group_by(year, lat, lon) %>%
  summarise(env_slp = median(slp, na.rm = TRUE)) %>%
  ungroup() 


spatial_env <- spatial_sst %>% 
  left_join(spatial_upstr,by = c("year","lat","lon")) %>% 
  left_join(spatial_sea_level_pressure,by = c("year","lat","lon"))

# wide_spatial_env <-  wide_spatial_env %>%
#   dplyr::mutate(geometry = purrr::map2(lon, lat, ~ sf::st_point(x = c(.x, .y), dim = 'XY'))) %>%
#   mutate(geometry = sf::st_sfc(geometry, crs = 4326)) %>%
#   sf::st_sf()
# 
# bbox <- sf::st_bbox(wide_spatial_env)
# 
# 
# global_map <- rnaturalearth::ne_countries(scale = 110) %>% 
#   sf::st_as_sf() %>% 
#   sf::st_transform(crs = 4326 )
# 
# ggplot() +
#   geom_sf(data = wide_spatial_env, aes(color = value)) +
#   geom_sf(data = global_map, fill = "grey") +
#   scale_color_viridis_c()  + 
#   facet_wrap(~variable)


wide_spatial_env <- spatial_env %>%
  pivot_longer(cols = contains("env_"),
               names_to = "variable",
               values_to = "value",
               names_prefix = "env_") %>% 
  group_by(variable) %>% 
  nest() %>% 
  mutate(vargam = map(data, ~gamm4::gamm4(value ~ s(year) + s(lat,k = 5) + s(lon), data = na.omit(.x)))) %>% 
  mutate(pred_value = map2(vargam, data, ~ tibble(pred = predict(.x$gam, .y)))) %>% 
  select(-vargam) %>% 
  unnest(cols = c(data, pred_value)) %>% 
  ungroup() %>% 
  mutate(value = ifelse(is.na(value), pred,value)) %>% 
  select(-pred) %>% 
  mutate(combo = paste("lat",lat,"lon",lon, variable, sep = '_')) %>% 
  mutate(combo = str_replace_all(combo,"\\.","x")) %>% 
  select(-variable, -lat, -lon) %>% 
  pivot_wider(names_from = combo, values_from = value)

# 
# wide_spatial_env %>% 
#   filter(year > 2018) %>% 
#   group_by(lat,lon, variable) %>% 
#   summarise(m = mean(value, na.rm = TRUE)) %>% 
#   group_by(variable) %>% 
#   mutate(m = scale(m)) %>% 
#   ggplot(aes(lon, lat, fill = m)) + 
#   geom_raster() + 
#   facet_wrap(~variable)
#   

#
# upstr %>%
#   filter(year < 2018) %>%
#   ggplot(aes(year, mean_upstr)) +
#   geom_line()
data <- data %>%
  mutate(hit_the_water = brood_yr + fw_age) %>%
  left_join(pdo, by = c("hit_the_water" = "year")) %>%
  left_join(sst, by = c("hit_the_water" = "year")) %>%
  left_join(sea_level_pressure, by = c("hit_the_water" = "year")) %>%
  left_join(upstr, by = c("hit_the_water" = "year")) %>%
  # left_join(wide_spatial_env,  by = c("hit_the_water" = "year")) %>% 
  ungroup() %>%
  select(-hit_the_water)
# filter(!system %in% c("Nushagak", "Togiak","Igushik"))


# spawners <-
#   read_csv(here("Data", "Bristol Bay Spawner-Recruit Data.csv")) %>%
#   janitor::clean_names() %>%
#   select(system, brood_yr, spawn) %>%
#   rename(spawners = spawn) %>%
#   mutate(spawners = spawners / 1000)
#
# wtf <- spawners %>% filter(system == "Igushik",
#                            brood_yr == 2010)
#
# data <- data %>%
#   left_join(spawners, by = c("brood_yr" = "brood_yr", "system")) %>%
#   filter(!is.na(spawners))

top_systems <- data %>%
  group_by(age_group, system) %>%
  summarise(tr = sum(ret)) %>%
  group_by(age_group) %>%
  mutate(trrank = percent_rank(tr)) %>%
  ungroup()


# data <- data %>%
#   left_join(top_systems) %>%
#   filter(trrank > 0.75) %>%
#   select(-trrank,-tr)


# add in ruggerone data ---------------------------------------------------

rugg_salmon <- rugg_salmon %>%
  mutate(year = year + 1) %>%
  mutate(combo = paste("rugg", species, region, sep = '_')) %>%
  select(-species, -region) %>%
  # group_by(combo) %>%
  # mutate(returns = scale(returns)) %>%
  # ungroup() %>%
  na.omit() %>%
  spread(combo, returns)

data <- data %>%
  left_join(rugg_salmon, by = c("ret_yr" = "year"))

# write_csv(data, path = "bristol_bay_salmon_data.csv")

# explore data ------------------------------------------------------------

autocorr <- data %>%
  filter(ret_yr > 1963) %>%
  group_by(ret_yr) %>%
  summarise(tr = sum(ret)) %>% {
    acf(.$tr)
  }


# run leave-one-out time analysis -----------------------------------------



looframe <-
  tidyr::expand_grid(
    pred_system = if(by_system){unique(top_systems$system)}else {"all"},
    # dep_age = c("1.2","1.3"),
    dep_age = top_age_groups,
    test_year = first_year:last_year,
    model_type = c("rand_forest","boost_tree"),
    use_full_cohorts = c(TRUE),
    use_wide_cohorts = c(TRUE),
    use_years = c(TRUE),
    use_spatial_enviro = c(FALSE),
    factor_years = c(FALSE),
    log_returns = c(FALSE),
    assess = c(2),
    delta_returns = c(FALSE),
    omit_nas = c(FALSE)
  ) %>% 
  filter(!(log_returns == TRUE & delta_returns == TRUE))



future::plan(future::multiprocess, workers = cores)

if (fit_parsnip_models == TRUE){
  
  
        a <- Sys.time()
        set.seed(42)
        loo_preds <- looframe %>%
          ungroup() %>% 
          # filter(model_type == "boost_tree") %>%
          # sample_n(6) %>%
          mutate(pred = future_pmap(
            list(
            pred_system = pred_system,
            dep_age = dep_age,
            test_year = test_year,
            model_type = model_type,
            use_wide_cohorts = use_wide_cohorts,
            use_full_cohorts = use_full_cohorts,
            use_spatial_enviro = use_spatial_enviro,
            use_years = use_years,
            log_returns = log_returns,
            delta_returns = delta_returns,
            omit_nas = FALSE,
            assess = assess,
            factor_years = factor_years
            ),
            (fit_ml_salmon),
            data = data,
            scalar = scalar,
            freshwater_cohort = freshwater_cohort,
            weight = weight_returns,
            trees = trees,
            initial_prop = 0.8,
            forecast = FALSE,
            .progress = TRUE
          ))
        Sys.time() - a
      
  write_rds(loo_preds, path = file.path(results_dir, "parsnip_loo_preds.rds"))
  
} else {
  loo_preds <- readr::read_rds(file.path(results_dir, "parsnip_loo_preds.rds"))
}



# huh <- loo_preds %>% 
#   filter(model_type == "boost_tree",
#          dep_age == "2.2") %>% 
#   mutate(i = 1:nrow(.)) %>% 
#   mutate(best_params = map(pred, "best_params")) %>% 
#   select(-pred) %>% 
#   unnest(cols = best_params) %>% 
#   group_by(i) %>% 
#   filter(.estimate == min(.estimate))
# 
# hist(huh$learn_rate)





# run recurrent neural nets -----------------------------------------------

if (fit_rnn_models == TRUE) {
  message("starting neural nets")
  rnn_experiments <- purrr::cross_df(
    list(
      engineering = c("scale"),
      delta_dep = c(FALSE),
      form = c("cohort"),
      dep_age = top_age_groups,
      units = c(4,16,32),
      dropout = c(0.1,.5),
      batch_size = c(2,10)
    )
  )
  
  lookback <- str_split(rnn_experiments$dep_age,"\\.", simplify = TRUE) %>% 
    data.frame(stringsAsFactors = FALSE) %>% 
    map_df(as.numeric) %>% 
    rowSums() - 2
  
  rnn_experiments$lookback <- lookback
  

  # prepare data for each experiment
  rnn_experiments <- rnn_experiments %>%
    mutate(
      pdata = pmap(
        list(dep_age = dep_age,
             engineering = engineering,
             delta_dep = delta_dep),
        prepare_data,
        test_year = first_year,
        data = data,
        shuffle = FALSE,
        scalar = scalar,
        val_prop = 0.05,
        ages = "cohort"
      )
    )
  # fit each experiment
  rnn_experiments <- rnn_experiments %>%
    mutate(fit = pmap(
      list(
        prepped_data = pdata,
        lookback = lookback,
        units = units,
        batch_size = batch_size,
        dropout = dropout
      ),
      fit_rnn,
      epochs = 500,
      early_stop = FALSE,
      model = "rnn"
    ))
  
  # run predictions
  rnn_experiments <- rnn_experiments %>%
    mutate(predictions = pmap(
      list(
        prepped_data = pdata,
        fit = fit,
        return_delta = delta_dep
      ),
      predict_returns
    ))
  
  
  rnn_experiments <- rnn_experiments %>% 
    mutate(smoothed_val_loss =  map(fit, c("history", "metrics", "val_loss")) %>% map(~smooth(.x)))
  
  rnn_experiments <- rnn_experiments %>%
    mutate(val_loss = map_dbl(smoothed_val_loss,min)) %>%
    mutate(epochs =  map_dbl(smoothed_val_loss,~ last(which(.x == min(.x)))))
  
  
  write_rds(rnn_experiments, path = file.path(results_dir, "rnn_experiments.rds"))
  
# } else {
  # rnn_experiments <- readr::read_rds(file.path(results_dir, "rnn_experiments.rds"))
#   
# }
# select models -----------------------------------------------------------

message("finished rnn experiments")

# experiments$fit[[1]]$history %>%
#   plot()

# experiments %>%
#   ggplot(aes(val_loss)) +
#   geom_histogram() +
#   facet_grid(dep_age ~ delta_dep)


# select the "best" model configurations from the experiments in terms of
# hyperparameters, by both delta and absolute estimators, by RMSE in the validation period
best_rnn_models <- rnn_experiments %>%
  group_by(dep_age, delta_dep) %>%
  filter(val_loss == min(val_loss)) %>%
  select(-fit, -pdata,-smoothed_val_loss)

# best_models$delta_dep <- FALSE
# 
# best_models$units <- 64
# 
# best_models$epochs <- 1000
# 
# best_models$batch_size <- 1000

a = best_rnn_models %>%
  unnest(cols = predictions) %>%
  # filter(ret_yr < 2000) %>% 
  ggplot() +
  geom_point(aes(ret_yr, ret, color = data_use)) +
  geom_line(aes(ret_yr, pred, color = data_use)) +
  facet_wrap(dep_age ~ system , scales = "free_y")

# complete_preds %>%
#   ggplot() +
#   geom_point(aes(ret_yr, ret)) +
#   geom_line(aes(ret_yr, pred)) +
#   facet_wrap(~ system, scales = "free_y")


# fit best models -----------------------------------------------------------
# once the 'best' model is selected, go through and fit models 
# using the selected hyperparameters in a leave-one-out style (fit through 2000, predict 2001, fit through 2001, predict 2002, etc. )

rnn_looframe <- looframe %>% 
  select(dep_age, test_year) %>% 
  unique()

  rnn_loo_preds <- best_rnn_models %>%
    left_join(rnn_looframe, by = "dep_age") %>%
    mutate(
      prepped_data = pmap(
        list(
          dep_age = dep_age,
          engineering = engineering,
          test_year = test_year,
          delta_dep = delta_dep
        ),
        ages = "cohort",
        prepare_data,
        data = data,
        shuffle = FALSE,
        scalar = scalar,
        val_prop = 0
      )
    ) %>% 
    ungroup() %>% 
    mutate(fit_name = paste0("depage_",dep_age,"-","year_",test_year,"-delta_",delta_dep))
  
  
  rnn_loo_preds <- rnn_loo_preds %>%
    mutate(fit = pmap(
      list(
        prepped_data = prepped_data,
        lookback = lookback,
        units = units,
        epochs = 500,
        save_name = fit_name,
        batch_size = batch_size,
        dropout = dropout
      ),
      fit_rnn,
      save_dir = results_dir,
      save_fit = TRUE,
      early_stop = FALSE,
      model = "rnn",
      pred = FALSE
    ))
  
  
  # test <- load_model_hdf5(file.path(results_dir,"fits", "depage_2.2-year_2000-delta_FALSE.h5"rnn_loo_preds <- rnn_loo_preds %>%
  # mutate(predictions = pmap(
  #   list(
  #     prepped_data = prepped_data,
  #     fit = fit,
  #     return_delta = FALSE
  #   ),
  #   predict_returns
  # ))

write_rds(rnn_loo_preds, file.path(results_dir, "rnn_loo_preds.rds"))

} else{
  
  rnn_loo_preds <-
    read_rds(file.path(results_dir, "rnn_loo_preds.rds"))
  
}


# close fit rnn models
# loo_preds <- loo_preds %>%
#   filter(map_lgl(map(pred, "error"), is.null)) %>%
#   mutate(pred = map(pred,c("result","salmon_data")))



# proces rnn models -------------------------------------------------------



rolp <- rnn_loo_preds

rnn_loo_results <- rnn_loo_preds %>%
  select(-prepped_data, -fit) %>% 
  unnest(cols = predictions) %>% 
  mutate(pred = pmax(pred * scalar, 0),
         ret = ret * scalar) %>% 
  filter(ret_yr == test_year)

  
rnn_loo_results %>%
  ggplot(aes(ret, pred, color = data_use)) +
  geom_point() +
  geom_abline(aes(slope = 1, intercept = 0))

rnn_age_loo_results <- rnn_loo_results %>% 
  group_by(ret_yr, dep_age,data_use) %>% 
  summarise(observed = sum(ret),
            predicted = sum(pred))

rnn_sys_loo_results <- rnn_loo_results %>% 
  group_by(ret_yr,system,data_use) %>% 
  summarise(observed = sum(ret),
            predicted = sum(pred))

rnn_total_loo_results <- rnn_loo_results %>% 
  group_by(ret_yr,data_use) %>% 
  summarise(observed = sum(ret),
            predicted = sum(pred))

rnn_age_sys_loo_results <- rnn_loo_results %>%
  group_by(ret_yr, system, dep_age, data_use) %>%
  summarise(observed = sum(ret),
            predicted = sum(pred))


rnn_loo_results <- rnn_loo_results %>% 
  separate(age_group,c("fwa","oa"), sep =  "\\." , convert = TRUE, remove = TRUE) %>% 
  mutate(age_group = paste(fwa, oa, sep = "_")) %>% 
  rename(return_year = ret_yr,
         observed_returns = ret,
         predicted_returns = pred) %>% 
  mutate(age = fwa + oa + 1) %>% 
  mutate(brood_year = return_year - age) %>% 
  mutate(model = "rnn") %>% 
  select(model, brood_year, return_year, system, age_group, observed_returns, predicted_returns)

write_csv(rnn_loo_results, path = file.path(results_dir,"rnn_loo_results.csv"))



# proces parsnip models ---------------------------------------------------



olp <- loo_preds

loo_preds <- loo_preds %>%
  mutate(pred = map(pred,c("salmon_data")))

rids <- rlang::parse_exprs(colnames(looframe)[!colnames(looframe) %in% c("dep_age","test_year","pred_system")])

ogloo <- loo_preds

loo_results <- loo_preds %>% 
  unnest(cols = pred) %>% 
  filter(ret_yr == test_year) %>% 
  mutate(id = paste(!!!rids, sep ='-'),
         pred = pmax(0, pred))

run_ids <- rlang::parse_exprs(c("id",colnames(looframe)[!colnames(looframe) %in% c("dep_age","test_year", "pred_system")]))

# loo_results %>%
#   ggplot(aes(ret, pred, color = assess)) +
#   geom_point() +
#   geom_abline(aes(slope = 1, intercept = 0)) +
#   facet_grid(log_returns ~ delta_returns,
#              labeller = "label_both")

age_loo_results <- loo_results %>% 
  group_by(ret_yr, dep_age, !!!run_ids) %>% 
  summarise(observed = sum(ret),
            predicted = sum(pred))

sys_loo_results <- loo_results %>% 
  group_by(ret_yr,system, !!!run_ids) %>% 
  summarise(observed = sum(ret),
            predicted = sum(pred))

total_loo_results <- loo_results %>% 
  group_by(ret_yr, !!!run_ids) %>% 
  summarise(observed = sum(ret),
            predicted = sum(pred))

age_sys_loo_results <- loo_results %>% 
  group_by(ret_yr,system, dep_age, model_type,!!!run_ids) %>% 
  summarise(observed = sum(ret),
            predicted = sum(pred))



performance_summary <- age_sys_loo_results %>% 
  group_by(!!!run_ids) %>%
  yardstick::rmse(truth = observed, estimate = predicted) %>%
  arrange(.estimate)

performance_summary %>% 
  ggplot(aes(.estimate, fill = model_type)) + 
  geom_histogram()


# performance_summary <- total_loo_results %>%
#   group_by(id,model_type, log_returns, delta_returns, use_spatial_enviro) %>%
#   yardstick::rmse(truth = observed, estimate = predicted) %>%
#   arrange(.estimate)

best_performer <- performance_summary %>% 
  group_by(model_type) %>% 
  # filter(model_type == "boost_tree") %>%
  filter(.estimate == min(.estimate))

best_enviro_performer <- performance_summary %>% 
  group_by(use_spatial_enviro) %>% 
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
  filter(id %in% best_performer$id) %>% 
  ggplot() + 
  geom_col(dat = data, aes(ret_yr,ret / 1000)) + 
  geom_point(aes(ret_yr, predicted / 1000, color = "forecast", fill = model_type), shape = 21, size= 4, alpha = 0.85) +
  theme_minimal() + 
  scale_x_continuous(limits = c(first_year - 1, NA)) + 
  labs(x = "Year", y = "Bristol Bay Sockeye Returns (Million MT)",
       caption = "Bars are observed returns, points machine learning forecasts") + 
  theme(axis.text = element_text(size = 16)) +
  scale_color_discrete(name = '')

best_loo_preds <- loo_results %>% 
  filter(id %in% best_performer$id) %>% 
  group_by(dep_age) %>% 
  nest() %>% 
  ungroup()

looplot <- function(i,x,scalar){
  x %>% 
    ggplot() + 
    geom_col(aes(ret_yr, ret), position = "dodge") + 
    facet_wrap( ~ system, scales = "free_y") + 
    geom_point(aes(ret_yr, pred, fill = model_type), shape = 21, size= 4, alpha = 0.85) + 
    labs(title = paste0("Age Group: ",i), x = "Year", y = "Returns")
  
}

best_loo_preds <- best_loo_preds %>% 
  mutate(loo_plot = map2(dep_age,data, looplot, scalar = scalar))

# trelliscopejs::trelliscope(best_loo_preds, panel_col = "loo_plot", name = "loo")


best_loo_summary_plot <- best_loo_preds %>%
  select(-loo_plot) %>% 
  unnest(cols = data) %>% 
  group_by(dep_age, ret_yr, system, model_type) %>% 
  summarise(ret = sum(ret),
            pred = sum(pred)) %>% 
  ggplot(aes(ret,pred, color = model_type)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1) + 
  geom_smooth(method = "lm", color = "red") +
  facet_grid(dep_age ~ system, scales = "free")


best_loo_summary <- best_loo_preds %>%
  select(-loo_plot) %>% 
  unnest(cols = data) %>% 
  group_by(dep_age, ret_yr, system, model_type) %>% 
  summarise(ret = sum(ret),
            pred = sum(pred)) %>% 
  group_by(dep_age, system,model_type) %>% 
  summarise(rmse = yardstick::rmse_vec(ret, pred),
            mae = yardstick::mae_vec(ret, pred),
            rsq = yardstick::rsq_vec(ret, pred),
            ccc = yardstick::ccc_vec(ret, pred),
            median_bias = median((pred - ret), na.rm = TRUE)) %>% 
  ungroup() %>% 
  arrange(desc(rsq))


temp <- best_loo_preds %>%
  select(-loo_plot) %>%
  unnest(cols = data) %>%
  separate(
    dep_age,
    c("fwa", "oa"),
    sep =  "\\." ,
    convert = TRUE,
    remove = TRUE
  ) %>%
  mutate(age_group = paste(fwa, oa, sep = "_")) %>% 
  rename(
    model = model_type,
    return_year = ret_yr,
    observed_returns = ret,
    predicted_returns = pred
  ) %>%
  mutate(age = fwa + oa + 1) %>%
  mutate(brood_year = return_year - age) %>% 
  select(model, brood_year, return_year, system, age_group, observed_returns, predicted_returns)

# temp %>%
#   ggplot(aes(observed_returns, predicted_returns, color = "model_type")) +
#   geom_point()
readr::write_csv(temp, path = file.path(results_dir,"parsnip_loo_results.csv"))


# create naive models -----------------------------------------------------


observed <- data %>%
  select(ret_yr, system, age_group, ret) %>%
  rename(observed = ret) %>%
  filter(age_group %in% top_age_groups,
         system %in% unique(top_systems$system))
# 
observed %>%
  group_by(ret_yr) %>%
  summarise(o = sum(observed)) %>%
  ggplot(aes(ret_yr, o)) +
  geom_point()



simple_forecast <- observed %>%
  group_by(age_group, system) %>%
  arrange(ret_yr) %>%
  mutate(lag_forecast = observed,
         runmean_forecast = RcppRoll::roll_meanr(observed, 4)) %>%
  mutate(ret_yr = ret_yr + 1) %>%
  pivot_longer(
    cols = contains("_forecast"),
    names_to = "model",
    values_to = "pred"
  ) %>%
  select(ret_yr, system, age_group, model, observed, pred) %>%
  ungroup() %>% 
group_by(system, age_group, model) %>%
  arrange(ret_yr) %>%
  mutate(observed = lead(observed)) %>% 
  ungroup()


temp <- simple_forecast %>%
  separate(
    age_group,
    c("fwa", "oa"),
    sep =  "\\." ,
    convert = TRUE,
    remove = FALSE
  ) %>%
  mutate(age_group = paste(fwa, oa, sep = "_")) %>% 
  rename(
    model = model,
    return_year = ret_yr,
    observed_returns = observed,
    predicted_returns = pred
  ) %>%
  mutate(age = fwa + oa + 1) %>%
  mutate(brood_year = return_year - age) %>% 
  na.omit() %>% 
  select(model, brood_year, return_year, system, age_group, observed_returns, predicted_returns)


# temp %>% 
#   filter(return_year >= 2000,
#          model == "lag_forecast") %>% 
#   group_by(return_year, model) %>% 
#   summarise(observed = sum(observed_returns),
#             predicted = sum(predicted_returns)) %>% 
#   ggplot() + 
#   geom_col(aes(return_year, observed)) + 
#   geom_point(aes(return_year, predicted)) + 
#   facet_wrap(~model)

readr::write_csv(temp, path = file.path(results_dir,"benchmark_loo_results.csv"))

# run prediciton for next year --------------------------------------------



predframe <-
  tidyr::expand_grid(dep_age = top_age_groups,
                     test_year = last_year,
                     pred_system = unique(top_systems$system),
                     model_type = unique(best_performer$model_type))

if (run_next_forecast){

set.seed(42)
forecast_fit <- predframe %>%
  mutate(
    pred = future_pmap(
      list(dep_age = dep_age,
           test_year = test_year,
           pred_system = pred_system,
           model_type = model_type),
      fit_ml_salmon,
      use_wide_cohorts = best_performer$use_wide_cohorts[1],
      use_spatial_enviro = best_performer$use_spatial_enviro[1],
      use_full_cohorts = best_performer$use_full_cohorts[1],
      use_years = best_performer$use_years[1],
      log_returns = best_performer$log_returns[1],
      delta_returns = best_performer$delta_returns[1],
      omit_nas = best_performer$omit_nas[1],
      assess = best_performer$assess[1],
      factor_years = best_performer$factor_years[1],
      data = data,
      scalar = scalar,
      freshwater_cohort = freshwater_cohort,
      weight = weight_returns,
      trees = trees,
      produce = "fits",
      forecast = TRUE,
      .progress = TRUE
    )
  )



write_rds(forecast_fit, path = file.path(results_dir,"next_forecast.rds"))



} else {
  
  
  forecast_fit <-  read_rds(path = file.path(results_dir,"next_forecast.rds"))
  
  
}

next_forecast <- forecast_fit %>%
  mutate(pred = map(pred,"salmon_data")) %>%
  unnest(cols = pred) 

write_csv(next_forecast, path = file.path(results_dir,"next_loo_results.csv"))



next_forecast %>% 
  group_by(ret_yr, model_type) %>% 
  summarise(observed = sum(ret),
            predicted = sum(pred)) %>% 
  ggplot() + 
  geom_point(aes(ret_yr, observed)) + 
  geom_line(aes(ret_yr, predicted, color = model_type))


next_forecast %>% 
  group_by(ret_yr, dep_age,model_type) %>% 
  summarise(observed = sum(ret),
            predicted = sum(pred)) %>% 
  ggplot() + 
  geom_point(aes(ret_yr, observed)) + 
  geom_line(aes(ret_yr, predicted, color = model_type)) + 
  facet_wrap(~dep_age, scales = "free_y")


next_forecast %>% 
  filter(ret_yr >= 2000) %>% 
  group_by(ret_yr, system) %>% 
  summarise(observed = sum(ret),
            predicted = sum(pred)) %>% 
  ggplot() + 
  geom_point(aes(ret_yr, observed)) + 
  geom_line(aes(ret_yr, predicted), color = "red") + 
  facet_wrap(~system)


# process forecast --------------------------------------------------------


ml_forecast <- loo_results %>%
  # filter(id == best_performer$id) %>%
  select(ret_yr, system, dep_age, pred, model_type) %>%
  rename(forecast = pred,
         age_group = dep_age) 

# aggregate forecasts

forecasts <- ml_forecast %>% 
  left_join(observed, by = c("ret_yr","age_group","system")) %>% 
  na.omit() %>% 
  mutate(observed =  observed / 1000,
         forecast = forecast / 1000 )  %>% 
  rename(model = model_type)

total_forecast <- forecasts %>% 
  filter(ret_yr >= first_year) %>% 
  group_by(ret_yr, model) %>% 
  summarise(observed = sum(observed),
            forecast = sum(forecast))

age_forecast <- forecasts %>% 
  filter(ret_yr >= first_year) %>% 
  group_by(ret_yr,age_group,model) %>% 
  summarise(observed = sum(observed),
            forecast = sum(forecast))

system_forecast <- forecasts %>% 
  filter(ret_yr >= first_year) %>% 
  group_by(ret_yr,system,model) %>% 
  summarise(observed = sum(observed),
            forecast = sum(forecast))


age_system_forecast <- forecasts %>% 
  filter(ret_yr >= first_year) %>% 
  group_by(ret_yr,age_group,system,model) %>% 
  summarise(observed = sum(observed),
            forecast = sum(forecast))

total_next_forecast <-  next_forecast %>% 
  filter(ret_yr == max(ret_yr)) %>% 
  group_by(ret_yr) %>% 
  summarise(observed = sum(ret) / 1000, forecast = sum(pred) / 1000)

total_past_forecast <- forecasts %>% 
  bind_rows(total_next_forecast) %>% 
  group_by(ret_yr) %>% 
  summarise(observed = sum(observed),
            forecast = sum(forecast))

next_total_forecast_plot <- total_past_forecast %>%
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
  scale_y_continuous(name = "Returns (millions)", limits = c(0,75)) +
  scale_x_continuous(name = '', limits = c(NA, 2023)) 

next_total_forecast_plot

## -----------------------------------------------------------------------------

system_next_forecast <-  next_forecast %>% 
  filter(ret_yr == max(ret_yr)) %>% 
  group_by(ret_yr, system) %>% 
  summarise(observed = sum(ret) / 1000, forecast = sum(pred) / 1000)

total_past_forecast <- system_forecast %>% 
  bind_rows(system_next_forecast)

next_system_forecast_plot <-  total_past_forecast %>%
  ungroup() %>%
  mutate(label = ifelse(ret_yr == max(ret_yr), paste("2020 Forecast:", round(forecast),"Million"),'')) %>% 
  ggplot() +
  geom_col(aes(ret_yr, observed), alpha = 0.75) +
  geom_line(
    aes(ret_yr, forecast, color = ret_yr == max(ret_yr)),
    linetype = 2,
    show.legend = FALSE
  ) +
  geom_point(
    aes(ret_yr, forecast, fill = ret_yr == max(ret_yr)),
    size = 4,
    shape = 21,
    show.legend = FALSE
  ) +
  scale_y_continuous(name = "Returns (millions)") +
  scale_x_continuous(name = '') + 
  facet_wrap(~system, scales = "free_y")

next_system_forecast_plot


## -----------------------------------------------------------------------------

age_next_forecast <-  next_forecast %>% 
  filter(ret_yr == max(ret_yr)) %>% 
  group_by(ret_yr, dep_age) %>% 
  summarise(observed = sum(ret) / 1000, forecast = sum(pred) / 1000) %>% 
  rename(age_group = dep_age)

total_past_forecast <- age_forecast %>% 
  select(-model) %>% 
  bind_rows(age_next_forecast)

next_age_forecast_plot <- total_past_forecast %>%
  ungroup() %>%
  mutate(label = ifelse(ret_yr == max(ret_yr), paste("2020 Forecast:", round(forecast),"Million"),'')) %>% 
  ggplot() +
  geom_col(aes(ret_yr, observed), alpha = 0.75) +
  geom_line(
    aes(ret_yr, forecast, color = ret_yr == max(ret_yr)),
    linetype = 2,
    show.legend = FALSE
  ) +
  geom_point(
    aes(ret_yr, forecast, fill = ret_yr == max(ret_yr)),
    size = 4,
    shape = 21,
    show.legend = FALSE
  ) +
  scale_y_continuous(name = "Returns (millions)") +
  scale_x_continuous(name = '') + 
  facet_wrap(~age_group, scales = "free_y")

next_age_forecast_plot

## -----------------------------------------------------------------------------
retrospective_bias_plot <- complete_best_loo_preds %>% 
  group_by(ret_yr, test_year) %>% 
  summarise(ret = sum(ret) / 1000,
            pred = sum(pred) / 1000) %>% 
  ungroup() %>% 
  filter(test_year %% 5 == 0) %>% 
  ggplot() + 
  geom_col(aes(ret_yr, ret),alpha = 0.5) + 
  geom_line(aes(ret_yr, pred, color = ret_yr >= test_year),show.legend = FALSE,size = 1) + 
  geom_point(aes(ret_yr, pred, fill = ret_yr >= test_year),show.legend = FALSE,size = 4, shape = 21) + 
  scale_x_continuous(name = '') +
  scale_y_continuous(name = "Returns (millions)")+
  facet_wrap(~test_year) + 
  theme(axis.text = element_text(angle = 45, vjust = 0, hjust = 1, size = 10))

retrospective_bias_plot

## -----------------------------------------------------------------------------


age_system_performance <- age_system_forecast %>% 
  group_by(age_group, system, model) %>% 
  summarise(rmse = yardstick::rmse_vec(truth = observed, estimate = forecast),
            r2 = yardstick::rsq_vec(truth = observed, estimate = forecast),
            ccc = yardstick::ccc_vec(truth = observed, estimate = forecast),
            mape = yardstick::mape_vec(truth = observed, estimate = forecast),
            bias = mean(forecast - observed)) %>% 
  ungroup()


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
  group_by(model, system) %>% 
  summarise(rmse = yardstick::rmse_vec(truth = observed, estimate = forecast),
            r2 = yardstick::rsq_vec(truth = observed, estimate = forecast),
            ccc = yardstick::ccc_vec(truth = observed, estimate = forecast),
            mape = yardstick::mape_vec(truth = observed, estimate = forecast),
            bias = mean(forecast - observed)) %>% 
  ungroup() %>% 
  arrange(rmse)

age_performance <- age_forecast %>% 
  group_by(model, age_group) %>% 
  summarise(rmse = yardstick::rmse_vec(truth = observed, estimate = forecast),
            r2 = yardstick::rsq_vec(truth = observed, estimate = forecast),
            ccc = yardstick::ccc_vec(truth = observed, estimate = forecast),
            mape = yardstick::mape_vec(truth = observed, estimate = forecast),
            bias = mean(forecast - observed)) %>% 
  ungroup() %>% 
  arrange(rmse)


total_performance %>%
  arrange(rmse) %>% 
  gt() %>%
  fmt_number(columns = which(colnames(.) != "model"),
             decimals = 2)

## -----------------------------------------------------------------------------


age_performance %>%
  group_by(age_group) %>% 
  arrange(rmse) %>% 
  gt() %>%
  fmt_number(columns = which(map_lgl(., is.numeric)),
             decimals = 2)



## -----------------------------------------------------------------------------


system_performance %>%
  group_by(system) %>% 
  arrange(rmse) %>% 
  gt() %>%
  fmt_number(columns = which(map_lgl(., is.numeric)),
             decimals = 2)



# make forecast table -----------------------------------------------------




raw_forecast_table <- next_forecast %>% 
  ungroup() %>% 
  filter(ret_yr == max(ret_yr)) %>% 
  rename(forecast = pred,
         age_group = dep_age) %>% 
  ungroup() %>% 
  mutate(
    age_group= forcats::fct_relevel(age_group, c("1.2","1.3","2.2","2.3"))) %>% 
  select(ret_yr, system, age_group, forecast, model_type) %>% 
  bind_rows(ml_forecast) %>% 
  group_by(system, ret_yr, model_type) %>%
  mutate(forecast = forecast * 1000) %>% 
  mutate(Totals = sum(forecast)) %>%
  ungroup() %>% 
  arrange(ret_yr, age_group) %>% 
  pivot_wider(names_from = age_group, values_from = forecast) %>% 
  select(dplyr::everything(),-Totals, Totals) %>% 
  arrange(desc(ret_yr))

raw_forecast_table %>% 
  pivot_longer(contains("."), names_to = "age_group", values_to = "forecast") %>% 
  group_by(ret_yr, model_type) %>% 
  summarise(total_forecast = sum(forecast)) %>% 
  ungroup() %>% 
  ggplot(aes(ret_yr, total_forecast, color  = model_type)) + 
  geom_line()

write_csv(raw_forecast_table %>% mutate_if(is.numeric,round), "raw-machine-learning-forecast-table.csv")

total_vars <- colnames(raw_forecast_table)

total_vars <- total_vars[str_detect(total_vars,"(\\.)|(Totals)")]

forecast_table <- raw_forecast_table %>%
  group_by(ret_yr, model_type) %>%
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

forecast_table
# gt::gtsave(forecast_table,"ml-forecast-table.png", zoom = 10, expand = 10)
# 
# gt::gtsave(forecast_table,"ml-forecast-table.tex")





