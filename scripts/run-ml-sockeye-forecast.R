#########################
### mechasalmon ########
########################
# author: Dan Ovando
# purpose: develop and test machine learning approaches to salmon forecasting
#


# load ------------------------------------------------------------------------

functions <- list.files(here::here("functions"))

purrr::walk(functions, ~ source(here::here("functions", .x)))

prep_run(results_name = "ml_test", results_description = "testing machine learning")

if (!dir.exists(file.path(results_dir,"figs"))) {
  
  dir.create(file.path(results_dir, "figs"), recursive = TRUE)
  
  dir.create(file.path(results_dir, "fits"), recursive = TRUE)
  
}


#aha,

# https://stackoverflow.com/questions/9944246/setting-defaults-for-geoms-and-scales-ggplot2?rq=1
# scale_colour_discrete <-
#   function(...)
#     scale_colour_brewer(..., palette = "Set2")
# scale_fill_discrete <-
#   function(...)
#     scale_fill_brewer(... , palette = "Set2")

# set options -------------------------------------------------------------

fit_parsnip_models <- TRUE

fit_rnn_models <- FALSE

run_query_erddap <-  TRUE

run_next_forecast <- TRUE

stride <- 4 #stride for errdaap data

weight_returns <- FALSE

cores <- 8

trees <- 500

freshwater_cohort <- TRUE #leave as true

last_year <- 2019 # the final year in the data

min_year <- 1965 # only include data greater than or equal this year

age_groups <- 4 #number of top age groups to include

first_year <- 2000 # the first year splitting the test and training data

scalar <- 10000
  #10000 # number do divide the total returns by just to get the data a bit closer to the -1 to 1 range

min_lat <-  52

max_lat <- 60

min_lon <- -178

max_lon <- -156

min_year <- 1960

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
    min_year = min_year,
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

sst <- erddap$esrlIcoads1ge$data %>%
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
  left_join(wide_spatial_env,  by = c("hit_the_water" = "year")) %>% 
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

# explore data ------------------------------------------------------------
#
# data %>%
#   group_by(ret_yr, age_group) %>%
#   summarise(tr = sum(ret)) %>%
#   group_by(age_group) %>%
#   mutate(str = scale(tr)) %>%
#   ggplot(aes(ret_yr, str, color = age_group)) +
#   geom_line(show.legend = FALSE) +
#   facet_wrap(~ age_group) +
# theme_minimal()

# data %>%
#   group_by(ret_yr) %>%
#   summarise(tr = sum(ret)) %>%
#   ungroup() %>%
#   ggplot(aes(ret_yr, tr)) +
#   geom_line()

#
# data %>%
#   filter(ret_yr > 1963) %>%
#   group_by(ret_yr, system) %>%
#   summarise(tr = sum(ret)) %>%
#   group_by(system) %>%
#   mutate(rolling_mean = RcppRoll::roll_mean(tr, 20, fill = NA, align = "center")) %>%
#   ungroup() %>%
#   ggplot(aes(ret_yr, rolling_mean)) +
#   geom_line() +
#   facet_wrap( ~ system, scales = "free_y")
#
# data %>%
#   filter(ret_yr > 1963) %>%
#   group_by(ret_yr, system) %>%
#   summarise(tr = sum(ret)) %>%
#   group_by(system) %>%
#   mutate(delta_tr = tr - lag(tr)) %>%
#   ungroup() %>%
#   ggplot(aes(ret_yr, delta_tr)) +
#   geom_line() +
#   facet_wrap( ~ system, scales = "free_y")

#
# data %>%
#   filter(ret_yr > 1963) %>%
#   group_by(ret_yr) %>%
#   summarise(tr = sum(ret)) %>%
#   ungroup() %>%
#   mutate(delta_tr = tr - lag(tr)) %>%
#   mutate(rolling_mean = RcppRoll::roll_mean(delta_tr, 20, fill = NA, align = "center")) %>%
#   ggplot(aes(ret_yr, rolling_mean)) +
#   geom_line()


autocorr <- data %>%
  filter(ret_yr > 1963) %>%
  group_by(ret_yr) %>%
  summarise(tr = sum(ret)) %>% {
    acf(.$tr)
  }


# run leave-one-out time analysis -----------------------------------------

looframe <-
  tidyr::expand_grid(
    dep_age = top_age_groups,
    test_year = first_year:last_year,
    model_type = c("rand_forest","boost_tree"),
    use_full_cohorts = c(FALSE,TRUE),
    use_wide_cohorts = c(FALSE,TRUE),
    use_years = c(TRUE),
    use_spatial_enviro = c(FALSE),
    factor_years = c(FALSE,TRUE),
    log_returns = c(FALSE),
    assess = c(1),
    delta_returns = c(FALSE),
    omit_nas = c(TRUE)
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
            dep_age = dep_age,
            test_year = test_year,
            model_type = model_type,
            use_wide_cohorts = use_wide_cohorts,
            use_full_cohorts = use_full_cohorts,
            use_spatial_enviro = use_spatial_enviro,
            use_years = use_years,
            log_returns = log_returns,
            delta_returns = delta_returns,
            omit_nas = omit_nas,
            assess = assess,
            factor_years = factor_years
            ),
            (fit_salmon),
            data = data,
            scalar = scalar,
            freshwater_cohort = freshwater_cohort,
            weight = weight_returns,
            trees = trees,
            n_mtry = 5,
            forecast = FALSE,
            .progress = TRUE
          ))
        Sys.time() - a
      
  write_rds(loo_preds, path = file.path(results_dir, "parsnip_loo_preds.rds"))
  
} else {
  loo_preds <- readr::read_rds(file.path(results_dir, "parsnip_loo_preds.rds"))
}




# run recurrent neural nets -----------------------------------------------

if (fit_rnn_models == TRUE) {
  
  rnn_experiments <- purrr::cross_df(
    list(
      engineering = c("scale"),
      delta_dep = c(FALSE),
      form = c("cohort"),
      dep_age = top_age_groups,
      units = c(4,16,32),
      dropout = c(0.1, 0.5),
      batch_size = c(2,50)
    )
  )
  
  lookback <- str_split(rnn_experiments$dep_age,"\\.", simplify = TRUE) %>% 
    data.frame(stringsAsFactors = FALSE) %>% 
    map_df(as.numeric) %>% 
    rowSums() - 2
  
  rnn_experiments$lookback <- lookback
  
  # experiments <- purrr::cross_df(
  #   list(
  #     engineering = c("range", 'yeo_johnson', 'scale'),
  #     delta_dep = c(FALSE, TRUE),
  #     form = c("age"),
  #     dep_age = top_age_groups,
  #     lookback = c(1),
  #     units = c(64)
  #   )
  # )
  
  
  # set up potential experiments
  # experiments <- purrr::cross_df(
  #   list(
  #     engineering = c('scale'),
  #     delta_dep = c(TRUE),
  #     form = c("age"),
  #     dep_age = top_age_groups,
  #     lookback = c(3),
  #     units = c(16)
  #   )
  # )
  
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
#   rnn_experiments <- readr::read_rds(file.path(results_dir, "rnn_experiments.rds"))
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

  rnn_looframe <-
    purrr::cross_df(list(
      dep_age = unique(best_rnn_models$dep_age),
      test_year = first_year:last_year
    ))
  
  rnn_loo_preds <- best_rnn_models %>%
    left_join(looframe, by = "dep_age") %>%
    # filter(delta_dep == FALSE) %>% 
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
        epochs = epochs,
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
  
  
  # test <- load_model_hdf5(file.path(results_dir,"fits", "depage_2.2-year_2000-delta_FALSE.h5"))
rnn_loo_preds <- rnn_loo_preds %>%
  mutate(predictions = pmap(
    list(
      prepped_data = prepped_data,
      fit = fit,
      return_delta = FALSE
    ),
    predict_returns
  ))

write_rds(rnn_loo_preds, file.path(results_dir, "rnn_loo_preds.rds"))

} else{
  
  rnn_loo_preds <- read_rds(file.path(results_dir, "rnn_loo_preds.rds"))

}


# close fit rnn models
# loo_preds <- loo_preds %>%
#   filter(map_lgl(map(pred, "error"), is.null)) %>%
#   mutate(pred = map(pred,c("result","salmon_data")))
# process results --------------------------------------------

olp <- loo_preds

# loo_preds <- olp

# a = loo_preds %>% 
#   filter(model_type == "boost_tree")

loo_preds <- loo_preds %>%
  # filter(map_lgl(map(pred, "error"), is.null)) %>%
  mutate(pred = map(pred,c("salmon_data")))


rids <- rlang::parse_exprs(colnames(looframe)[!colnames(looframe) %in% c("dep_age","test_year")])

ogloo <- loo_preds

loo_results <- loo_preds %>% 
  unnest(cols = pred) %>% 
  filter(ret_yr == test_year) %>% 
  mutate(id = paste(!!!rids, sep ='-'),
         pred = pmax(0, pred))

run_ids <- rlang::parse_exprs(c("id",colnames(looframe)[!colnames(looframe) %in% c("dep_age","test_year")]))



loo_results %>%
  ggplot(aes(ret, pred, color = assess)) +
  geom_point() +
  geom_abline(aes(slope = 1, intercept = 0)) +
  facet_grid(log_returns ~ delta_returns,
             labeller = "label_both")

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


total_loo_results %>% 
  ggplot() + 
  geom_col(dat = data, aes(ret_yr,ret)) + 
  geom_point(aes(ret_yr, predicted, fill = use_wide_cohorts), shape = 21, size= 2, alpha = 0.85) +
  theme_minimal() + 
  scale_x_continuous(limits = c(first_year - 1, NA))

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
  filter(id == best_performer$id) %>% 
  ggplot() + 
  geom_col(dat = data, aes(ret_yr,ret / 1000)) + 
  geom_point(aes(ret_yr, predicted / 1000, color = "forecast"), shape = 21, size= 4,fill = "tomato", alpha = 0.85) +
  theme_minimal() + 
  scale_x_continuous(limits = c(first_year - 1, NA)) + 
  labs(x = "Year", y = "Bristol Bay Sockeye Returns (Million MT)",
       caption = "Bars are observed returns, points machine learning forecasts") + 
  theme(axis.text = element_text(size = 16)) +
  scale_color_discrete(name = '')

best_loo_preds <- loo_results %>% 
  filter(id == best_performer$id) %>% 
  group_by(dep_age) %>% 
  nest() %>% 
  ungroup()

looplot <- function(i,x,scalar){
  x %>% 
    ggplot() + 
    geom_col(aes(ret_yr, ret)) + 
    facet_wrap( ~ system, scales = "free_y") + 
    geom_point(aes(ret_yr, pred), shape = 21, size= 4, fill = "red", alpha = 0.85) + 
    labs(title = paste0("Age Group: ",i), x = "Year", y = "Returns")
  
}

best_loo_preds <- best_loo_preds %>% 
  mutate(loo_plot = map2(dep_age,data, looplot, scalar = scalar))

# trelliscopejs::trelliscope(best_loo_preds, panel_col = "loo_plot", name = "loo")

best_loo_summary <- best_loo_preds %>%
  select(-loo_plot) %>% 
  unnest(cols = data) %>% 
  mutate(dep_age = str_replace_all(dep_age, "\\.", "_")) %>%
  mutate(unq = paste(system, dep_age),
         pred = pred) %>%
  rename(year = ret_yr, ml_pred = pred) %>%
  select(year, unq, ml_pred)


best_loo_summary_plot <- best_loo_preds %>%
  select(-loo_plot) %>% 
  unnest(cols = data) %>% 
  group_by(dep_age, ret_yr, system) %>% 
  summarise(ret = sum(ret),
            pred = sum(pred)) %>% 
  ggplot(aes(ret,pred)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1) + 
  geom_smooth(method = "lm", color = "red") +
  facet_grid(dep_age ~ system, scales = "free")


best_loo_summary <- best_loo_preds %>%
  select(-loo_plot) %>% 
  unnest(cols = data) %>% 
  group_by(dep_age, ret_yr, system) %>% 
  summarise(ret = sum(ret),
            pred = sum(pred)) %>% 
  group_by(dep_age, system) %>% 
  summarise(rmse = yardstick::rmse_vec(ret, pred),
            mae = yardstick::mae_vec(ret, pred),
            rsq = yardstick::rsq_vec(ret, pred),
            ccc = yardstick::ccc_vec(ret, pred),
            median_bias = median((pred - ret), na.rm = TRUE)) %>% 
  ungroup() %>% 
  arrange(desc(rsq))


readr::write_rds(best_loo_preds, path = file.path(results_dir,"best_loo_preds.rds"))

# weightname <- ifelse(weight_returns, "weighted","unweighted")

# readr::write_rds(forest_pred, path = file.path(results_dir, paste0(weightname,"_forest_preds.rds")))



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
  tidyr::expand_grid(dep_age = top_age_groups,
                     test_year = last_year)

if (run_next_forecast){

set.seed(42)
forecast_fit <- predframe %>%
  mutate(
    pred = future_pmap(
      list(dep_age = dep_age,
           test_year = test_year),
      fit_salmon,
      model_type = best_performer$model_type,
      use_wide_cohorts = best_performer$use_wide_cohorts,
      use_spatial_enviro = best_performer$use_spatial_enviro,
      use_full_cohorts = best_performer$use_full_cohorts,
      use_years = best_performer$use_years,
      log_returns = best_performer$log_returns,
      delta_returns = best_performer$delta_returns,
      omit_nas = best_performer$omit_nas,
      assess = best_performer$assess,
      factor_years = best_performer$factor_years,
      data = data,
      scalar = scalar,
      freshwater_cohort = freshwater_cohort,
      weight = weight_returns,
      trees = trees,
      n_mtry = 5,
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


next_forecast %>% 
  group_by(ret_yr) %>% 
  summarise(observed = sum(ret),
            predicted = sum(pred)) %>% 
  ggplot() + 
  geom_point(aes(ret_yr, observed)) + 
  geom_line(aes(ret_yr, predicted), color = "red")


next_forecast %>% 
  group_by(ret_yr, dep_age) %>% 
  summarise(observed = sum(ret),
            predicted = sum(pred)) %>% 
  ggplot() + 
  geom_point(aes(ret_yr, observed)) + 
  geom_line(aes(ret_yr, predicted), color = "red") + 
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


# compare predictions -----------------------------------------------------



# pull out "truth"

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


# load in fri forecasts

# source(here("R","get-published-fcst.R"))  # Returns published UW-FRI Forecasts by year
# source(here("R","get-currys-1step-preds.R")) # Reads in 1-step ahead predictions from all UW-FRI current model types
# 
# # Published UW-FRI preseason forecasts
# published.fcsts <- get_published_fcst(dir.pf=here(file.path("data","preseasonForecast.dat")),
#                                       dir.ids=here(file.path("data","ID_Systems.csv")), 
#                                       years=2000:last_year) %>% 
#   rename(forecast = FRIfcst) %>% 
#   mutate(model = "fri_forecast")
# 
# 
# # UW-FRI DLM Forecast 1963+ Linear
# dlm.63.linear.fcsts <- get_currys_1step_preds(dir.retro.output=file.path("Retrospective-Analysis","Output", "Group"),
#                                               name.output="group.dlm_marss.rds",
#                                               transform="linear",
#                                               start.year=1963) %>% 
#   rename(forecast = fcst) %>% 
#   mutate(model = "dlm_forecast")

# aggregate fri forecasts

# alternative_forecasts <- published.fcsts %>%
#   bind_rows(dlm.63.linear.fcsts[, colnames(dlm.63.linear.fcsts) %in% colnames(published.fcsts)]) %>%
#   janitor::clean_names() %>%
#   mutate(age_group = paste(fw_age, o_age, sep = '.')) %>%
#   filter(age_group %in% top_age_groups,
#          system %in% unique(top_systems$system)) %>% 
#   select(ret_yr, system, age_group, model, forecast)

# create naive forecasts

simple_forecast <- observed %>%
  group_by(age_group, system) %>%
  arrange(ret_yr) %>%
  mutate(
    lag_forecast = observed,
    runmean_forecast = RcppRoll::roll_meanr(observed, 4)
  ) %>% 
  mutate(ret_yr = ret_yr + 1) %>% 
  pivot_longer(cols = contains("_forecast"), names_to = "model", values_to = "forecast") %>% 
  select(ret_yr, system, age_group, model, forecast)


# process ml forecast

ml_forecast <- loo_results %>%
  filter(id == best_performer$id) %>%
  select(ret_yr, system, dep_age, pred) %>%
  rename(forecast = pred,
         age_group = dep_age) %>%
  mutate(model = "ml_forecast", ret_yr = ret_yr)

# aggregate forecasts

forecasts <- ml_forecast %>% 
  bind_rows(simple_forecast) %>% 
  # bind_rows(alternative_forecasts) %>% 
  left_join(observed, by = c("ret_yr","age_group","system")) %>% 
  na.omit() %>% 
  mutate(observed =  observed / 1000,
         forecast = forecast / 1000 ) %>% 
  mutate(model = str_replace(model, "_forecast",""))

# create performance summaries

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



total_forecast_plot <- total_forecast %>% 
  ggplot() + 
  geom_point(aes(ret_yr, observed),size = 2) + 
  geom_line(aes(ret_yr, forecast, color = model), show.legend = FALSE) + 
  facet_wrap(~model)

total_forecast_plot

age_forecast_plot <- age_forecast %>% 
  ggplot() + 
  geom_point(aes(ret_yr, observed),size = 2) + 
  geom_line(aes(ret_yr, forecast, color = model), show.legend = FALSE) + 
  facet_grid(age_group~model, scales = "free_y")

age_forecast_plot


system_forecast_plot <- system_forecast %>% 
  ggplot() + 
  geom_point(aes(ret_yr, observed),size = 2) + 
  geom_line(aes(ret_yr, forecast, color = model), show.legend = FALSE) + 
  facet_grid(system~model, scales = "free_y")

system_forecast_plot


age_system_forecast %>% 
  filter(system == "Kvichak", 
         model == "ml") %>% 
  ggplot() +
  geom_point(aes(ret_yr, observed)) +
  geom_line(aes(ret_yr, forecast)) + 
  facet_wrap(~age_group)

# evaluate performance

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


total_performance_plot <- total_performance %>% 
  ggplot(aes(reorder(model, rmse), rmse)) + 
  geom_col()

total_performance_plot

age_performance_plot <- age_performance %>% 
  group_by(age_group) %>% 
  ggplot(aes(reorder(model, rmse),fill = model == "ml", rmse)) + 
  geom_col() + 
  facet_wrap(~age_group)

age_performance_plot


system_performance_plot <- system_performance %>% 
  group_by(system) %>% 
  ggplot(aes(reorder(model, rmse),fill = model == "ml", rmse)) + 
  geom_col() + 
  facet_wrap(~system)

system_performance_plot


age_system_performance_plot <-  age_system_performance %>% 
  group_by(age_group, system) %>% 
  mutate(ml_improvement = (rmse[model == "fri"] - rmse[model == "ml"]) /  rmse[model == "fri"],
         ref_rmse =rmse[model == "fri"],
         sd_rmse = sd(rmse)) %>% 
  filter(rmse == min(rmse))  %>%
  ungroup() %>% 
  mutate(scaled_rmse = -(ref_rmse - rmse) / ref_rmse,
         fface = ifelse(model == "ml", "italic","plain")) %>% 
  ggplot(aes(system, age_group, label = model,color = scaled_rmse)) + 
  geom_text(size = 7, aes(fontface = fface)) + 
  theme_bw() + 
  scale_color_gradient(low = "tomato", high = "steelblue", 
                       labels = scales::percent,
                       name = "% Reduction in RMSE",
                       limits = c(-.75,0), 
                       breaks = seq(-.75,0, by = 0.25))

age_system_performance_plot


# save things -------------------------------------------------------------



performance_files <- ls()[str_detect(ls(),"_performance")]

forecast_files <- ls()[str_detect(ls(),"forecast")]

loo_files <- ls()[str_detect(ls(),"_loo")]

env_files <-  ls()[str_detect(ls(),"env")]

save(list = performance_files,file = file.path(results_dir,"performance_files.RData"))

save(list = loo_files,file = file.path(results_dir,"loo_files.RData"))

save(list = forecast_files,file = file.path(results_dir,"forecast_files.RData"))

save(list = env_files,file = file.path(results_dir,"env_files.RData"))

salmon_data <- data

save(salmon_data, file = file.path(results_dir,"salmon_data.RData"))


# make plots --------------------------------------------------------------


salmon_data <- salmon_data %>% 
  filter(age_group %in% top_age_groups) %>% 
  mutate(ret = ret/ 1000)



## -----------------------------------------------------------------------------

return_plot <- salmon_data %>% 
  group_by(ret_yr) %>% 
  summarise(ret = sum(ret)) %>% 
  ggplot(aes(ret_yr, ret)) + 
  geom_col(alpha = 0.75) + 
  scale_y_continuous(name = "Returns (millions)") + 
  scale_x_continuous(name = '') 

return_plot



## ----age-sys-return-plot------------------------------------------------------

age_sys_return_plot <- salmon_data %>% 
  ggplot(aes(ret_yr, ret, fill = age_group)) + 
  geom_col(alpha = 0.75) + 
  facet_wrap(~system, scales = "free_y") + 
  scale_fill_viridis_d(name = "Age Group") + 
  scale_y_continuous(name = "Returns (millions)") + 
  scale_x_continuous(name = '') +
  theme(legend.position = "top") 

age_sys_return_plot


## ----ml-pred-plot-------------------------------------------------------------

ml_pred_plot <- total_forecast %>% 
  filter(model == "ml") %>% 
  ggplot() + 
  geom_col(aes(ret_yr, observed),alpha = 0.75) + 
  geom_line(aes(ret_yr, forecast),color = "tomato", linetype = 2) +
  geom_point(aes(ret_yr, forecast),fill = "tomato", size = 4, shape = 21) +
  scale_y_continuous(name = "Returns (millions)") + 
  scale_x_continuous(name = '')  + 
  labs(caption = "Red points are forecasts")

ml_pred_plot



## ----all-pred-plot------------------------------------------------------------
all_pred_plot <- total_forecast %>% 
  ggplot() + 
  geom_col(aes(ret_yr, observed),alpha = 0.75) + 
  geom_line(aes(ret_yr, forecast, color = model), linetype = 2) +
  geom_point(aes(ret_yr, forecast, fill = model), size = 4,shape = 21, alpha = 0.75) +
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
  geom_col(aes(ret_yr, observed),alpha = 0.75) +
  geom_line(aes(ret_yr, forecast, color = model), linetype = 1, size = 1) +
  geom_point(aes(ret_yr, forecast, fill = model), size = 4,shape = 21) +
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
  geom_col(aes(ret_yr, observed), position = "dodge",alpha = 0.75) +
  geom_line(aes(ret_yr, forecast, color = model), linetype = 1, size = 1) +
  # geom_point(aes(ret_yr, forecast, fill = model), size = 4,shape = 21) +
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
  mutate(ml_improvement = (rmse[model == "fri"] - rmse[model == "ml"]) /  rmse[model == "fri"],
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
                       name = "% Change in Error",
                       limits = c(-.75,0), 
                       breaks = seq(-.75,0, by = 0.25),
                       guide = guide_colorbar(ticks.colour = "black",frame.colour = "black",barwidth = unit(15, units = "lines")))+ 
  scale_x_discrete(name = '') + 
  scale_y_discrete(name = '')+
  scale_size(range = c(4,12), guide = FALSE) +
  theme(legend.position = "top")

age_system_performance_plot



## -----------------------------------------------------------------------------

naive_ensemble <- age_system_performance %>%
  filter(model != "fri") %>% 
  group_by(age_group, system) %>%
  filter(rmse == min(rmse)) %>%
  select(age_group, system, model) %>%
  rename(best_model = model)

naive_ensemble_forecasts <- forecasts %>%
  left_join(naive_ensemble, by = c("age_group", "system")) %>%
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
  scale_y_continuous(name = "Returns (millions)") +
  scale_x_continuous(name = '')  +
  labs(caption = "Red points are naive-ensemble based forecasts")

naive_ensemble_forecasts_plot



## ----include=TRUE-------------------------------------------------------------
system_naive_ensemble_forecasts_plot <-  naive_ensemble_forecasts %>%
  group_by(ret_yr, system) %>%
  summarise(observed = sum(observed),
            forecast = sum(forecast)) %>%
  ungroup() %>%
  filter(ret_yr >= 2000) %>%
  ggplot() +
  geom_col(aes(ret_yr, observed), alpha = 0.75) +
  geom_line(aes(ret_yr, forecast), color = "tomato", linetype = 2, size = 1) +
  geom_point(aes(ret_yr, forecast), fill = "tomato", size = 4, shape = 21) +
  facet_wrap(~system, scale = "free_y") +
  scale_y_continuous(name = "Returns (millions)") +
  scale_x_continuous(name = '')  +
  labs(caption = "Red points are naive-ensemble based forecasts")

system_naive_ensemble_forecasts_plot





## -----------------------------------------------------------------------------

total_next_forecast <-  next_forecast %>% 
  filter(ret_yr == max(ret_yr)) %>% 
  group_by(ret_yr) %>% 
  summarise(observed = sum(ret) / 1000, forecast = sum(pred) / 1000)

total_past_forecast <- total_forecast %>% 
  filter(model == "ml") %>% 
  select(-model) %>% 
  bind_rows(total_next_forecast)

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
  filter(model == "ml") %>% 
  select(-model) %>% 
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

next_total_forecast_plot


## -----------------------------------------------------------------------------

age_next_forecast <-  next_forecast %>% 
  filter(ret_yr == max(ret_yr)) %>% 
  group_by(ret_yr, dep_age) %>% 
  summarise(observed = sum(ret) / 1000, forecast = sum(pred) / 1000) %>% 
  rename(age_group = dep_age)

total_past_forecast <- age_forecast %>% 
  filter(model == "ml") %>% 
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


total_performance %>%
  arrange(rmse) %>% 
  gt() %>%
  fmt_number(columns = which(colnames(.) != "model"),
             decimals = 2)



## -----------------------------------------------------------------------------


age_performance %>%
  filter(model %in% c("fri","ml")) %>% 
  group_by(age_group) %>% 
  arrange(rmse) %>% 
  gt() %>%
  fmt_number(columns = which(map_lgl(., is.numeric)),
             decimals = 2)



## -----------------------------------------------------------------------------


system_performance %>%
  filter(model %in% c("fri","ml")) %>% 
  group_by(system) %>% 
  arrange(rmse) %>% 
  gt() %>%
  fmt_number(columns = which(map_lgl(., is.numeric)),
             decimals = 2)



# make forecast table -----------------------------------------------------



raw_forecast_table <- next_forecast %>% 
  filter(ret_yr == max(ret_yr)) %>% 
  rename(forecast = pred,
         age_group = dep_age) %>% 
  ungroup() %>% 
  mutate(
         age_group= forcats::fct_relevel(age_group, c("1.2","1.3","2.2","2.3"))) %>% 
  select(ret_yr, system, age_group, forecast) %>% 
  bind_rows(ml_forecast %>% select(-model)) %>% 
  group_by(system, ret_yr) %>%
  mutate(forecast = forecast * 1000) %>% 
  mutate(Totals = sum(forecast)) %>%
  ungroup() %>% 
  arrange(ret_yr, age_group) %>% 
  pivot_wider(names_from = age_group, values_from = forecast) %>% 
  select(dplyr::everything(),-Totals, Totals)

write_excel_csv(raw_forecast_table %>% mutate_if(is.numeric,round), "raw-machine-learning-forecast-table.csv")

total_vars <- colnames(raw_forecast_table)

total_vars <- total_vars[str_detect(total_vars,"(\\.)|(Totals)")]

forecast_table <- raw_forecast_table %>%
  group_by(ret_yr) %>%
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
                     cells_data(columns = vars(Totals)))
  )

gt::gtsave(forecast_table,"ml-forecast-table.png", zoom = 10, expand = 10)

gt::gtsave(forecast_table,"ml-forecast-table.tex")


readr::write_csv(forecast_table, "test.csv")



# repeat but over time



## -----------------------------------------------------------------------------

plots <- ls()[str_detect(ls(), "_plot")]

path <- results_dir

plotfoo <- function(x,height = 6, width = 9 , device = "pdf",path){
  
  ggsave(filename = file.path(path,"figs",paste(x,device, sep = '.')),plot = get(x),height = height, width = width)
  
}

walk(plots, plotfoo, path = results_dir)


