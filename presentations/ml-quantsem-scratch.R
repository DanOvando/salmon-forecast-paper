library(tidymodels)
library(tidyverse)
library(ranger)
library(xgboost)
library(neuralnet)
library(here)
library(rstanarm)
library(vip)
library(ggthemes)
library(patchwork)
library(pdp)
min_years_catch <- 25

min_years_catch <- 25 # minimum years of catch data to include

crazy_b <- 5 # maximum B/Bmsy to allow

crazy_u <- 10 # maximum U/Umsy to allow

catchability <- 1e-3 # survey catchability


if (file.exists(here("data", "ram.zip")) == FALSE) {
  download.file(
    "https://zenodo.org/record/4824192/files/RAMLDB%20v4.495.zip?download=1",
    destfile = here("data", "ram.zip"),
    mode = "wb"
  )
  
  unzip(here("data", "ram.zip"), exdir = here("data", "ram")) # unzip = 'unzip' needed for windows
}

ram_files <-
  list.files(here("data", "ram", "R Data"), recursive = TRUE)

ram_files <- ram_files[str_detect(ram_files, ".RData")]

load(here("data", "ram", "R Data", ram_files[1]))

# process ram data ------------------------------------------------------------

stock <- stock %>%
  left_join(area, by = "areaid")
# catches
ram_catches <- tcbest.data %>%
  mutate(year = rownames(.) %>% as.integer()) %>%
  as_tibble() %>%
  gather(stockid, catch, -year)

# B/Bmsy
ram_b_v_bmsy <- divbpref.data %>%
  mutate(year = rownames(.) %>% as.integer()) %>%
  tibble() %>%
  gather(stockid, b_v_bmsy, -year)


# U/Umsy
ram_u_v_umsy <- divupref.data %>%
  mutate(year = rownames(.) %>% as.integer()) %>%
  as_tibble() %>%
  gather(stockid, u_v_umsy, -year)

# Effort
ram_effort <- effort.data %>%
  mutate(year = rownames(.) %>% as.integer()) %>%
  as_tibble() %>%
  gather(stockid, effort, -year)

# biomass

ram_total_biomass <- tbbest.data %>%
  mutate(year = rownames(.) %>% as.integer()) %>%
  as_tibble() %>%
  gather(stockid, total_biomass, -year)

# ssb

ram_ss_biomass <- ssb.data %>%
  mutate(year = rownames(.) %>% as.integer()) %>%
  as_tibble() %>%
  gather(stockid, ss_biomass, -year)


ram_exp_rate <- ram_catches %>%
  left_join(ram_total_biomass, by = c("stockid", "year")) %>%
  mutate(exploitation_rate = catch / total_biomass) %>%
  select(-catch,-total_biomass)

# put it together

ram_data <- ram_catches %>%
  left_join(bioparams_values_views, by = "stockid") %>%
  left_join(ram_b_v_bmsy, by = c("stockid", "year")) %>%
  left_join(ram_u_v_umsy, by = c("stockid", "year")) %>%
  left_join(ram_exp_rate, by = c("stockid", "year")) %>%
  left_join(ram_effort, by = c("stockid", "year")) %>%
  left_join(ram_total_biomass, by = c("stockid", "year")) %>%
  left_join(ram_ss_biomass, by = c("stockid", "year")) %>%
  left_join(stock, by = "stockid") %>%
  select(stockid, scientificname, commonname, everything())


# create new variables

ram_data <- ram_data %>%
  mutate(tb_v_tb0 = total_biomass / TB0,
         ssb_v_ssb0 = ss_biomass / SSB0)

# filter data -------------------------------------------------------------

# for now, only include continuous catch series

ram_data <- ram_data %>%
  filter(is.na(catch) == FALSE) %>%
  # filter(stockid == "ATBTUNAEATL") %>%
  group_by(stockid) %>%
  mutate(delta_year = year - lag(year)) %>%
  mutate(delta_year = case_when(year == min(year) ~ as.integer(1),
                                TRUE ~ delta_year)) %>%
  mutate(missing_gaps = any(delta_year > 1)) %>%
  filter(missing_gaps == FALSE) %>%
  mutate(n_years = n_distinct(year)) %>%
  filter(n_years >= min_years_catch) %>%
  filter(all(b_v_bmsy < crazy_b, na.rm = TRUE),
         all(u_v_umsy < crazy_u, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(stockid) %>%
  mutate(
    has_tb0 = !all(is.na(TB0)),
    has_tb = all(!is.na(total_biomass)),
    first_catch_year = year[which(catch > 0)[1]]
  ) %>%
  filter(year >= first_catch_year) %>%
  mutate(
    pchange_effort = lead(u_v_umsy) / (u_v_umsy + 1e-6),
    cs_effort = (u_v_umsy - mean(u_v_umsy)) / sd(u_v_umsy),
    index = total_biomass * catchability,
    approx_cpue = catch / (u_v_umsy / catchability + 1e-3),
    b_rel = dplyr::case_when(
      has_tb0 ~ total_biomass / max(TB0),
      has_tb ~ total_biomass / max(total_biomass),
      TRUE ~ b_v_bmsy / 2.5
    )
  ) %>%
  mutate(approx_cpue = pmin(quantile(approx_cpue, 0.9, na.rm = TRUE), approx_cpue)) %>%
  ungroup()

ram_data <- ram_data %>%
  rename(
    fao_area_code = primary_FAOarea,
    scientific_name = scientificname,
    common_name = commonname,
    capture = catch
  ) %>%
  mutate(
    macroid = paste(scientific_name, fao_area_code, sep = '_'),
    fao_area_code = as.integer(fao_area_code)
  )


status_model_data <- ram_data %>%
  mutate(catch = capture) %>%
  group_by(stockid) %>%
  mutate(
    c_div_maxc = catch / max(catch, na.rm = TRUE),
    c_div_meanc = catch / mean(catch, na.rm = TRUE),
    fishery_year = 1:length(catch)
  ) %>%
  mutate(
    c_roll_meanc = RcppRoll::roll_meanr(c_div_meanc, 5),
    c_roll_maxc = catch / cummax(catch),
    c_init_slope = lm(log(catch[1:10] + 1e-3) ~ year[1:10])$coefficients[2]
  ) %>%
  gather(metric, value, b_v_bmsy, u_v_umsy, exploitation_rate) %>%
  select(stockid,
         year,
         contains('c_'),
         metric,
         value,
         fishery_year) %>%
  mutate(log_value = log(value + 1e-3)) %>%
  unique() %>%
  na.omit() %>%
  ungroup() %>%
  group_by(stockid) %>%
  filter(fishery_year > 20) %>%
  ungroup()

b_data <- status_model_data %>%
  filter(metric == "b_v_bmsy") %>%
  left_join(ram_data %>% select(stockid, primary_country, fao_area_code) %>% unique(),
            by = "stockid")

training_data <- b_data %>%
  filter(!primary_country %in% c("New Zealand", "Australia", "South Africa"))


testing_data <- b_data %>%
  filter(primary_country %in% c("New Zealand", "Australia", "South Africa"))



# fit models --------------------------------------------------------------


glm_model <-
  stan_glm(
    value ~ c_div_maxc + c_div_meanc + c_roll_meanc + c_roll_maxc + c_init_slope + fishery_year,
    family = Gamma(link = "log"),
    data = training_data,
    cores = 4, 
    chains = 4
  )

training_data$pred_stanglm <- glm_model$fitted.values

testing_data$pred_stanglm <- colMeans(rstanarm::posterior_epred(glm_model, newdata = testing_data))


randomforest_model <-   ranger(
  value ~ c_div_maxc + c_div_meanc + c_roll_meanc + c_roll_maxc + fishery_year,
  data = training_data
)

training_data$pred_randomforest <-  (predict(randomforest_model, data = training_data)$predictions)

testing_data$pred_randomforest <-  (predict(randomforest_model, data = testing_data)$predictions)



xgb_train <- training_data %>% select(c_div_maxc, c_div_meanc, c_roll_meanc, c_roll_maxc, fishery_year) %>% as.matrix()

xgb_test <- testing_data %>% select(c_div_maxc, c_div_meanc, c_roll_meanc, c_roll_maxc, fishery_year) %>% as.matrix()

xgboost_model <-   xgboost(
  label = training_data$value, 
  data = xgb_train,
  nrounds = 15
)

training_data$pred_xgboost <-  predict(xgboost_model, newdata = xgb_train)

testing_data$pred_xgboost<-   predict(xgboost_model, newdata = xgb_test)


# neuralnet_model <-  neuralnet(
#   value ~ c_div_maxc + c_div_meanc + c_roll_meanc + c_roll_maxc + c_init_slope,
#   data = training_data %>% dplyr::sample_n(2000),
#   hidden = c(2,1),
#   rep = 1,
#   stepmax = 1e5,
#   linear.output=TRUE,
#   algorithm = "rprop-"
# )


# training_data$neuralnet_pred<-  predict(neuralnet_model, newdata = training_data)
# 
# testing_data$xgboost_pred <-   predict(xgboost_model, newdata = xgb_test)


# nn <- neuralnet(Petal.Length ~ Petal.Width, iris, hidden = 2, rep = 10)
# 
# iris$test = as.numeric(predict(nn, newdata = iris)
# 
# iris %>% 
#   ggplot(aes(Petal.Length, test)) + 
#   geom_abline(slope = 1, intercept = 0) +
#   geom_point() 

train <- training_data %>% 
  pivot_longer(contains("pred_"), names_to = "model", values_to = "Predicted", names_prefix = "pred_") %>% 
  mutate(split = "Training")


test <- testing_data %>% 
  pivot_longer(contains("pred_"), names_to = "model", values_to = "Predicted", names_prefix = "pred_") %>% 
  mutate(split = "Testing")

compare <- train %>% 
  bind_rows(test)

compare %>% 
  ggplot(aes(value, Predicted, color = model)) + 
  geom_abline(slope = 1, intercept = 0)+
  geom_point(alpha = 0.25) + 
  geom_smooth(method = "lm") +
  facet_wrap(~split)

# variable importance -----------------------------------------------------


n <- 1000

x <- rnorm(n,10,1)

z <- rnorm(n,-10,1)

w <- sample(c(-1,1), n, replace = TRUE) # w is a switch that controls the direction of the effect of z

y <- 25 * x + -10*z*w + 50 *z # true model

dat <- data.frame(x = x, z = z, y = y, w = w)

linear_model <- lm(y ~ x + z + z:w + w, data = dat) # correctly specified linear regression

ml_model <- ranger(y ~ x + z + w, data = dat, importance = "impurity") # random forest


lm_mod <- broom::tidy(linear_model) %>%
  select(term, estimate) %>%
  filter(term != "(Intercept)")

lm_plot <- lm_mod %>% 
  ggplot(aes(term, estimate)) + 
  geom_hline(aes(yintercept = 0)) +
  geom_col() + 
  scale_x_discrete(name = '') +
  scale_y_continuous(name = "Estimated Coefficient") +
  theme_economist() + 
  labs(title = "Linear regression")

# w shows up as the most important variable by a landslide, despite
# actually only being a switch on the effect of z. But, you can't understand the actually big effect of z without understanding w, so it is in fact very important, but it's because it helps you explain the
# effect of z
ml_mod <- ml_model %>% 
  vi() %>% 
  arrange(desc(Importance)) 

ml_plot <- ml_mod %>% 
  ggplot(aes(Variable, Importance)) + 
  geom_hline(aes(yintercept = 0)) +
  geom_col() + 
  scale_x_discrete(name = '') +
  scale_y_continuous(name = "Variable Importance") +
  theme_economist() + 
  labs(title = "Random Forest")


vip_plot <- lm_plot + ml_plot

vip_plot
# partial dependency -----------------------------------------------------

c_max_c_plot <- pdp::partial(randomforest_model, pred.var = "c_div_maxc", plot = TRUE,
              plot.engine = "ggplot2")


tmp <- tibble(x = seq(-10,10, by = .01))

pd_data <- tmp %>% 
  bind_rows(tmp) %>% 
  mutate(splitter = sample(c(1,-1), nrow(.), replace = TRUE)) %>% 
  mutate(y = .2 * x * splitter)

pd_data_plot <- pd_data %>% 
  ggplot(aes(x,y)) + 
  geom_point()

pd_model <- ranger(y ~ x + splitter, data = pd_data)

pd_plot <- pdp::partial(pd_model, pred.var = "x", plot = TRUE,
                             plot.engine = "ggplot2",ice = TRUE)

pd_plot + 
  geom_point(data = pd_data, aes(x,y), alpha = 0.25)

