#' prepare data in wide format with feature engineering from training split
#'
#' takes raw returns by age group and performs feature engineering and filtering
#' to prepare data for model fitting. Note that due to different model types
#' no lagging is performed in prepare_data, and instead needs to be done
#' in the actual model fitting step as appropriate.
#'
#' @param data salmon data
#' @param val_prop the proportion of the training data to hold out for validation
#' @param test_year the year splitting training from testing data
#' @param min_year the minimum year to include in the data
#' @param shuffle should the dependent data be shuffled?
#' @param scalar number to divide the returns by to get it closer to the 0-1 scale
#' @param engineering the type of feature engineering to do
#' @param form whether to use age histories or sibling histories
#' @param delta_dep delta transform dependent variable
#' @param dep_age the age the model is predicting
#'
#' @return a dataframe prepared for model fitting
#' @export
#'
#' @examples
#' \dontrun{
#'check <- prepare_data(dep_age = "1.2", test_year = 2000, data = data)
#' }
#'
prepare_data <-
  function(engineering = "yeo_johnson",
           ages = "all",
           delta_dep = FALSE,
           dep_age,
           data,
           val_prop = 0.2,
           test_year,
           min_year = 1963,
           shuffle = TRUE,
           scalar = 1000) {
    if (shuffle == TRUE) {
      warning(
        "shuffling dependent variable for development, set shuffle = FALSE when you want to fit"
      )
    }
    
    # data <- data %>% {
    #   if (shuffle == TRUE) {
    #     mutate(., ret = sample(ret, length(ret), replace = FALSE))
    #   } else {
    #     .
    #   }
    # } %>%
    #   filter(ret_yr > min_year) %>%
    #   mutate(ret = ret / scalar,
    #          spawners = spawners / scalar) %>%
    #   ungroup()
    # 
    data <- data %>% {
      if (shuffle == TRUE) {
        mutate(., ret = sample(ret, length(ret), replace = FALSE))
      } else {
        .
      }
    } %>%
      filter(ret_yr > min_year) %>%
      mutate(ret = ret / scalar) %>%
      ungroup()
    
    
    train_data <- data %>%
      filter(ret_yr < test_year)
    # go through and prepare training data for feature engineering
    
    val_year <-
      sort(unique(train_data$ret_yr))[round(n_distinct(train_data$ret_yr) * (1 - val_prop))]
    
    pred_age <-
      ifelse(ages == "all", 999, sum(str_split(dep_age, "\\.", simplify = TRUE) %>% as.numeric()))
    
    temp_age <-
      as.numeric(str_split(dep_age, "\\.", simplify = TRUE))
    
    temp_x <- data %>%
      mutate(age = fw_age + o_age) %>% {
        if (ages == "younger") {
          filter(., age < sum(temp_age))
        } else if (ages == "siblings") {
          filter(., o_age == (temp_age[2] - 1) & fw_age == temp_age[1])
        } else if (ages == "cohort") {
          group_by(., ret_yr, brood_yr, system) %>%
            summarise(ret = sum(ret)) %>%
            mutate(age_group = as.character(brood_yr)) %>%
            ungroup()
        } else if (ages == "freshwater_cohort") {
          filter(., fw_age == temp_age[1]) %>%
            group_by(ret_yr, brood_yr, system) %>%
            summarise(ret = sum(ret)) %>%
            mutate(age_group = as.character(brood_yr)) %>%
            ungroup()
        } else {
          .
        }
      } %>%
      mutate(year = ret_yr) %>%
      select(ret_yr, system, age_group, ret, year) %>%
      arrange(ret_yr) %>%
      ungroup()
    
    test <- temp_x %>%
      unite("sysage", system, age_group, sep = '_') %>%
      spread(sysage, ret, fill = 0)
    
    
    # browser()
    # temp_x %>%
    #   filter(ret_yr > 2000, ret_yr < 2010) %>%
    #   ggplot(aes(ret_yr, ret, color = factor(age_group))) +
    #   geom_line(show.legend = FALSE) +
    #   geom_point(show.legend = FALSE) +
    #   scale_color_viridis_d() +
    #   facet_wrap(~system, scales = "free_y")
    #
    #
    
    temp_y <- data %>%
      filter(age_group == dep_age) %>%
      select(ret, ret_yr, brood_yr, contains("env_"), system) %>%
      arrange(system) %>%
      rename(y = ret)
    
    
    # temp_y <- data %>%
    #   filter(age_group == dep_age) %>%
    #   select(ret, ret_yr, brood_yr, contains("env_"), spawners, system) %>%
    #   arrange(system) %>%
    #   rename(y = ret)
    
    if (delta_dep == TRUE) {
      temp_y <- temp_y %>%
        group_by(system) %>%
        arrange(ret_yr) %>%
        mutate(ogy = y) %>%
        mutate(y = y - lag(y)) %>%
        na.omit()
      
    }
    
    ogs <- temp_y$system
    
    temp_y <- temp_y %>%
      model.matrix( ~ . - 1, data = .) %>%
      as.data.frame() %>%
      mutate(system = ogs)
    
    temp_train_x <- temp_x %>%
      filter(ret_yr <= val_year)
    
    
    train_recipe <- recipe(~ ., data = temp_train_x) %>%       {
      if (engineering == "yeo_johnson") {
        step_YeoJohnson(., all_numeric(), -contains("_yr"))
      } else if (engineering == "scale") {
        step_center(., all_numeric(), -contains("_yr")) %>%
          step_scale(., all_numeric(), -contains("_yr"))
      } else if (engineering == "range") {
        step_range(.,
                   all_numeric(),
                   -contains("_yr"),
                   min = 0,
                   max = 1)
      } else if (engineering == "none") {
        .
      }
    } #%>%
    # step_dummy(all_nominal(),-one_of("sys","age_group"), one_hot = TRUE) #interesting, step dummy as different syntax than the other ones, can't really string conditionals
    # # browser()
    
    prepped_recipe <-
      prep(train_recipe, training = temp_train_x, retain = TRUE)
    
    
    test <- juice(prepped_recipe) %>%
      unite("sysage", system, age_group, sep = '_') %>%
      spread(sysage, ret, fill = 0)
    
    processed_x <- bake(prepped_recipe, new_data = temp_x) %>%
      mutate(age_group = as.character(temp_x$age_group)) %>%
      unite("sys_age", system, age_group, sep = '_') %>%
      spread(sys_age, ret, fill = 0)
    
    processed_data <- temp_y %>% {
      if (ages != "cohort") {
        left_join(., processed_x, by = c("ret_yr"))
      } else {
        left_join(., processed_x, by = c("ret_yr"))
      }
    } %>%
      rename(ret = y) %>%
      arrange(system, ret_yr) # this step is CRITICAL to making sure the tensors come out rightr
    
    #
    # if (delta_dep == TRUE) {
    #   temp_ogy <- temp_y$ogy
    #
    #   processed_data <- processed_data %>%
    #     select(-ogy)
    # }
    #
    
    
    data_uses <-
      case_when(
        processed_data$ret_yr >= test_year ~ "test",
        processed_data$ret_yr < test_year &
          processed_data$ret_yr > val_year ~ "validate",
        TRUE ~ "train"
      )
    
    
    processed_data$data_use <-  data_uses
    
    # processed_data$system <- data$system
    
    processed_data$age_group <- dep_age
    
    processed_data$ages <- ages
    
    
    
    return(processed_data)
    
  }