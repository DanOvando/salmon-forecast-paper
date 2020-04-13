#' prepare tensor for passing to keras without a generator function
#' takes raw returns by age group and returns a tensorflow ready object
#'
#' @param dep_age
#' @param lookback
#' @param data salmon data
#'
#' @return a dataframe prepared for tensorflow
#' @export
#'
#' @examples
#' \dontrun{
#' tensor_data <- prepare_splits(data,
#' dep_var = "1.2",
#' test_year= 2000,
#' shuffle = FALSE,
#' delta = TRUE)
#' }
#'
prepare_tensor <-
  function(data,
           lookback,
           buffer = TRUE,
           split_type = "train") {
    data$samples <- 1:nrow(data)
    
    x_data <-
      data[, !colnames(data) %in% c(
        "ret",
        "ret_yr",
        "brood_yr",
        "data_use",
        "samples",
        "system",
        "age_group",
        "ogy",
        "ages"
      )] # god this is hacky
    
    # x_data <-
    #   data[, !colnames(data) %in% c("ret",
    #                                 "ret_yr",
    #                                 "data_use",
    #                                 "samples",
    #                                 "system",
    #                                 "age_group",
    #                                 "ogy")] # god this is hacky
    
    
    data$og_data_use <- data$data_use
    
    if (split_type == "all") {
      data$data_use <- split_type
      
    }
    
    if (buffer == TRUE) {
      min_year <-
        min(data$ret_yr[data$data_use == split_type]) + lookback - 1
      
    } else {
      min_year <- min(data$ret_yr) + lookback - 1
    }
    
    sample_ids <-
      data$samples[data$ret_yr > min_year &
                     data$data_use == split_type]
    
    systems <- n_distinct(data$system)
    
    temp <- x_data %>%
      select(-contains("_")) %>%
      bind_cols(x_data %>% select(contains(as.character(1990))))
    
    samples <-
      array(NA, dim = c(
        length(sample_ids),
        lookback,
        ifelse(unique(data$ages) == "cohort", ncol(temp), ncol(x_data))
      ))
    
    for (i in seq_along(sample_ids)) {
  
      s <- sample_ids[i]
      
      brood_yr <- data$brood_yr[s]
      
      tempx <- x_data[(s - lookback):(s - 1), ]
      
      if (unique(data$ages) == "cohort") {
        tempx <- tempx %>%
          select(-contains("_")) %>%
          bind_cols(tempx %>% select(contains(as.character(brood_yr))))
        
      }
      
      samples[i, ,] <- as.matrix(tempx)
      
    }
    
    targets <- data$ret[sample_ids]
    
    meta_targets <-
      data[sample_ids, colnames(data) %in% c("age_group", "system", "ret_yr", "ret", "og_data_use",
                                             "ogy")]
    
    meta_targets <- rename(meta_targets, data_use = og_data_use)
    
    return(list(
      samples = samples,
      targets = targets,
      meta_targets = meta_targets
    ))
    
  }
