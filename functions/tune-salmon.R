#' tune forest
#'
#' @param splits the data splits
#' @param mtry mtry
#' @param splitrule splitrule
#' @param min_n minimum node size
#'
#' @return observed and predicted
#' @export
#'
tune_salmon <- function(model_type = "rand_forest",salmon_recipe, log_returns = FALSE,...) {
  
  options(dplyr.summarise.inform = FALSE)
  
  arguments <- list(...)
  
  analysis_split <-   rsample::analysis(arguments$splits) %>%
    unnest(data) %>%
    select(-year)
  
  assessment_split <-  rsample::assessment(arguments$splits) %>%
    unnest(data) %>%
    select(-year) 
  
  prepped_salmon <- prep(salmon_recipe, new_data = analysis_split)
  
  analysis_data <- bake(prepped_salmon, new_data = analysis_split)
  
  assessment_data <- bake(prepped_salmon, new_data = assessment_split)
  
  # forest_recipe <- recipe(ret ~ ., data = analysis_split) %>%
  #   step_dummy(all_nominal()) %>%
  #   step_nzv(-all_outcomes()) %>%
  #   step_corr(-all_outcomes()) %>%
  #   # step_bagimpute(contains("env_"), impute_with = imp_vars(ret_yr)) %>%
  #   prep(data = analysis_split, retain = TRUE)
  
  
  if (model_type == "rand_forest"){
    # browser()
    # message("hi")
    # ranger(
    #   formula(prepped_salmon),
    #   mtry = 600,
    #   min.node.size = arguments$min_n ,
    #   data = analysis_data,
    #   num.threads = 10,
    #   num.trees = trees
    # )
  salmon_fits <-
    parsnip::rand_forest(
      mode = "regression",
      mtry = arguments$mtry,
      min_n = arguments$min_n,
      trees = arguments$trees
    ) %>%
    parsnip::set_engine(
      "ranger",
      importance = "none",
      splitrule = arguments$splitrule#,
      # num.threads = 10
      # always.splits.variables = 'ret_yr'
    ) %>%
    parsnip::fit(formula(prepped_salmon), data = analysis_data)
  
  # importance(salmon_fits$fit) %>%
  #   broom::tidy() %>%
  #   View()
  } 
  if (model_type == "boost_tree"){
    
    salmon_fits <-
      parsnip::boost_tree(
        mode = "regression",
        mtry = arguments$mtry,
        min_n = arguments$min_n,
        learn_rate = arguments$learn_rate,
        tree_depth = arguments$tree_depth,
        loss_reduction = arguments$loss_reduction,
        trees = arguments$trees,
        sample_size = arguments$sample_size
      ) %>%
      parsnip::set_engine(
        "xgboost"
      ) %>%
      parsnip::fit(formula(prepped_salmon), data = analysis_data)
  }
  
  if (model_type == "mars"){
    salmon_fits <-
      parsnip::mars(
        mode = "regression",
        num_terms = arguments$num_terms,
        prod_degree = arguments$prod_degree,
        prune_method = arguments$prune_method
      ) %>%
      parsnip::set_engine(
        "earth") %>%
      parsnip::fit(formula(prepped_salmon), data = analysis_data)
    
    
  }
  
  assessment_prediction <-
    predict(salmon_fits, new_data = assessment_data)
  
  if (log_returns) {
    assessment_prediction$.pred <- exp(assessment_prediction$.pred)
  }
  
  if (nrow(assessment_prediction) != nrow(assessment_split)){
    stop("something really bad hapened in tune-salmon.R")
  }
  assessment_prediction$observed = assessment_split$ret
  
  return(assessment_prediction)
  
} # close tune forest
