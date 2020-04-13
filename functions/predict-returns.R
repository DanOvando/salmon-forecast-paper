#' predict returns from fitted model and prepped data
#'
#' @param prepped_data a prepped dataframe
#' @param fit the output from fit_model
#' @param split which split to use, defaults to all, so do filtering beforehand ideally
#'
#' @return predictions with metadata
#' @export
#'
#' @examples
#' \dontrun{
#' complete_preds <- predict_returns(prepped_data = prepped_data %>% filter(data_use != "test"),
#' fit = test)
#'
#' }
predict_returns <- function(prepped_data,
                            fit,
                            split = "all",
                            return_delta = FALSE) {
  if (str_detect(fit$model_name, "rnn")) {
    # put data that you want to predict in tensor format
    pred_splits <-
      prepare_tensor(
        data = prepped_data,
        lookback = fit$options$lookback,
        split_type = "all",
        buffer = fit$options$buffer
      )
    
    
    pred_splits$meta_targets$pred <-
      predict(fit$model, pred_splits$samples) # use model to predict
    
    # pred_splits$meta_targets %>%
    #   ggplot() +
    #   geom_point(aes(ret_yr, ret)) +
    #   geom_line(aes(ret_yr, pred)) +
    #   facet_wrap( ~ system, scales = "free_y")
    #
    
    out <- pred_splits$meta_targets
    
    if ("ogy" %in% colnames(out) & return_delta == FALSE) {
      # if desired, calcualte the raw returns by applying the differences starting at a reference year returns
      
      predyears <- sort(unique(out$ret_yr[out$data_use != "train"]))
      
      predsystems <- unique(out$system)
      
      last_train_year <- max(out$ret_yr[out$data_use == "train"])
      
      for (i in predsystems) {
        base <- out$ogy[out$system == i & out$ret_yr == last_train_year]
        
        for (y in predyears) {
          base <- base + out$pred[out$ret_yr == y & out$system == i]
          
          out$pred[out$ret_yr == y & out$system == i] <- base
          
        } # close years loop
        
      } # close systems loop
      
      out$ret <- out$ogy
      
    } # close ogy if
    
    
  } #close rnn if
  
  if (return_delta == FALSE) {
    out$pred <- pmax(0, out$pred)
  }
  
  return(out)
  
}