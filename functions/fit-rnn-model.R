#' fit salmon forecasting models
#'
#' @param tensor_data tensor data prepared by prepare_splits()
#' @param models a character vector of models to run
#' @param test include testing data (should be no)
#' @param ... options to be passed to models
#'
#' @return a list of fitted models, predictions, and options
#' @export
#'
#' @examples
#' \dontrun{
#' predictions <- fit_models(tensor_data, lookback = 4, units = 8,
#' test = TRUE)
#' }
#'
fit_rnn <-
  function(prepped_data,
           model = c("rnn"),
           preds = TRUE,
           save_fit = FALSE,
           save_dir,
           save_name = NA,
           ...) {
    # fill in options
    options <- list(...)
    
    defaults <-  list(
      dropout = 0.25,
      lookback = 2,
      units = 16,
      shuffle = TRUE,
      batch_size = 32,
      steps = 250,
      epochs = 200,
      rollwindow = 10,
      buffer = FALSE,
      early_stop = FALSE
    )
    
    
    foo <- function(par, name, opts) {
      if (name %in% names(opts)) {
        out <- opts[[name]]
        
      } else {
        out <- par
      }
      return(out)
    }
    
    options <- map2(defaults, names(defaults), foo, opts = options)
    
    out <- list()
    
    out$options <- options
    
    pd <- prepped_data %>%
      select(-ret_yr, -data_use) %>%
      as.matrix()
    
    
    if ("rnn" %in% model) {
      # 'simple' RNN
      
      training_splits <-
        prepare_tensor(
          data = prepped_data,
          lookback = options$lookback,
          split_type = "train",
          buffer = options$buffer
        )
      
      
      validation_splits <-
        prepare_tensor(
          data = prepped_data,
          lookback = options$lookback,
          split_type = "validate",
          buffer = options$buffer
        )
      
      min_batch <-
        ifelse(
          length(validation_splits$targets) == 0,
          options$batch_size,
          length(validation_splits$targets)
        )
      
      rnn_model <- keras_model_sequential() %>%
        layer_gru(
          units = options$units,
          input_shape = list(
            dim(training_splits$samples)[2],
            dim(training_splits$samples)[3]
          ),
          return_sequences = FALSE,
          activation = "relu",
          dropout = options$dropout,
          recurrent_dropout = 0.5,
        ) %>% {
          if (any(prepped_data$ret < 0)) {
            layer_dense(., units = 1)
            
          } else {
            layer_dense(., units = 1,
                        activation = "relu")
            
          }
        }
      rnn_model %>%
        compile(optimizer = optimizer_rmsprop(),
                loss = "mae")
      
      if (length(validation_splits$targets) > 0) {
        val_data <-
          list(validation_splits$samples, validation_splits$targets)
      } else {
        val_data <- NULL
      }
      
      if (options$early_stop == TRUE) {
        callbacks_list <- list(callback_early_stopping(monitor = "val_loss",
                                                       patience = 10))
      } else {
        callbacks_list <- NULL
      }
      history <- rnn_model %>% fit(
        x = training_splits$samples,
        y = training_splits$targets,
        batch_size = pmin(min_batch, options$batch_size),
        epochs = options$epochs,
        validation_data = val_data,
        shuffle = TRUE,
        callbacks = callbacks_list,
        verbose = 0
      )
      
      
      out$history <- history
      if (preds == TRUE) {
        training_splits$meta_targets$pred <-
          as.numeric(predict(rnn_model, training_splits$samples))
        
        validation_splits$meta_targets$pred <-
          as.numeric(predict(rnn_model, validation_splits$samples))
        
        out$preds <- list(training = training_splits$meta_targets,
                          validation = validation_splits$meta_targets)
        
      }
      out$model <- rnn_model
      
      out$model_name <-  "rnn"
      
      if (save_fit == TRUE) {
        save_model_hdf5(rnn_model, filepath = file.path(save_dir, "fits", paste0(save_name, ".h5")))
      }
      
    }
    
    if ("rnn-2" %in% model) {
      # more complicated rnn
      
      training_splits <-
        prepare_tensor(
          data = prepped_data,
          lookback = options$lookback,
          split_type = "train",
          buffer = options$buffer
        )
      
      validation_splits <-
        prepare_tensor(
          data = prepped_data,
          lookback = options$lookback,
          split_type = "validate",
          buffer = options$buffer
        )
      
      
      min_batch <-
        ifelse(
          length(validation_splits$targets) == 0,
          options$batch_size,
          length(validation_splits$targets)
        )
      
      rnn_model <- keras_model_sequential() %>%
        layer_gru(
          units = options$units,
          input_shape = list(
            dim(training_splits$samples)[2],
            dim(training_splits$samples)[3]
          ),
          return_sequences = TRUE,
          activation = "relu",
          dropout = options$dropout,
          recurrent_dropout = 0.5
        ) %>%
        layer_gru(
          units = options$units * 2,
          input_shape = list(
            dim(training_splits$samples)[2],
            dim(training_splits$samples)[3]
          ),
          return_sequences = TRUE,
          activation = "relu",
          dropout = options$dropout,
          recurrent_dropout = 0.5
        ) %>%    layer_gru(
          units = options$units * 2,
          input_shape = list(
            dim(training_splits$samples)[2],
            dim(training_splits$samples)[3]
          ),
          return_sequences = FALSE,
          activation = "relu",
          dropout = options$dropout,
          recurrent_dropout = 0.5
        ) %>% {
          if (any(prepped_data$ret < 0)) {
            layer_dense(., units = 1)
          } else {
            layer_dense(., units = 1,
                        activation = "relu")
            
          }
        }
      
      
      # rnn_model <- keras_model_sequential() %>%
      #   layer_gru(
      #     units = options$units * 2,
      #     input_shape = list(
      #       dim(training_splits$samples)[2],
      #       dim(training_splits$samples)[3]
      #     ),
      #     return_sequences = TRUE,
      #     activation = "relu",
      #     dropout = 0.1,
      #     recurrent_dropout = 0.5
      #   ) %>%
      #   layer_gru(
      #     units = options$units * 2,
      #     input_shape = list(
      #       dim(training_splits$samples)[2],
      #       dim(training_splits$samples)[3]
      #     ),
      #     return_sequences = TRUE,
      #     activation = "relu",
      #     dropout = 0.1,
      #     recurrent_dropout = 0.5
      #   ) %>%
      #   layer_gru(
      #     units = options$units,
      #     input_shape = list(
      #       dim(training_splits$samples)[2],
      #       dim(training_splits$samples)[3]
      #     ),
      #     return_sequences = TRUE,
      #     activation = "relu",
      #     dropout = 0.1,
      #     recurrent_dropout = 0.5
      #   ) %>%
      #   layer_gru(
      #     units = options$units * 2,
      #     input_shape = list(
      #       dim(training_splits$samples)[2],
      #       dim(training_splits$samples)[3]
      #     ),
      #     return_sequences = FALSE,
      #     activation = "relu",
      #     dropout = 0.1,
      #     recurrent_dropout = 0.5
      #   ) %>% {
      #     if (any(prepped_data$ret < 0)) {
      #       layer_dense(., units = 1)
      #
      #
      #     } else {
      #       layer_dense(., units = 1,
      #                   activation = "relu")
      #
      #
      #     }
      #   }
      
      # layer_dense(units = 1,
      #             activation = "relu")
      #
      rnn_model %>%
        compile(optimizer = optimizer_rmsprop(),
                loss = "mae")
      
      if (length(validation_splits$targets) > 0) {
        val_data <-
          list(validation_splits$samples, validation_splits$targets)
      } else {
        val_data <- NULL
      }
      
      if (options$early_stop == TRUE) {
        callbacks_list <- list(callback_early_stopping(monitor = "val_loss",
                                                       patience = 10))
      } else {
        callbacks_list <- NULL
      }
      
      # batch_size = pmin(min_batch,options$batch_size),
      history <- rnn_model %>% fit(
        x = training_splits$samples,
        y = training_splits$targets,
        batch_size = pmin(min_batch, options$batch_size),
        epochs = options$epochs,
        validation_data = val_data,
        shuffle = TRUE,
        callbacks = callbacks_list,
        verbose = 0
      )
      
      
      out$history <- history
      if (preds == TRUE) {
        training_splits$meta_targets$pred <-
          as.numeric(predict(rnn_model, training_splits$samples))
        
        validation_splits$meta_targets$pred <-
          as.numeric(predict(rnn_model, validation_splits$samples))
        
        out$preds <- list(training = training_splits$meta_targets,
                          validation = validation_splits$meta_targets)
        
      }
      out$model <- rnn_model
      
      out$model_name <-  "rnn"
      
      if (save_fit == TRUE) {
        save_model_hdf5(rnn_model, filepath = file.path(save_dir, "fits", paste0(save_name, ".h5")))
      }
      
    }
    
    
    return(out)
    
    
    
  }