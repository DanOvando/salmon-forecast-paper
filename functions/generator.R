generator <- function(data, lookback, delay, min_index, max_index,
                      shuffle = FALSE, batch_size = 10) {
  if (is.null(max_index)) max_index <- nrow(data) - delay - 1
  i <- min_index + lookback
  function() {
    if (shuffle) {
      rows <- sample(c((min_index+lookback):max_index), size = batch_size)
    } else {
      if (i + batch_size >= max_index)
        i <<- min_index + lookback
      rows <- c(i:min(i+batch_size, max_index - delay))
      i <<- i + length(rows)
    } # close else
    samples <- array(0, dim = c(length(rows),
                                lookback,
                                dim(data)[[-1]]))
    targets <- array(0, dim = c(length(rows)))
    for (j in 1:length(rows)) {
      indices <- seq(rows[[j]] - lookback + 1, rows[[j]],
                     by = 1)
      samples[j,,] <- data[indices,]
      targets[[j]] <- data[rows[[j]] + delay,"obs"] # the temperature data
    }
    list(samples, targets)
  }
}