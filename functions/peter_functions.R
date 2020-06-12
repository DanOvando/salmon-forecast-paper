#--------------------------------------------------------------------------------------
#Source functions used below
#Requires creating a new column of lagged observations
my_mase <- function (y, y_hat, y_prev) 
{
  # print("make sure NAs filtered out of input")
  #Calculate the ae using the previous time steps
  den <- sum(ae(y, y_prev), na.rm = T)
  out <- sum(ae(y, y_hat)) / den
  return(out)
}

#Functions from tsensembler package
#Absolute error
ae <- function (y, y_hat, ...) 
{
  stopifnot(length(y) == length(y_hat), is.numeric(y), is.numeric(y_hat))
  abs(y - y_hat)
}

#Squared error
se <- function (y, y_hat, ...) 
{
  stopifnot(length(y) == length(y_hat), is.numeric(y), is.numeric(y_hat))
  (y - y_hat)^2
}

#Mean squared error
mse <- function(y, y_hat) mean(se(y, y_hat), na.rm = TRUE)

#Root mean squared error
rmse <- function (y, y_hat) sqrt(mse(y, y_hat))