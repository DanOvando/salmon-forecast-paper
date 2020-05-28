#' Extract published UW-FRI forecasts
#'
#' @param dir.pf Location of .dat file with past preseason forecasts
#' @param dir.ids Location of the system ID's to link to preseasonForecast.dat file
#' @param years Vector of years to extract
#'
#' @return
#' @export
#'
#' @examples
get_published_fcst <- function(dir.pf=here(file.path("data","preseasonForecast.dat")),
                                    dir.ids=here(file.path("data","ID_Systems.csv")), 
                                    years=2000:2018) {
  require(here)
  require(tidyverse)
  require(dplyr)

  # Read true preseason forecast data
  pf <- data.frame(read.table(dir.pf, skip=6))
  names(pf) <- c('agency','forecaster','retYr','dist','stream','fwA','oA','ret')
  
  # Limit to only UW-FRI forecasts
  pf.2 <- pf %>% filter(agency==1)
  
  # Load System ID's for linking
  ids <- read.csv(dir.ids, header=TRUE, stringsAsFactors = FALSE)
  
  # Link IDs to forecasts
  pf.3 <- pf.2 %>% left_join(ids, by=c("dist"="DistrictID", "stream"="SystemID"))
  
  # Select output
  pf.4 <- pf.3 %>% select(retYr, System, fwA, oA, ret)
  
  # Update names
  names(pf.4) <- c("retYr", "System", "fwAge", "oAge", "FRIfcst")
  
  # Limit year range
  pf.5 <- pf.4 %>% filter(retYr %in% years)
  
  # Return section
  return(data.frame(pf.5))
  
}

# TESTING ===============
# get_published_fcst(dir.pf=here(file.path("data","preseasonForecast.dat")),
#                      dir.ids=here(file.path("data","ID_Systems.csv")), 
#                      years=2000:2018)
