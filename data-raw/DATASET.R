library(tidyverse)
library(here)
library(tools)


sheets <- readxl::excel_sheets(here("data-raw","2020_BBay_Return_Table.xlsx"))


get_returns <- function(system){
  
# system <- return_table$System[1]
  
  dat <- readxl::read_xlsx(here("data-raw","2020_BBay_Return_Table.xlsx"), sheet = toupper(system))
  
  if (is.character(dat[[1,1]])){
    
    dat <- readxl::read_xlsx(here("data-raw","2020_BBay_Return_Table.xlsx"), skip = 1,sheet = toupper(system))
    
  }
  
  template <- read_csv(here("data","2019.csv"))
  colnames(template)
  colnames(dat) 
  
  # template <- template %>% 
  #   mutate(test = retYr - (as.integer(fwAge) + as.integer(oAge) + 1)) %>% 
  #   mutate(check = test == broodYr)
  
  
  dat <-
    dat %>% 
    rename(retYr = `Return Year`) %>% 
    pivot_longer(-retYr, names_to = "age_group", values_to = "ret") %>% 
    mutate(age_group = as.character(round(as.numeric(age_group),1))) %>% 
    separate(age_group, into = c("fwAge","oAge"), sep = "\\.", convert = TRUE) %>% 
    mutate(broodYr = retYr - (as.integer(fwAge) + as.integer(oAge) + 1))
  
  dat <- dat[colnames(template %>% select(-System))]
  
}

template <- read_csv(here("data","2019.csv"))


return_table <- tibble(System = tools::toTitleCase(tolower(sheets)) %>% stringr::str_trim()) %>% 
  mutate(return = map(System, get_returns)) %>% 
  unnest(cols = return)

return_table <- return_table[,colnames(template)]


write_csv(return_table, path = here("data","2020.csv"))
