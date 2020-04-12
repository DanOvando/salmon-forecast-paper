
# prepare run -------------------------------------------------------------

functions <- list.files("functions")

sapply(functions, function(x) source(file.path("functions",x), print = FALSE))

prep_run(results_name = "test", results_description = "testing")

# load data ---------------------------------------------------------------

returns <- read_csv(here("data","2019.csv"))

# run analysis ------------------------------------------------------------

mean_annual_returns <- returns %>% 
  group_by(retYr) %>% 
  summarise(total_returns = sum(ret))

# process results ---------------------------------------------------------

write_rds(mean_annual_returns, path = file.path(results_dir,"mean_annual_returns.rds"))
