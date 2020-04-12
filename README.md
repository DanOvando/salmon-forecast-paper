
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Salmon Forecast Paper

<!-- badges: start -->

<!-- badges: end -->

Materials needed to produce results for the loosely titled “Evaluating
the Potential of Computer-Age Statistical Methods in Ecological
Forcasting”

The end goal is to allow the script `make-salmon-forecast-results.R` to
produce all required results for our paper. We’ll make that happen once
we’ve all got our component scripts going though.

To facilitate this, I’ve set up a general structure to

1.  Avoid library conflicts (e.g. one person loads `plyr` in their
    script after someone else has loaded `dplyr`)

2.  Ensure a consistent data and results structure.

I’ve added a function to the `functions` folder called `prep-run.R`.
Place any libraries you need in there. That function will also create
(if needed) a folder for your results. For now, I suggest that each one
of you source this function at the start of your individual script.
Then, run `prep_run`, setting the name of the results folder you want to
create/use. This will not overwrite anyone’s results, but if you want to
work in your own folder for now we can knit this all together once
everything works.

In the example below, this will load the function and create a folder
called “test” inside the results folder. It will also add a variable
called `results_dir` to the global environment where results should be
saved.

One things to note is that the repo ignores the results folder (so that
we’re not pushing and pulling masive results files constantly). So, in
theory each one of us will need to re-run the others’ results on our own
systems to see them all. But, if that’s a pain time-wise during
development we can of course use google drive or whatever to share
results. But, since all the code is mostly done at this point I don’t
anticipate a lot of code wrangling at this point?

``` r

functions <- list.files("functions")

sapply(functions, function(x) source(file.path("functions",x), print = FALSE))
#>         prep-run.R
#> value   ?         
#> visible FALSE

prep_run(results_name = "test", results_description = "testing")

results_dir
#> [1] "/Users/danovan/projects/salmon-forecast-paper/results/test"
```

This step is pretty minor, just trying to avoid redundant and
conflicting libraries. To make sure that everyone’s scripts can run on
all computers, I’d sugest using the `here` packages (`file.path` also
works). `here` will create OS-correct paths on any computer. The only
assumption is that the working directory is set to this repository
(e.g. i.e. `salmon-forecast-paper`). You can then pass that path to
`file.path` to save objects in the correct place.

In terms of results to save, the only thing I’d request is that we have
a final dataframe with columns `model_name`,`brood_yr`,`return_year`,
`system`, `age_group`, `observed_returns`, `predicted_returns` saved as
a .rds file so that we can load and compare models.

I think we decided that 1965 is the first year of data we’ll use? So as
long as we all start in 1965, and all predictions are one-step ahead
predictions, should be able to compare performance easily.

To give an example

``` r
here::dr_here()
#> here() starts at /Users/danovan/projects/salmon-forecast-paper, because it contains a file matching `[.]Rproj$` with contents matching `^Version: ` in the first line

returns <- read_csv(here("data","2019.csv")) %>% 
  janitor::clean_names()
#> Parsed with column specification:
#> cols(
#>   broodYr = col_double(),
#>   retYr = col_double(),
#>   System = col_character(),
#>   fwAge = col_double(),
#>   oAge = col_double(),
#>   ret = col_double()
#> )

lag_model <- returns %>%
  tidyr::unite("age_group", contains("_age"), sep = '_') %>%
  group_by(system, age_group) %>%
  arrange(ret_yr) %>% 
  mutate(predicted_returns = lag(ret),
         model = "lag_1") %>%
  ungroup() %>% 
  rename(
    return_year = ret_yr,
    brood_year = brood_yr,
    observed_returns = ret
  ) %>%
  select(
    model,
    brood_year,
    return_year,
    system,
    age_group,
    observed_returns,
    predicted_returns
  ) %>% 
  filter(!is.na(predicted_returns))

head(lag_model)
#> # A tibble: 6 x 7
#>   model brood_year return_year system age_group observed_returns
#>   <chr>      <dbl>       <dbl> <chr>  <chr>                <dbl>
#> 1 lag_1       1950        1953 Egegik 0_2                      0
#> 2 lag_1       1950        1953 Egegik 1_1                      0
#> 3 lag_1       1950        1953 Ugash… 0_2                      0
#> 4 lag_1       1950        1953 Ugash… 1_1                      0
#> 5 lag_1       1951        1954 Kvich… 0_2                      0
#> 6 lag_1       1951        1954 Kvich… 1_1                      0
#> # … with 1 more variable: predicted_returns <dbl>

write_rds(lag_model, path = file.path(results_dir,"lag_model_predictions.rds"))
```
