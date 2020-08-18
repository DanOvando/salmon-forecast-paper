
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Salmon Forecast Paper

<!-- badges: start -->

<!-- badges: end -->

Materials needed to produce results for the loosely titled “Evaluating
the Potential of Computer-Age Statistical Methods in Ecological
Forecasting”

## Reproducing Results

All materials needed to reproduce our results and manuscript are
contained in this repository. In order to reproduce

1.  Fork the repository and clone to your machine

2.  Open R and set your working directory of the cloned repository (or
    just use RStudio projects)

3.  This project is set up with
    [`renv`](https://rstudio.github.io/renv/articles/renv.html) to
    manage package dependencies. Inside R (and with your working
    directory set correctly) run `renv::restore()`. This will install
    the correct versions of all the packages needed to replicate our
    results. Packages are installed in a stand-alone project library for
    this paper, and will not affect your installed R packages anywhere
    else.

4.  Open `make-salmon-forecast-paper.R` and

<!-- end list -->

  - set the name of the run in the call to `prep_run`
  - ensure that

<!-- end list -->

``` r
run_edm_forecast <- TRUE

run_dlm_forecast <- TRUE

run_ml_forecast <- TRUE

fit_statistical_ensemble <- TRUE

run_importance <- TRUE

knit_manuscript <- TRUE
```

5.  source `make-salmon-forecast-paper.R`

6.  The resulting manuscript will be compiled in the documents folder

## Abstract

Salmon are an ecological and economic keystone of Alaska. Effective
management of these species depends on accurate pre-season forecasts,
allowing fishery regulations to be set appropriately and fishing
operations to plan their economic activities accordingly. These
predictions are challenging to make though, given the complex set of
dynamic factors that affect numbers of returning salmon, from egg
production to predation to environmental drivers. Computer-age
statistical methods such as dynamic linear modeling, machine learning
and empirical dynamical modeling have fundamentally transformed many
aspects of our society by providing previously impossible levels of
predictive accuracy to tasks from digital advertising to weather
forecasting. However, these methods underutilized as applied tools for
predicting and managing natural resources. We utilize data from the
salmon populations of Bristol Bay, Alaska to evaluate the potential for
computer-age statistical methods to improve the accuracy of predictions
of annual returns. Salmon present an ideal case study due to the
presence of long and accurate time series of annual returns, and the
ecological and economical importance of reliable predictions to
commercial fisheries and subsistence-dependent communities. We
demonstrate how computer-age methods can help researchers and managers
provide improvements in forecast skill, and identify when the bottleneck
predictive skill is model design versus data. We also show that these
methods may not be a panacea for improving predictive performance when
confronted with time series of short duration, as are often found in
analysis of population dynamics and ecological forecasting applications
at annual time steps for aquatic and terrestrial species.

## Sample Results

![](README_files/figure-gfm/unnamed-chunk-3-1.pdf)<!-- -->

![](README_files/figure-gfm/unnamed-chunk-4-1.pdf)<!-- -->

![](README_files/figure-gfm/unnamed-chunk-5-1.pdf)<!-- -->
