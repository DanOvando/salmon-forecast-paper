#==================================================================================================
#Project Name: SALMON FORECAST PAPER - Presentation to BBRSDA
#Creator: Curry James Cunningham, College of Fisheries and Ocean Sciences, UAF
#Date: 8.30.20
#
#Purpose: To generate summary figures for presentation to BBRSDA
#
#
#
#==================================================================================================
#NOTES:

#==================================================================================================
require(tidyverse)
require(dplyr)
require(ggthemes)
require(here)
require(readr)
require(tidybayes)

# Define Workflow Paths ============================================
# dir.results <- file.path(here(), "results", "v0.5.5")
# dir.results <- file.path(here(), "results", "v1.0.0.9000-2021")
dir.results <- file.path(here(), "results", "v1.1-2021")
dir.data <- file.path(here(), "data")
dir.figs <- file.path(here(), "imgs")
dir.fxns <- file.path(here(), "functions")

source(file.path(dir.fxns, "get-published-fcst.R"))

# Load Data ========================================================
dlm <- read.csv(file.path(dir.results,"dlm_loo_results.csv"), header=TRUE, stringsAsFactors=FALSE)
edm <- read.csv(file.path(dir.results,"edm_loo_results.csv"), header=TRUE, stringsAsFactors=FALSE)
# rnn <- read.csv(file.path(dir.results,"rnn_loo_results.csv"), header=TRUE, stringsAsFactors=FALSE)
parsnip <- read.csv(file.path(dir.results,"parsnip_loo_results.csv"), header=TRUE, stringsAsFactors=FALSE)
nextl <- read.csv(file.path(dir.results,"next_loo_results.csv"), header=TRUE, stringsAsFactors=FALSE)
benchmark <- read.csv(file.path(dir.results,"benchmark_loo_results.csv"), header=TRUE, stringsAsFactors=FALSE)

# Combined
result <- rbind(dlm, edm, parsnip, benchmark)# nextl)
result <- data.frame(result)
# Rename edm
result$model[result$model=="simplex_one system top6 ages"] <- "edm_simplex"
result$model[result$model=="smap_one system top6 ages"] <- "edm_smap"

# Create Factors
result$model <- as.factor(result$model)
result$system <- as.factor(result$system)
result$age_group <- as.factor(result$age_group)

# Load Observed Data
# dat <- read.csv(file.path(here(), "data", "2019.csv"), header=TRUE, stringsAsFactors=TRUE)
dat <- read.csv(file.path(here(), "data", "2020.csv"), header=TRUE, stringsAsFactors=TRUE)
dat <- dat %>% mutate("age_group"=paste0(fwAge, "_", oAge))
dat$age_group <- factor(dat$age_group)

# rename system
names(dat)[3] <- "system"

# Load FRI published forecast
# fri <- get_published_fcst(dir.pf=here(file.path("data","preseasonForecast.dat")),
#                        dir.ids=here(file.path("data","ID_Systems.csv")),
#                        years=1990:2019)
# result.fri <- cbind("FRI", fri$retYr-fri$fwAge-fri$oAge-1, fri$retYr, 
#                     fri$System, paste0(fri$fwAge,"_",fri$oAge))
# Summarize by Stock and Age =======================================
result.stock <- result %>% group_by(model, return_year, system) %>% 
                  summarize(obs=sum(observed_returns, na.rm=TRUE),
                            pred=sum(predicted_returns, na.rm=TRUE))

result.age <- result %>% group_by(model, return_year, age_group) %>% 
              summarize(obs=sum(observed_returns, na.rm=TRUE),
              pred=sum(predicted_returns, na.rm=TRUE))

# Separate Observed Run Size =======================================
g <- result.stock %>% mutate(pct.diff=(pred-obs)/obs) %>% 
       ggplot(aes(x=model, y=pct.diff, fill=model)) +
         theme_linedraw() +
         geom_hline(yintercept = 0, col="red", lwd=1.5) +
         geom_eye(.width = c(0.5, 0.95)) +
         facet_wrap(~system, scales="free_y") +
         theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1))
g

# diff in log space
g <- result.stock %>% mutate(log.diff=log(pred)-log(obs)) %>% 
  ggplot(aes(x=model, y=log.diff, fill=model)) +
  theme_linedraw() +
  geom_hline(yintercept = 0, col="red", lwd=1.5) +
  stat_eye(.width = c(0.5, 0.95)) +
  facet_wrap(~system, scales="free_y") +
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1))
g

# Check to make sure results are on same scale
result.stock %>%  ggplot(aes(log(obs), fill=model)) +
                    geom_density(, alpha=0.2) +
                    facet_wrap(~system)

# MAPE Summary =====================================================
# g.mape.stock <- result.stock %>% filter(model!="runmean_forecast", model!="lag_forecast", model!="edm_smap", system!="Alagnak", system!="Togiak"
#                                         ) %>% 
g.mape.stock <- result.stock %>% filter(model!="runmean_forecast", model!="lag_forecast", 
                                          system!="Alagnak", system!="Togiak") %>% 
            mutate(pct.diff=(pred-obs)/obs) %>% 
            group_by(system, model) %>% summarize(mape=mean(abs(pct.diff))) %>% 
            ggplot(aes(x=model, y=mape, fill=model)) +
            theme_linedraw() +
            scale_fill_colorblind() +
            geom_hline(yintercept = 0, col="black") +
            geom_col() +
            facet_wrap(~system, scales="free_y", ncol=7) +
            # facet_wrap(~system, ncol=7) +
            theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1),
                  legend.position="top") +
            scale_y_continuous(labels = scales::percent) +
            # ylab("Mean Absolute Percent Error") +
            ylab("Average Percent Error")
g.mape.stock
ggsave(file.path(dir.figs, "MAPE Stock.pdf"), plot=g.mape.stock, height=6, width=9, units="in")

# RMSE
g.rmse.stock <- result.stock %>% filter(model!="runmean_forecast", model!="lag_forecast", 
                                        # model!="edm_smap", 
                                        system!="Alagnak", system!="Togiak") %>% 
  mutate(pct.diff=(pred-obs)/obs) %>% 
  group_by(system, model) %>% summarize(mape=mean(abs(pct.diff)),
                                        rmse=yardstick::rmse_vec(truth=obs, estimate=pred)) %>% 
  ggplot(aes(x=model, y=mape, fill=model)) +
  theme_linedraw() +
  scale_fill_colorblind() +
  geom_hline(yintercept = 0, col="black") +
  geom_col() +
  facet_wrap(~system, scales="free_y", ncol=7) +
  # facet_wrap(~system, ncol=7) +
  theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1),
        legend.position="top") +
  scale_y_continuous(labels = scales::percent) +
  # ylab("Mean Absolute Percent Error") +
  ylab("Root Mean Square Error")
g.rmse.stock
ggsave(file.path(dir.figs, "RMSE Stock.pdf"), plot=g.rmse.stock, height=6, width=9, units="in")


g.mape.age <- result.age %>% filter(model!="runmean_forecast", model!="lag_forecast",
                                    age_group %in% c("1_2","1_3","2_2","2_3")) %>% 
  # , model!="edm_smap") %>%
  mutate(pct.diff=(pred-obs)/obs) %>% 
  group_by(age_group, model) %>% summarize(mape=mean(abs(pct.diff))) %>% 
  ggplot(aes(x=model, y=mape, fill=model)) +
  theme_linedraw() +
  scale_fill_colorblind() +
  geom_hline(yintercept = 0, col="black") +
  geom_col() +
  facet_wrap(~age_group, scales="free_y") +
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1)) +
  scale_y_continuous(labels = scales::percent) +
  ylab("Mean Absolute Percent Error")
g.mape.age
ggsave(file.path(dir.figs, "MAPE Age.pdf"), plot=g.mape.age, height=6, width=9, units="in")

# Egegik Lines ======================================
dat.eg <- dat %>% filter(system=="Egegik", retYr>=1990,
                         age_group %in% c("1_2","1_3","2_2","2_3"))

g.eg <- result %>% filter(system=="Egegik") %>% 
                    filter(model!="runmean_forecast", model!="lag_forecast", 
                           # model!="edm_smap"
                           ) %>%
          ggplot(aes(x=return_year, y=predicted_returns/1e6, col=model)) +
            theme_linedraw() +
            scale_color_colorblind() +
            geom_col(aes(x=retYr, y=ret/1e3), data=dat.eg, fill='darkgray', col=NA) +
            # geom_col(aes(x=return_year, y=observed_returns/1e6), fill='darkgray', col=NA) +
            geom_line(alpha=0.75, lwd=1) +
            facet_wrap(~age_group, scales="free_y") +
            ylab("Returns (millions of salmon)") + 
            xlab("Year") +
            ggtitle("Egegik")
            

g.eg
ggsave(file.path(dir.figs, "Egegik Lines.pdf"), plot=g.eg, height=6, width=9, units="in")


# PDF of Stock Lines ======================================
temp.stocks <- unique(result$system)
n.temp.stocks <- length(temp.stocks)

pdf(file.path(dir.figs, "Stocks Lines All.pdf"), height=6, width=9)
# ggsave(file.path(dir.figs, "Egegik Lines.pdf"), plot=g.eg, height=6, width=9, units="in")

s <- 1
for(s in 1:n.temp.stocks) {
  temp.stock <- temp.stocks[s]
  
  dat.eg <- dat %>% filter(system==temp.stock, retYr>=1990,
                           age_group %in% c("1_2","1_3","2_2","2_3"))

  g <- result %>% filter(system==temp.stock, age_group %in% c("1_2","1_3","2_2","2_3")) %>% 
                  filter(model!="runmean_forecast", model!="lag_forecast", 
          # model!="edm_smap"
         ) %>%
    ggplot(aes(x=return_year, y=predicted_returns/1e6, col=model)) +
    theme_linedraw() +
    scale_color_colorblind() +
    geom_col(aes(x=retYr, y=ret/1e3), data=dat.eg, fill='darkgray', col=NA) +
    # geom_col(aes(x=return_year, y=observed_returns/1e6), fill='darkgray', col=NA) +
    geom_line(alpha=0.75, lwd=1) +
    facet_wrap(~age_group, scales="free_y") +
    ylab("Returns (millions of salmon)") + 
    xlab("Year") +
    ggtitle(temp.stock)

  # Plot the figure
  plot(g)

} #next s

dev.off()



# Stock Lines =======================================================
dat.temp <- dat %>% filter(retYr>=1990) %>% group_by(system, retYr) %>% 
              summarize(ret=sum(ret, na.rm=TRUE))

g.line <- result.stock %>% 
  filter(model!="runmean_forecast", model!="lag_forecast", 
         # model!="edm_smap"
         ) %>%
  ggplot(aes(x=return_year, y=pred/1e6, col=model)) +
  theme_linedraw() +
  scale_color_colorblind() +
  geom_col(aes(x=retYr, y=ret/1e3), data=dat.temp, fill='darkgray', col="gray") +
  geom_line(alpha=0.6, lwd=1) +
  facet_wrap(~system, scales="free_y") +
  ylab("Returns (millions of salmon)") + 
  xlab("Year") +
  theme(legend.position="top")


g.line
ggsave(file.path(dir.figs, "System Lines.pdf"), plot=g.line, height=6, width=9, units="in")

# Stock Lines - Subset =======================================================
stocks.sub <- c("Egegik","Wood")
n.stocks.sub <- length(stocks.sub)

dat.temp <- dat %>% filter(retYr>=1990, system %in% stocks.sub) %>% group_by(system, retYr) %>% 
              summarize(ret=sum(ret, na.rm=TRUE))

g.line.sub <- result.stock %>% 
  filter(model!="runmean_forecast", model!="lag_forecast", 
         # model!="edm_smap", 
         system %in% stocks.sub) %>%
  ggplot(aes(x=return_year, y=pred/1e6, col=model)) +
  theme_linedraw() +
  scale_color_colorblind() +
  geom_col(aes(x=retYr, y=ret/1e3), data=dat.temp, fill='darkgray', col=NA) +
  geom_line(alpha=0.75, lwd=1) +
  facet_wrap(~system, scales="free_y") +
  ylab("Returns (millions of salmon)") + 
  xlab("Year") +
  theme(legend.position="top")


g.line.sub
ggsave(file.path(dir.figs, "System Lines Subset.pdf"), plot=g.line.sub, height=6*0.75, width=9*0.75, units="in")



# 

