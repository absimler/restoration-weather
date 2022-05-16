## Code for: Interannual variation in climate contributes to contingency in post-fire restoration outcomes in seeded sagebrush steppe ####
## Simler-Williamson, Applestein, & Germino ###
## Final v. May 2022 ##

## 3) Autoregressive model for Question 3: Spatial variation in effects of weather #####

library(tidyverse)
library(brms)
library(rstan)
library(corrplot)
library(caret)
library(tidybayes)
library(loo)
library(corrplot)
library(ggplot2)
library(bayesplot)
library(modelr)

## Load and format data ######

sagegrowth <- read.csv("growthannualsage.csv")

## Deviation variables for spring weather variables:

sagegrowth$pptdev <- sagegrowth$pptcurr - sagegrowth$pptavg
sagegrowth$tmaxdev <- sagegrowth$tmaxcurr - sagegrowth$tmaxavg
sagegrowth$tmindev <- sagegrowth$tmincurr - sagegrowth$tminavg
sagegrowth$pptlagdev <- sagegrowth$pptlag - sagegrowth$pptavg
sagegrowth$tmaxlagdev <- sagegrowth$tmaxlag - sagegrowth$tmaxavg
sagegrowth$tminlagdev <- sagegrowth$tminlag - sagegrowth$tminavg
sagegrowth$se <- (sagegrowth$sdsage)/ sqrt(sagegrowth$totpixels)

## Split into test and training datasets:
set.seed(345)

trainIndex <- createDataPartition(sagegrowth$fireyr, p = .67,
                                  list = FALSE,
                                  times = 1)

growthtrain <- sagegrowth[trainIndex,]
growthtest <-sagegrowth[-trainIndex,]

##### Indices for varying effects:
growthtrain$rawfireyr <- growthtrain$fireyr
growthtrain$fireyr <- group_indices(growthtrain, fireyr)
growthtrain$clustindex <- group_indices(growthtrain, uniqueID)



### Fit autoregressive model for annual change in sagebrush cover#####
options(mc.cores=parallel::detectCores())

###

anngrowthmod <- brm(meansage |se(se, sigma=TRUE)+ trunc(lb=0, ub=100)~ 
                      scale(sageprev) + 
                    scale(pptavg) + scale(I(pptavg^2)) +
                    scale(tmaxavg) + scale(I(tmaxavg^2)) +
                    scale(tminavg) + scale(I(tminavg^2)) +
                    scale(pptdev) + scale(tmaxdev) +
                    scale(tmindev) +  
                    scale(pptlagdev) +
                    scale(pptavg)*scale(pptdev) +
                    scale(pptavg)*scale(pptlagdev) +
                    scale(tmaxavg)*scale(tmaxdev) + 
                    scale(tminavg)*scale(tmindev) +
                    (1|clustindex) + (1|TSF), 
                  data=growthtrain, 
                  family="Gaussian", chains=4, iter=2000, inits=0,
                  control=list(adapt_delta=0.99,max_treedepth = 15),
                  backend = "cmdstanr", 
                  threads = threading(8))

mcmc_plot(anngrowthmod, pars=c("sageprev", "pptavg", "tmaxavg", "tminavg",
                             "pptdev", "tmaxdev", "tmindev", "pptlagdev"))
mcmc_plot(anngrowthmod, pars=c("TSF"))

bayes_R2(anngrowthmod)
pp_check(anngrowthmod)

saveRDS(anngrowthmod, "modelfits/anngrowth_se.rds")


