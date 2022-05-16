## Code for: Interannual variation in climate contributes to contingency in post-fire restoration outcomes in seeded sagebrush steppe ####
## Simler-Williamson, Applestein, & Germino ###
## Final v. May 2022 ##

## 1) Fitting models associated with Questions 1 & 2 in text #####

#################################

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
library(cowplot)
library(cmdstanr)


## Format data #####

longtermsage <- read.csv("longtermsage.csv") # Load data formatted for long-term restoration outcome models.

nsites <- length(longtermsage$meansage10) # N for SE calc.


# Break into training and test datasets, scale, and create group index:
set.seed(345227)

trainIndex <- createDataPartition(longtermsage$BOX_ID, 
                                  p = .67,
                                  list = FALSE,
                                  times = 1)


# Subset variables:
longtermsage <-dplyr::select(longtermsage, contains(c("ER3", 
                                                      "30yrK", "ppt30", "dev", 
                                                      "mean_SPEI_2", "mean_SPEI_3", "mean_SPEI_4")), 
                             meanslope, meanelev, meanheatl, 
                             BOX_ID, fireyr, meansage1, meansage10, 
                             sdsage10, totpixels) %>% 
  dplyr::relocate(BOX_ID, fireyr, 
                  meansage1, meansage10, sdsage10) %>%
  dplyr::mutate( ppt30yr = apr_ppt30yr + 
                   mar_ppt30yr + feb_ppt30yr,
                 tmax30yr = (feb_tmax30yrK*28 + mar_tmax30yrK*31 + 
                               apr_tmax30yrK*30) / 89,
                 tmin30yr = feb_tmin30yrK)

# Scale variables
longtermsage <- dplyr::mutate_at(longtermsage, 
                                 vars(feb_tmax30yrK:meanheatl), scale)
longtermsage$se <- longtermsage$sdsage10 / sqrt(longtermsage$totpixels) # SE for measurement error portion of model
longtermsage$firenum <-longtermsage$fireyr
longtermsage$fireyr <- dplyr::group_indices(longtermsage, fireyr) #Indices for varying effects


# Split into test and train
clustertrain <- longtermsage[trainIndex,]
clustertest <- longtermsage[-trainIndex,]



### Fit models for comparison #####

options(mc.cores=parallel::detectCores())

# Model 5: Static, climate, and Weather variables with varying intercept: #####

## Subset variables:
clustertrain.full <- dplyr::select(clustertrain, meansage10, meansage1,
                                   ER314:aprppt_dev_y4, meanslope:totpixels, fireyr, se)


weatheryear <- brm(meansage10 |se(se, sigma=TRUE) + trunc(lb=0, ub=100)~  
                     . -se -totpixels -fireyr + (1|fireyr), data=clustertrain.full, 
                   family="Gaussian", chains=4, iter=2000, inits=0,
                   prior = set_prior(horseshoe(df=1, par_ratio=0.25, 
                                               scale_slab=2)),
                   control=list(adapt_delta=0.9999,max_treedepth = 20),
                   backend = "cmdstanr", 
                   threads = threading(8))


saveRDS(weatheryear, "modelfits/weatheryearSE.rds")



## Model 4: Static, climate, and weather variables only:
weather <- brm(meansage10 |se(se, sigma=TRUE) + trunc(lb=0, ub=100)~  
                 . - se - totpixels , 
               data=clustertrain.full, 
               family="Gaussian", chains=4, iter=2000, inits=0,
               prior = set_prior(horseshoe(df=1, par_ratio=0.25, 
                                           scale_slab=2)),
               control=list(adapt_delta=0.99,max_treedepth = 15),
               backend = "cmdstanr", 
               threads = threading(8))

mcmc_plot(weather)
saveRDS(weather, "modelfits/weatherSE.rds")


#Model 3: Static biophysical, climate, and year-1 weather only:
clustertrain.red2 <- select(clustertrain, meansage10, meansage1,
                            ER314:ER38, meanslope:totpixels, 
                            contains(c("30yr", "y1")),
                            fireyr, se)

weather.yr1 <- brm(meansage10 |se(se, sigma=TRUE) + trunc(lb=0, ub=100)~  
                     . - se - totpixels , 
                   data=clustertrain.red2, 
                   family="Gaussian", chains=4, iter=2000, inits=0,
                   prior = set_prior(horseshoe(df=1, par_ratio=0.25, 
                                               scale_slab=2)),
                   control=list(adapt_delta=0.999,max_treedepth = 15),
                   backend = "cmdstanr", 
                   threads = threading(8))


saveRDS(weather.yr1, "modelfits/weather.yr1SE.rds")

summary(weather.yr1)



#### Model 2: Static biophysical + Climate only:

clustertrain.clim <- select(clustertrain, meansage10, meansage1,
                            ER314:apr_ppt30yr, meanslope:totpixels, se)

climate <- brm(meansage10 |se(se, sigma=TRUE) + trunc(lb=0, ub=100)~ 
                 . - se - totpixels, 
               data=clustertrain.clim, 
               family="Gaussian", chains=4, iter=2000, inits=7,
               prior = set_prior(horseshoe(df=1, par_ratio=0.25, 
                                           scale_slab=2)),
               control=list(adapt_delta=0.99,max_treedepth = 15),
               backend = "cmdstanr", 
               threads = threading(8))


saveRDS(climate, "modelfits/climateSE.rds")


#### Extra model (not in text): Static + Climate + SPEI:

clustertrain.spei <- select(clustertrain, meansage10, meansage1,
                            ER314:apr_ppt30yr,
                            mean_SPEI_2.Yr0:mean_SPEI_2.Yr4,
                            mean_SPEI_3.Yr0:mean_SPEI_3.Yr4,
                            mean_SPEI_4.Yr0:mean_SPEI_4.Yr4,
                            meanslope:totpixels)

spei <- brm(meansage10 |se(se, sigma=TRUE) + trunc(lb=0, ub=100)~ . - se - totpixels, 
            data=clustertrain.spei, 
            family="Gaussian", chains=4, iter=2000, inits=0,
            prior = set_prior(horseshoe(df=1, par_ratio=0.25, 
                                        scale_slab=2)),
            control=list(adapt_delta=0.99,max_treedepth = 15))


saveRDS(spei, "modelfits/speiSE.rds")


#### Model 1: Static biophysical variables only:

clustertrain.stat <- select(clustertrain, meansage10, meansage1,
                            ER314:ER38, meanslope:totpixels, se)

static <- brm(meansage10  |se(se, sigma=TRUE) + trunc(lb=0, ub=100) ~
                ER314 + ER312 + ER311 + ER313 +
                ER39 + ER310 + ER38 +
                meanslope + meanelev + meanheatl + meansage1, 
              data=clustertrain.stat, 
              family="Gaussian", chains=4, iter=2000, inits=10,
              prior = set_prior(horseshoe(df=1, par_ratio=0.25, 
                                          scale_slab=2)),
              control=list(adapt_delta=0.99,max_treedepth = 15),
              backend = "cmdstanr", 
              threads = threading(8))


saveRDS(static, "modelfits/staticSE.rds")






