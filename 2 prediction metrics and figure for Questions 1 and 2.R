## Code for: Interannual variation in climate contributes to contingency in post-fire restoration outcomes in seeded sagebrush steppe ####
## Simler-Williamson, Applestein, & Germino ###
## Final v. May 2022 ##

## 2) Comparison of model prediction for Questions 1 & 2 in text #####

#################################

### Colors for plots:
orange <- "#DF4C20"
lightorange <- "#FF7C00"
red <- "#A32C18"
darkgrey <- "grey20"
lightgrey <-"#a4a4a4"
teal <-   "#02606c"
lightteal <-   "#6f9ca5"
blue <- "#052c6e"


## Load fitted models: ###########
weatheryear <-readRDS("modelfits/weatheryearSE.rds")
weather <-readRDS("modelfits/weatherSE.rds")
weather1 <-readRDS("modelfits/weather.yr1SE.rds")
#spei <- readRDS("modelfits/speiSE.rds")
climate <-readRDS("modelfits/climateSE.rds")
static <-readRDS("modelfits/staticSE.rds")


##Compare models on basis of LOOIC/ELPD:
weatheryear <- add_criterion(weatheryear, "loo")
weather <- add_criterion(weather, "loo")
weather1 <- add_criterion(weather1, "loo")
static <- add_criterion(static, "loo")
#spei <- add_criterion(spei, "loo")
climate <- add_criterion(climate, "loo")
loo_compare(weatheryear, weather, weather1, spei, static, climate)

modelloos <- loo_compare(weatheryear, weather, weather1, static, climate)

write.csv(modelloos, "tables/modelloos.csv")


## Within-sample R2 values:
R2weatheryr <- as.data.frame(bayes_R2(weatheryear))
R2weatheryr

R2weather <- as.data.frame(bayes_R2(weather))
R2weather

R2weather1 <- as.data.frame(bayes_R2(weather1))
R2weather1

R2climate <- as.data.frame(bayes_R2(climate))
R2climate

R2static <- as.data.frame(bayes_R2(static))
R2static


R2 <- rbind(R2static, R2climate, R2weather1, R2weather, R2weatheryr)

R2$model <- c("Static",
              "Static + Climate", 
              "Static + Climate + Weather (yr 1)",
              "Static + Climate + Weather (yrs 1-4)",
              "Static + Climate + Weather (yrs 1-4) + Year")
write.csv(R2, "tables/modelR2s.csv")


#### Calculate Out of sample prediction ##########

## Held out data for predictions:
clustersub <- dplyr::select(clustertest, meanelev, meanheatl, meanslope, 
                      apr_ppt30yr, mar_ppt30yr, feb_ppt30yr,
                      apr_tmax30yrK, mar_tmax30yrK, feb_tmax30yrK,
                      meansage10, ppt30yr, tmax30yr,tmin30yr, meansage1,
                      ER314:ER38)

# Create Bayesian R2 function for OOS CV:
## Based on same equation for premade bayes_R2 function used above: https://avehtari.github.io/bayes_R2/bayes_R2.html#2_Functions_for_Bayesian_R-squared_for_stan_glm_models (Based on Gelman, A., Goodrich, B., Gabry, J., & Vehtari, A. (2018). R-squared for Bayesian regression models. The American Statistician, 1-6. doi: 10.1080/00031305.2018.1549100)


r2 <- function(ypred, y){
e <- -1 * sweep(ypred, 2, y)
var_ypred <- apply(ypred, 1, var)
var_e <- apply(e, 1, var)
return(var_ypred / (var_ypred + var_e))
}

## OOS R2 Model 5:
weatheryearOOS <- as.data.frame(posterior_epred(weatheryear, newdata= clustertest,
                                        allow_new_levels=T))

weatheryear.r2 <- r2(weatheryearOOS, clustertest$meansage10)
weatheryear.r2mean<- mean(weatheryear.r2)
weatheryear.r2se <- sd(weatheryear.r2) / sqrt(length(weatheryearOOS))

## OOS R2 Model 4:
weatherOOS <- as.data.frame(posterior_epred(weather, 
                newdata= clustertest,allow_new_levels=T))

weather.r2 <- r2(weatherOOS, clustertest$meansage10)
weather.r2mean <- mean(weather.r2)
weather.r2se <- sd(weather.r2) / sqrt(length(weatherOOS))


## OOS R2 Model 3:
weather1OOS <- as.data.frame(posterior_epred(weather1, 
                                            newdata= clustertest,allow_new_levels=T))

weather1.r2 <- r2(weather1OOS, clustertest$meansage10)
weather1.r2mean <- mean(weather1.r2)
weather1.r2se <- sd(weather1.r2) / sqrt(length(weather1OOS))


## OOS r2 Model 2:
climateOOS <- as.data.frame(posterior_epred(climate, 
                                            newdata= clustertest,allow_new_levels=T))
climate.r2 <- r2(climateOOS, clustertest$meansage10)
climate.r2mean <- mean(climate.r2)
climate.r2se <- sd(climate.r2) / sqrt(length(climateOOS))


## OOS r2 Model 1:
staticOOS <- as.data.frame(posterior_epred(static, 
                                            newdata= clustertest,allow_new_levels=T))
static.r2 <- r2(staticOOS, clustertest$meansage10)
static.r2mean <- mean(static.r2)
static.r2se <- sd(static.r2) / sqrt(length(staticOOS))

#Save OOS R2 in one table:
OOSrsq <- data.frame(
  c(static.r2mean, static.r2se),
  c(climate.r2mean, climate.r2se),
  c(weather1.r2mean, weather1.r2se),
  c(weather.r2mean, weather.r2se),
  c(weatheryear.r2mean, weatheryear.r2se))
colnames(OOSrsq) <- c("Model1", "Model2", "Model3",
                      "Model4", "Model5")
rownames(OOSrsq) <- c("Mean R2", "SE R2"
                      )

write.csv(OOSrsq, "tables/OOSpredictR2.csv")



## Code for Figure 2: How prediction varies across values of sagebrush cover ####

weatheryearOOS <- as.data.frame(predict(weatheryear, newdata= clustertest,
                                        allow_new_levels=T))
weatheryearOOS <- cbind(weatheryearOOS, clustersub)
weatheryearOOS$model <- "Year + Weather + Climate added (Model 5)"

weatherOOS <- as.data.frame(predict(weather, newdata= clustertest))
weatherOOS <- cbind(weatherOOS, clustersub)
weatherOOS$model <- "Weather + Climate added (Model 4)"

weather1OOS <- as.data.frame(predict(weather1, newdata= clustertest))
weather1OOS <- cbind(weather1OOS, clustersub)
weather1OOS$model <- "Weather yr 1 + Climate added (Model 3)"

speiOOS <- as.data.frame(predict(spei, newdata= clustertest))
speiOOS <- cbind(speiOOS, clustersub)
speiOOS$model <- "SPEI + Climate added"

climateOOS <- as.data.frame(predict(climate, newdata= clustertest))
climateOOS <- cbind(climateOOS, clustersub)
climateOOS$model <- "Long-term climate averages added (Model 2)"

staticOOS <- as.data.frame(predict(static, newdata= clustertest))
staticOOS <- cbind(staticOOS, clustersub)
staticOOS$model <- "Biophysical (static) variables only (Model 1)"


preds <- rbind(weatheryearOOS, weatherOOS, climateOOS, staticOOS)

preds$resid <- preds$Estimate - preds$meansage10



predaccuracy <-   ggplot(preds, 
                         aes(x = meansage10, y = Estimate-meansage10,
                             color=model, fill=model)) +
  geom_smooth(size=1, method="lm", level=0.95) +
  geom_hline(yintercept = 0, linetype = 3, size=1, color = "grey20")+
  scale_fill_manual(values=c(blue,teal, lightorange, red),
                    name="Covariates included in model")+
  scale_color_manual(values=c(blue, teal, lightorange, red),
                     name="Covariates included in model") +
  xlab("Observed sagebrush percent cover\n10 years after treatment") + 
  ylab("Predictive Error\n(Predicted - observed sagebrush cover)")+ 
  theme_bw() + theme(legend.position=c(0.3, 0.25))

predaccuracy


tiff('figures/figure2_predictiveerror_final.tiff',
     width=6, height=4.5, units="in", res=800
)
predaccuracy
dev.off()



