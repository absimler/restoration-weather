## Code for: Interannual variation in climate contributes to contingency in post-fire restoration outcomes in seeded sagebrush steppe ####
## Simler-Williamson, Applestein, & Germino ###
## Final v. May 2022 ##

## 4)  Figures associated with Question 3 (Figs 3-4) #####

#################################

### Colors for figs:
orange <- "#DF4C20"
red <- "#A32C18"
darkgrey <- "grey20"
lightgrey <-"#a4a4a4"
teal <-   "#02606c"
lightteal <-   "#6f9ca5"
blue <- "#052c6e"

anngrowth2 <- readRDS("modelfits/anngrowth_se.rds")



### Fig 3. Parameter plots ##################

posterior <- as.data.frame(anngrowth2)%>%
  dplyr::select(contains(c("b_", "sd")))
posterior <- as.data.frame(t(posterior)) 
posterior$variable <- rownames(posterior)
posterior$color <- c("std", "std", 
                     "ppt", "ppt", 
                     "max", "max",
                     "min", "min", 
                     "ppt","max", "min", 
                     "ppt",  "ppt", "ppt", "max", "min", 
                     "std", "std")
posterior$reorder <- c(18, 17, 4, 3, 6, 5,8,7, 
                       12, 14, 16, 11, 10, 9, 13, 15, 1, 2)
posterior$variable2 <- c("Intercept", "Previous cover (t-1)",
                            "Mean PPT",
                            "Mean PPT\n(quadratic)",
                            "Mean MAX TEMP",
                            "Mean MAX TEMP\n(quadratic)",
                            "Mean MIN TEMP",
                            "Mean MIN TEMP\n(quadratic)",
                            "PPT deviation",
                            "MAX TEMP deviation",
                            "MIN TEMP deviation",
                            "Lag PPT deviation",
                            "PPT deviation*mean",
                            "Lag PPT deviation*mean",
                            "MAX TEMP deviation*mean",
                            "MIN TEMP deviation*mean",
                            "SD for site-level intercept", 
                            "SD for time since fire intercept")
posterior$type <- c("std", "std", 
                    "Climate effects", "Climate effects",
                    "Climate effects", "Climate effects",
                    "Climate effects", "Climate effects",
                    "Weather effects",  "Weather effects",
                    "Weather effects", "Weather effects",
                    "Weather effects","Weather effects",
                    "Weather effects", "Weather effects",
                    "std", "std")
posterior <- posterior%>% 
              tidyr::gather( draw, estimate, 
                             -variable, -variable2, 
                             -color, -type, -reorder) 

climpars <- ggplot(subset(posterior, color=="ppt" | 
                    color=="min" | color=="max"),
                   aes(y = reorder(variable2, reorder), x = estimate, color=color)) +
  stat_halfeye(.width=c(0.5, 0.9), fill="grey80") + 
  scale_color_manual(values=c( "#b22904",teal,"#14088E"))+
  geom_vline(xintercept=0, linetype = 3, size=1, color = darkgrey)+
  xlab("Effect of parameter on sagebrush cover at time t")+
  ylab("")+ facet_wrap(~type, scales="free") +
  theme_bw()+theme(legend.position="none") +
  theme(strip.text.x = element_text(size = 12), 
        axis.text.y = element_text( size=10),
        axis.title = element_text(size=12), 
        axis.text.x=element_text(size=10))
climpars

otherpars <-ggplot(subset(posterior, color=="std"),
                   aes(y = reorder(variable2, reorder), x = estimate)) +
  stat_halfeye(.width=c(0.5, 0.9), fill="grey80", color=darkgrey) +
  geom_vline(xintercept=0, linetype = 3, size=1, color = darkgrey)+
  xlab("Effect of parameter on sagebrush cover at time t")+
  ylab("")+
  xlim(0, 3.5)+
  theme_bw() +
  theme(strip.text.x = element_text(size = 12), 
        axis.text.y = element_text( size=10),
        axis.title = element_text(size=12), 
        axis.text.x=element_text(size=10))
otherpars


growthpars <- plot_grid(climpars, otherpars, ncol=1,
                        nrow=2, rel_heights = c(1, 0.5))

growthpars


### Figures 4-5: Marginal effects plots: ############

mytheme <- theme_bw() + theme(legend.title=element_blank(),
                              axis.text.y = element_text( size=12),
                              axis.title = element_text(size=12), 
                              axis.text.x=element_text(size=12),
                              legend.text = element_text(size=10)) 

ngrad <- 100

## Plots for minimum temperature: ############
## Effect of climate at different levels of weather
tmingrad <- rep(seq(from=min(growthtrain$tminavg, na.rm=T), 
                    to=max(growthtrain$tminavg),
                    length.out=ngrad), 3)
tminquant <- quantile(growthtrain$tmindev, probs=c(0.1, 0.9))
tmindevgrad <- c(rep(tminquant[1], ngrad), rep(0,ngrad), rep(tminquant[2], ngrad))

preds <- growthtrain %>%
  data_grid(sageprev = median(sageprev),
            tmaxavg = median(tmaxavg, na.rm=T),
            pptavg = median(pptavg, na.rm=T),
            tminavg = tmingrad,
            tmindev = tmindevgrad,
            pptdev = 0,
            tmaxdev = 0,
            pptlagdev = 0,
            se = 0) %>%
          add_epred_draws(anngrowth2, draws=500, re_formula=NA)


me.tmin <-ggplot(preds, aes(x = tminavg, y = (.epred -sageprev) / sageprev, 
                            color=as.factor(tmindev), fill=as.factor(tmindev))) +
  stat_lineribbon(.width = c(.5), alpha=0.4, size=2) +
  stat_lineribbon(.width = c(.01), size=2) +
  scale_fill_manual(values=c( teal,  darkgrey, orange), 
                    labels=c("Cooler years (10th per.)",
                             "Average years", "Warmer years (90th per.)"))+
  scale_color_manual(values=c( teal, darkgrey, orange), 
                     labels=c("Cooler years (10th per.)",
                              "Average years", "Warmer years (90th per.)"))+
  xlab("Site mean (30 year) minimum spring temperature") + 
  ylab("Annual % change in sagebrush cover") +   mytheme +
  theme(legend.position=c(0.7, 0.85))


me.tmin


## Effect of weather at different levels of climate:
tmindevgrad <- rep(seq(from=min(growthtrain$tmindev, na.rm=T), 
                       to=max(growthtrain$tmindev),
                       length.out=ngrad), 2)
tminquant <- quantile(growthtrain$tminavg, probs=c(0.1, 0.9))
tminavggrad <- c(rep(tminquant[1], ngrad), rep(tminquant[2], 2))

preds <- growthtrain %>%
  data_grid(sageprev = median(sageprev),
            tmaxavg = median(tmaxavg, na.rm=T),
            pptavg = median(pptavg, na.rm=T),
            tmindev = tmindevgrad,
            tminavg = tminavggrad,
            pptdev = 0,
            tmaxdev = 0,
            pptlagdev = 0,
            se=0) %>%
  add_epred_draws(anngrowth2, draws=500, re_formula=NA)


me.tmin2 <-   ggplot(preds, aes(x = tmindev, y = (.epred -sageprev) / sageprev, 
                                color=as.factor(tminavg), fill=as.factor(tminavg))) +
  stat_lineribbon(.width = c(.5), alpha=0.4, size=2) +
  stat_lineribbon(.width = c(.01), size=2) +
  scale_fill_manual(values=c( blue, red),
                    labels=c("Cooler sites (10th per.)",
                             "Warmer sites (90th per.)"))+
  scale_color_manual(values=c( blue, red),
                     labels=c("Cooler sites (10th per.)",
                              "Warmer sites (90th per.)"))+
  geom_vline(xintercept = 0, linetype = 3, size=1, color = darkgrey) +
  xlab("Deviation from mean spring minimum temperature") + 
  ylab("Annual % change in sagebrush cover") +
  geom_segment(aes(x = 0, y =0.75, xend = -18, yend =0.75),
               arrow = arrow(length = unit(0.5, "cm")),color=blue, size=2) +
  geom_segment(aes(x = 0, y = 0.75, xend = 8, yend = 0.75),
               arrow = arrow(length = unit(0.5, "cm")), color=red, size=2) +
  annotate(geom="text", x=4, y=0.67, label="Warmer than avg.",
           color=red, fontface="bold")+
  annotate(geom="text", x=-9, y=0.67, label="Cooler than avg.",
           color=blue, fontface="bold")+
  ylim(-0.1, 0.8)+
  mytheme +
  theme(legend.position=c(0.3, 0.15))

me.tmin2



## Plots for Maximum temperature #####

# Effect of climate at different levels of weather
tmaxgrad <- rep(seq(from=min(growthtrain$tmaxavg, na.rm=T), 
                    to=max(growthtrain$tmaxavg),
                    length.out=ngrad), 3)
tmaxquant <- quantile(growthtrain$tmaxdev, probs=c(0.1, 0.9))
tmaxdevgrad <- c(rep(tmaxquant[1], ngrad), rep(0,ngrad), rep(tmaxquant[2], ngrad))

preds <- growthtrain %>%
  data_grid(sageprev = median(sageprev),
            tminavg = median(tminavg, na.rm=T),
            pptavg = median(pptavg, na.rm=T),
            tmaxavg = tmaxgrad,
            tmaxdev = tmaxdevgrad,
            pptdev = 0,
            tmindev = 0,
            pptlagdev = 0,
            se=0) %>%
  add_epred_draws(anngrowth2, draws=500, re_formula=NA)


me.tmax <-   ggplot(preds, aes(x = tmaxavg, y = (.epred -sageprev) / sageprev, 
                               color=as.factor(tmaxdev), fill=as.factor(tmaxdev))) +
  stat_lineribbon(.width = c(.5), alpha=0.4, size=2) +
  stat_lineribbon(.width = c(.01), size=2) +
  scale_fill_manual(values=c( teal,  darkgrey, orange), 
                    labels=c("Cooler years (10th per.)",
                             "Average years", "Warmer years (90th per.)"))+
  scale_color_manual(values=c( teal, darkgrey, orange), 
                     labels=c("Cooler years (10th per.)",
                              "Average years", "Warmer years (90th per.)"))+
  xlab("Site mean (30 year) maximum spring temperature") + 
  ylab("Annual % change in sagebrush cover") +
  mytheme +
  theme(legend.position=c(0.42, 0.15))

me.tmax




## Effect of weather at different levels of climate:
tmaxdevgrad <- rep(seq(from=min(growthtrain$tmaxdev, na.rm=T), 
                       to=max(growthtrain$tmaxdev),
                       length.out=ngrad), 2)
tmaxquant <- quantile(growthtrain$tmaxavg, probs=c(0.1, 0.9))
tmaxavggrad <- c(rep(tmaxquant[1], ngrad), rep(tmaxquant[2], 2))

preds <- growthtrain %>%
  data_grid(sageprev = median(sageprev),
            tminavg = median(tminavg, na.rm=T),
            pptavg = median(pptavg, na.rm=T),
            tmaxdev = tmaxdevgrad,
            tmaxavg = tmaxavggrad,
            pptdev = 0,
            tmindev = 0,
            pptlagdev =0,
            se=0) %>%
  add_epred_draws(anngrowth2, draws=500, re_formula=NA)


me.tmax2 <- ggplot(preds, aes(x = tmaxdev, y = (.epred -sageprev) / sageprev, 
                              color=as.factor(tmaxavg), fill=as.factor(tmaxavg))) +
  stat_lineribbon(.width = c(.5), alpha=0.4, size=2) +
  stat_lineribbon(.width = c(.01), size=2) +
  scale_fill_manual(values=c( blue, red),
                    labels=c("Cooler sites (10th per.)",
                             "Warmer sites (90th per.)"))+
  scale_color_manual(values=c( blue, red),
                     labels=c("Cooler sites (10th per.)",
                              "Warmer sites (90th per.)"))+
  geom_vline(xintercept = 0, linetype = 3, size=1, color = darkgrey) +
  xlab("Deviation from mean spring maximum temperature") + 
  ylab("Annual % change in sagebrush cover") +
  geom_segment(aes(x = 0, y = 0.85, xend = min(tmaxdev), yend = 0.85),
               arrow = arrow(length = unit(0.5, "cm")),color=blue, size=2) +
  geom_segment(aes(x = 0, y = 0.85, xend = max(tmaxdev), yend = 0.85),
               arrow = arrow(length = unit(0.5, "cm")), color=red, size=2) +
  annotate(geom="text", x=4.5, y=0.75, label="Warmer than avg.",
           color=red, fontface="bold")+
  annotate(geom="text", x=-8, y=0.75, label="Cooler than avg.",
           color=blue, fontface="bold")+
  mytheme +
  ylim(-0.45,0.9)+
  theme(legend.position=c(0.25, 0.15))

me.tmax2




##Plots for precipitation #####

pptgrad <- rep(seq(from=min(growthtrain$pptavg, na.rm=T), 
                   to=max(growthtrain$pptavg),
                   length.out=500), 3)
pptquant <- quantile(growthtrain$pptdev, probs=c(0.1, 0.9))
pptdevgrad <- c(rep(pptquant[1], 500), rep(0,500), rep(pptquant[2], 500))

preds <- growthtrain %>%
  data_grid(sageprev = median(sageprev),
            tminavg = median(tminavg, na.rm=T),
            tmaxavg = median(tmaxavg, na.rm=T),
            pptavg = pptgrad,
            pptdev = pptdevgrad,
            tmaxdev = 0,
            tmindev = 0,
            pptlagdev =0,
            se=0) %>%
  add_epred_draws(anngrowth2, draws=500, re_formula=NA)


me.ppt <-   ggplot(preds, aes(x = pptavg, y = (.epred -sageprev) / sageprev, 
                              color=as.factor(pptdev), fill=as.factor(pptdev))) +
  stat_lineribbon(.width = c(.5), alpha=0.4, size=2) +
  stat_lineribbon(.width = c(.01), size=2) +
  scale_fill_manual(values=c(orange, darkgrey, teal), 
                    labels=c("Drier years (10th per.)",
                             "Average years", "Wetter years (90th per.)"))+
  scale_color_manual(values=c(orange, darkgrey, teal),
                     labels=c("Drier years (10th per.)",
                              "Average years", "Wetter years (90th per.)"))+
  xlab("Site mean (30 year) total spring precipitation") + 
  ylab("Annual % change in sagebrush cover") +
  mytheme +
  theme(legend.position=c(0.5, 0.15))

me.ppt


## Effect of weather at different levels of climate:
pptdevgrad <- rep(seq(from=min(growthtrain$pptdev, na.rm=T), 
                      to=max(growthtrain$pptdev),
                      length.out=ngrad), 2)
pptquant <- quantile(growthtrain$pptavg, probs=c(0.1, 0.9))
pptavggrad <- c(rep(pptquant[1], ngrad), rep(pptquant[2], 2))

preds <- growthtrain %>%
  data_grid(sageprev = median(sageprev),
            tmaxavg = median(tmaxavg, na.rm=T),
            tminavg = median(tminavg, na.rm=T),
            pptdev = pptdevgrad,
            pptavg = pptavggrad,
            tmindev = 0,
            tmaxdev = 0,
            pptlagdev = 0,
            se=0) %>%
  add_epred_draws(anngrowth2, draws=500, re_formula=NA)


me.ppt2 <-   ggplot(preds, aes(x = pptdev, y = (.epred -sageprev) / sageprev, 
                               color=as.factor(pptavg), fill=as.factor(pptavg))) +
  stat_lineribbon(.width = c(.5), size=2, alpha=0.4) +
  stat_lineribbon(.width = c(.01), size=2) +
  scale_fill_manual(values=c(red, blue),
                    labels=c("Drier sites (10th per.)",
                             "Wetter sites (90th per.)"))+
  scale_color_manual(values=c(red, blue),
                     labels=c("Drier sites (10th per.)",
                              "Wetter sites (90th per.)"))+
  geom_vline(xintercept = 0, linetype = 3, size=1, color = darkgrey) +
  xlab("Deviation from mean spring precipitation") + 
  ylab("Annual % change in sagebrush cover") +
  geom_segment(aes(x = 0, y = 0.5, xend = min(pptdev), 
                   yend = 0.5),
               arrow = arrow(length = unit(0.5, "cm")),
               color=red, size=2) +
  geom_segment(aes(x = 0, y = 0.5, 
                   xend = max(pptdev), yend = 0.5),
               arrow = arrow(length = unit(0.5, "cm")), 
               color=blue, size=2) +
  annotate(geom="text", x=75, y=0.45, label="Wetter than avg.",
           color=blue, fontface="bold")+
  annotate(geom="text", x=-75, y=0.45, label="Drier than avg.",
           color=red, fontface="bold")+
  mytheme +
  theme(legend.position=c(0.75, 0.15))

me.ppt2


###  Plots for lagged precipitation #####

pptgrad <- rep(seq(from=min(growthtrain$pptavg, na.rm=T), 
                   to=max(growthtrain$pptavg),
                   length.out=500), 3)
pptquant <- quantile(growthtrain$pptlagdev, probs=c(0.1, 0.9))
pptdevgrad <- c(rep(pptquant[1], 500), rep(0,500), rep(pptquant[2], 500))

preds <- growthtrain %>%
  data_grid(sageprev = median(sageprev),
            tminavg = median(tminavg, na.rm=T),
            tmaxavg = median(tmaxavg, na.rm=T),
            pptavg = pptgrad,
            pptdev = 0,
            tmaxdev = 0,
            tmindev = 0,
            pptlagdev =pptdevgrad,
            se=0) %>%
  add_epred_draws(anngrowth2, draws=500, re_formula=NA)


me.ppt.lag <-   ggplot(preds, aes(x = pptavg, y = (.epred -sageprev) / sageprev, 
                              color=as.factor(pptlagdev), fill=as.factor(pptlagdev))) +
  stat_lineribbon(.width = c(.5), alpha=0.4, size=2) +
  stat_lineribbon(.width = c(.01), size=2) +
  scale_fill_manual(values=c(orange, darkgrey, teal), 
                    labels=c("Drier previous years (10th per.)",
                             "Average previous years", 
                             "Wetter previous years (90th per.)"))+
  scale_color_manual(values=c(orange, darkgrey, teal),
                     labels=c("Drier previous years (10th per.)",
                              "Average previous years", "Wetter previous years (90th per.)"))+
  xlab("Site mean (30 year) total spring precipitation") + 
  ylab("Annual % change in sagebrush cover") +
  mytheme +
  theme(legend.position=c(0.5, 0.15))

me.ppt.lag


## Effect of weather at different levels of climate:
pptdevgrad3 <- rep(seq(from=min(growthtrain$pptlagdev, na.rm=T), 
                      to=max(growthtrain$pptlagdev),
                      length.out=ngrad), 2)
pptquant3 <- quantile(growthtrain$pptavg, probs=c(0.1, 0.9))
pptavggrad <- c(rep(pptquant3[1], ngrad), rep(pptquant3[2], 2))

preds <- growthtrain %>%
  data_grid(sageprev = median(sageprev),
            tmaxavg = median(tmaxavg, na.rm=T),
            tminavg = median(tminavg, na.rm=T),
            pptdev = 0,
            pptavg = pptavggrad,
            tmindev = 0,
            tmaxdev = 0,
            pptlagdev = pptdevgrad3,
            se=0) %>%
  add_epred_draws(anngrowth2, draws=500, re_formula=NA)


me.ppt.lag2 <-   ggplot(preds, aes(x = pptlagdev, y = (.epred -sageprev) / sageprev, 
                               color=as.factor(pptavg), fill=as.factor(pptavg))) +
  stat_lineribbon(.width = c(.5), size=2, alpha=0.4) +
  stat_lineribbon(.width = c(.01), size=2) +
  scale_fill_manual(values=c(red, blue),
                    labels=c("Drier sites (10th per.)",
                             "Wetter sites (90th per.)"))+
  scale_color_manual(values=c(red, blue),
                     labels=c("Drier sites (10th per.)",
                              "Wetter sites (90th per.)"))+
  geom_vline(xintercept = 0, linetype = 3, size=1, color = darkgrey) +
  xlab("Previous year deviation from mean spring precipitation") + 
  ylab("Annual % change in sagebrush cover") +
  geom_segment(aes(x = 0, y = 0.85, xend = min(pptlagdev), 
                   yend = 0.85),
               arrow = arrow(length = unit(0.5, "cm")),
               color=red, size=2) +
  geom_segment(aes(x = 0, y = 0.85, 
                   xend = max(pptlagdev), yend = 0.85),
               arrow = arrow(length = unit(0.5, "cm")), 
               color=blue, size=2) +
  annotate(geom="text", x=75, y=0.75, label="Wetter than avg.",
           color=blue, fontface="bold")+
  annotate(geom="text", x=-75, y=0.75, label="Drier than avg.",
           color=red, fontface="bold")+
  mytheme +
  theme(legend.position=c(0.75, 0.15))

me.ppt.lag2

##### Combine panel plots:  ####

growth.temp <- plot_grid(me.tmax, me.tmax2,  me.tmin,
                           me.tmin2 , 
                           ncol=2, nrow=2, scale=0.95,
                           labels=c("A", "B", "C", "D"))
growth.temp

growth.ppt <- plot_grid(me.ppt, me.ppt2,  me.ppt.lag,
                         me.ppt.lag2 , 
                         ncol=2, nrow=2, scale=0.95,
                         labels=c("A", "B", "C", "D"))
growth.ppt




tiff('figures/figure3_growth_parsSE.tiff',
     width=8, height=6, units="in", res=800
)
growthpars
dev.off()


tiff('figures/figure4_tempgrowth_margeffSE.tiff',
     width=10, height=8.2, units="in", res=800
)
growth.temp
dev.off()


tiff('figures/figure5_pptgrowth_margeffSE.tiff',
     width=10, height=8.2, units="in", res=800
)
growth.ppt
dev.off()
