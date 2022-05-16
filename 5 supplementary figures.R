## Code for: Interannual variation in climate contributes to contingency in post-fire restoration outcomes in seeded sagebrush steppe ####
## Simler-Williamson, Applestein, & Germino ###
## Final v. May 2022 ##

## 5) Other figures (Figure 6 & Supplement) #####


## Supplement: Posterior parameter estimates for static biophysical parameters: ####

posterior <- as.data.frame(static)%>%
  dplyr::select(contains(c("b_")))
posterior <- as.data.frame(t(posterior)) 
posterior$variable <- rownames(posterior)
posterior$reorder <- c(12, 7, 6, 5, 4, 3, 2, 1,
                       11, 10, 9, 8)
posterior$variable2 <- c("Intercept", 
                         "Central Basin & Range ER",
                         "Northern Basin & Range ER",
                         "Snake River Plain ER",
                         "Wyoming Basin ER",
                         "Blue Mountains ER",
                         "Idaho Batholith ER",
                         "Middle Rockies ER",
                         "Mean Slope",
                         "Mean Elevation",
                         "Mean Heatload",
                         "Mean Surviving Sagebrush\nCover in Year 1")
posterior <- posterior%>% 
  gather( draw, estimate, -variable, -variable2, -reorder) 

staticpars <-ggplot(subset(posterior),
                   aes(y = reorder(variable2, reorder), x = estimate)) +
  stat_pointinterval(.width=c(0.5, 0.9), fill="grey80", color=teal) +
  geom_vline(xintercept=0, linetype = 3, size=1, color = darkgrey)+
  xlab("Effect of parameter on sagebrush cover
       10 years following treatment")+
  ylab("")+
  theme_bw() +
  theme(strip.text.x = element_text(size = 12), 
        axis.text.y = element_text( size=10),
        axis.title = element_text(size=12), 
        axis.text.x=element_text(size=10))
staticpars




## Figure 6: Varying intercept components for Fire Year ####
posterior <- as.data.frame(weatheryear)%>%
  dplyr::select(contains(c("r_fireyr")))
posterior <- as.data.frame(t(posterior)) 
posterior$variable <- rownames(posterior)
posterior$reorder <- c(1:21)
posterior$variable2 <- c(1984:2001, 2003:2005)
posterior <- posterior%>% 
  gather( draw, estimate, -variable, -variable2, -reorder) 

yearpars <-ggplot(subset(posterior),
                    aes(x = reorder(variable2, reorder), y = estimate)) +
  stat_pointinterval(.width=c(0.5, 0.9), color=blue)+
  geom_hline(yintercept=0, linetype = 3, size=1, color = darkgrey)+
  ylab("Effect on sagebrush cover\n10 years following treatment")+
  xlab("Year of fire")+
  scale_color_brewer() +
  theme_bw() +
  theme(strip.text.x = element_text(size = 12), 
        axis.text.y = element_text( size=10),
        axis.title = element_text(size=12), 
        axis.text.x=element_text(size=10, angle=90)) 
yearpars



## Save plots ###
tiff('figures/figure6_yearvary.tiff',
     width=7, height=4, units="in", res=800
)
yearpars
dev.off()


tiff('figures/supp_staticpars.tiff',
     width=7, height=4, units="in", res=800
)
staticpars
dev.off()
