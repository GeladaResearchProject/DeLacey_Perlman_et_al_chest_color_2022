# load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(extrafont)
library(lme4) 
library(lmerTest) 
library(stringr)
library(irr)

# prep transparent colors
makeTransparent <- function(black, alpha = 200){
  newColor <- col2rgb(black)
  apply(newColor, 2, function(curcoldata)			
  {rgb(red = curcoldata[1],
       green = curcoldata[2],
       blue = curcoldata[3],
       alpha = alpha,
       maxColorValue=  255)})
}
# Call transparent colors 
tBlack <- makeTransparent("black")
tRed <-  makeTransparent("red")
tSkin <- makeTransparent("gray30")
tModRed <- makeTransparent("violetred3")
tGreen <- makeTransparent("darkgreen")
tBlue <- makeTransparent("blue")

# load data
load(file="/Users/patsydelacey/Desktop/DeLacey_Perlman_et_al_chest_color_2022.RData")

#############################################################################
###                 Figure 2: Method consistency                          ###
#############################################################################

# data frames and values used:
# Fig 2a
# adobe_lighting_sum = data frame, adobe method summarized across lighting condition for each square 
# avg_ls_rg_adobe = value, average of adobe values for the light skin square across all lighting conditions
# avg_mr_rg_adobe = value, average of adobe values for the moderate red square across all lighting conditions
# Fig 2b
# imagej_lighting_sum = data frame, imagej micaToolbox method R-G Opponency and Luminance summarized across lighting condition for each square
# avg_ls_rgopp_ij = value, average of imagej micaToolbox R-G Opponency values for the light skin square across all lighting conditions
# avg_mr_rgopp_ij = value, average of imagej micaToolbox R-G Opponency values for the moderate red square across all lighting conditions
# Fig 2c
# imagej_lighting_sum (see above)
# avg_ls_lum_ij = value, average of imagej micaToolbox luminance values for the light skin square across all lighting conditions
# avg_mr_lum_ij = value, average of imagej micaToolbox luminance values for the moderate red square across all lighting conditions
# Stats
# adobe_lighting_ls_long = data frame, adobe method measurements for four lighting conditions for light skin
# adobe_lighting_mr_long = data frame, adobe method measurements for four lighting conditions for moderate red square
# imagej_lighting_ls_long = data frame, imagej micaToolbox method measurements for four lighting conditions for light skin square
# imagej_lighting_mr_long = data frame, imagej micaToolbox method measurements for four lighting conditions for moderate red square


### Run stats for Figure 2
# ADOBE METHOD # 
# redness across light condition for Adobe, light skin square
# reorder variables
adobe_lighting_ls_long$lighting <- factor(adobe_lighting_ls_long$lighting, 
                                          levels = c("cloud","backlit", "shade", "sun"))
# run linear model
lm_adobe_ls <- lm(rg ~ lighting, data=adobe_lighting_ls_long)
summary(lm_adobe_ls)

# redness across light condition for Adobe, moderate red square
# reorder variables
adobe_lighting_mr_long$lighting <- factor(adobe_lighting_mr_long$lighting, 
                                          levels = c("cloud","backlit", "shade", "sun"))
# run linear model
lm_adobe_mr <- lm(rg ~ lighting, data=adobe_lighting_mr_long)
summary(lm_adobe_mr)

# IMAGEJ MICATOOLBOX METHOD R-G OPPONENCY # 
# redness across light condition for ImageJ micaToolbox, light skin square
# reorder variables
imagej_lighting_ls_long$lighting <- factor(adobe_lighting_ls_long$lighting, 
                                           levels = c("cloud","backlit", "shade", "sun"))
# run linear model
lm_imagej_rgopp_ls <- lm(rg_opp ~ lighting, data=imagej_lighting_ls_long)
summary(lm_imagej_rgopp_ls)
# redness across light condition for ImageJ micaToolbox, moderate red square
# reorder variables
imagej_lighting_mr_long$lighting <- factor(adobe_lighting_mr_long$lighting, 
                                           levels = c("cloud","backlit", "shade", "sun"))
# run linear model
lm_imagej_rgopp_mr <- lm(rg_opp ~ lighting, data=imagej_lighting_mr_long)
summary(lm_imagej_rgopp_mr)

# IMAGEJ MICATOOLBOX METHOD LUMINANCE # 
# luminance across light condition for ImageJ micaToolbox, light skin square
# run linear model
lm_imagej_lum_ls <- lm(lum ~ lighting, data=imagej_lighting_ls_long)
summary(lm_imagej_lum_ls)
# luminance across light condition for ImageJ micaToolbox, moderate red square
# run linear model
lm_imagej_lum_mr <- lm(lum ~ lighting, data=imagej_lighting_mr_long)
summary(lm_imagej_lum_mr)

### Calculate magnitude of differences
# Adobe method light skin
ad_ls_sum <- adobe_lighting_ls_long %>%
  group_by(lighting) %>%
  summarise(mean_rg = mean(rg))
# backlit - could
backlit_cloud_diff_ad <- 1.250397 - 1.198838
# moderate red global mean - light skin global mean adobe
global_diff_ad <- avg_mr_rg_adobe - avg_ls_rg_adobe
# find ratio of biggest difference to global diff between patches 
(backlit_cloud_diff_ad/global_diff_ad)*100

# ImageJ micaToolbox method light skin square
ij_ls_sum <- imagej_lighting_ls_long %>%
  group_by(lighting) %>%
  summarise(mean_rg = mean(rg_opp))
# find cloud and backlit difference
cloud_backlit_diff_ij <- 	0.09785329 - 0.08937750
# moderate red global mean - light skin global mean imagej
global_diff_ij <- avg_mr_rgopp_ij - avg_ls_rgopp_ij
# find ratio of biggest difference to global diff between patches 
(cloud_backlit_diff_ij/global_diff_ij)*100

# plot figure 2a
# find range for graphing
summary(adobe_lighting_sum$rg_mean)
# reorder factors 
adobe_lighting_sum$lighting <- factor(adobe_lighting_sum$lighting, levels = c("cloud (intercept)", "backlit", "shade", "sun"))
# Call plot for Red/Green across lighting condition for Adobe method
fig2a <- ggplot(adobe_lighting_sum, aes(x=lighting, y=rg_mean,col=square)) +
  geom_errorbar(aes(ymin=rg_mean-rg_SEM, ymax=rg_mean+rg_SEM),
                size=1, 
                width=0.1)+
  geom_point(aes(shape=square,color=square),size = 5)
# Call plot with extra graphing parameters
fig2a <- fig2a + xlab("") + 
  labs(col="X-Rite ColorChecker Classic Square",shape="X-Rite ColorChecker Classic Square") +
  ylab("Adobe: Red/Green") + 
  theme(panel.background = element_blank()) + 
  theme(axis.line = element_line(color = "black")) + 
  scale_y_continuous(limits = c(1, 2.5), breaks = seq(1, 2.5, 0.5)) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width=10)) + 
  scale_color_manual(values = c(tSkin, tModRed)) +
  scale_shape_manual(values =c(15,17)) + 
  geom_hline(yintercept=avg_ls_rg_adobe, linetype="dashed", col=tSkin) + 
  geom_hline(yintercept=avg_mr_rg_adobe , linetype="dashed", col=tModRed) + 
  ggtitle("")  + 
  theme(plot.margin=unit(c(0.5,1,0.5,1),"cm"),
        axis.text=element_text(size=14,
                               family = 'Arial'),
        axis.title.y = element_text(size=16,
                                    family = 'Arial',
                                    vjust=3),
        panel.background = element_blank(),
        axis.line=element_line(color = "black"),
        plot.title=element_text(size=18),
        legend.title = element_text(size=16, 
                                    face="bold",
                                    family = 'Arial'),
        legend.text = element_text(size=16,
                                   family = 'Arial'),
        legend.position="top")
fig2a

stat.test.2a <- compare_means(rg ~ lighting, data=adobe_lighting_ls_long, method= "t.test")
# add in asterisks for cloud and backlit for light skin square
fig2a <- fig2a + geom_bracket(aes(xmin="cloud (intercept)", 
                     xmax="backlit", 
                     label= "***"),
                     data = stat.test.2a,
                     y.position = 1.5,
               inherit.aes = FALSE)
fig2a

# plot figure 2b
# find range for graphing
summary(imagej_lighting_sum$rg_opp_mean)
# reorder factors 
imagej_lighting_sum$lighting <- factor(imagej_lighting_sum$lighting, levels = c("cloud (intercept)", "backlit", "shade", "sun"))
# Call plot for RG-Opponency across lighting condition for ImageJ 
fig2b <- ggplot(imagej_lighting_sum, aes(x=lighting, y=rg_opp_mean,col=square)) +
  geom_errorbar(aes(ymin=rg_opp_mean-rg_opp_SEM, ymax=rg_opp_mean+rg_opp_SEM),
                size=1, 
                width=0.1)+
  geom_point(aes(shape=square,color=square),size = 5)
# Call plot with extra graphing parameters
fig2b <- fig2b + xlab("") + 
  labs(col="X-Rite ColorChecker Classic Square",shape="X-Rite ColorChecker Classic Square") +
  ylab("ImageJ: Red-Green Opponency") + 
  theme(panel.background = element_blank()) + 
  theme(axis.line = element_line(color = "black")) + 
  scale_y_continuous(limits = c(.05, .2), breaks = seq(.05, .2, 0.05)) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width=10)) + 
  scale_color_manual(values = c(tSkin, tModRed)) +
  scale_shape_manual(values =c(15,17)) + 
  geom_hline(yintercept=avg_ls_rgopp_ij, linetype="dashed", col=tSkin) + 
  geom_hline(yintercept=avg_mr_rgopp_ij, linetype="dashed", col=tModRed) + 
  ggtitle("")  + 
  theme(plot.margin=unit(c(0.5,1,0.5,1),"cm"),
        axis.text=element_text(size=14,
                               family = 'Arial'),
        axis.title.y = element_text(size=16,
                                    family = 'Arial',
                                    vjust=3),
        panel.background = element_blank(),
        axis.line=element_line(color = "black"),
        plot.title=element_text(size=18,
                                family = 'Arial'),
        legend.title = element_text(size=16, 
                                    face="bold",
                                    family = 'Arial'),
        legend.text = element_text(size=16,
                                   family = 'Arial'),
        legend.position="top")
fig2b

# prep for adding significance
stat.test.2b.ls <- compare_means(rg_opp ~ lighting, data = imagej_lighting_ls_long)
stat.test.2b.mr <- compare_means(rg_opp ~ lighting, data = imagej_lighting_mr_long)
# add in asterisks for cloud and backlit for light skin square
fig2b <- fig2b + geom_bracket(aes(xmin="cloud (intercept)", 
                         xmax="backlit", 
                         label= "***"),
                     data = stat.test.2b.mr,
                     y.position = 0.12,
                     inherit.aes = FALSE) +
  geom_bracket(aes(xmin="cloud (intercept)",
                   xmax="shade", 
                   label="*"),
                   data=stat.test.2b.mr,
                   y.position = 0.19,
               inherit.aes = FALSE) +
  geom_bracket(aes(xmin="cloud (intercept)",
                   xmax="sun", 
                   label="*"),
               data=stat.test.2b.mr,
               y.position = 0.20,
               inherit.aes = FALSE)
fig2b

# plot figure 2c
# find range for graphing
summary(imagej_lighting_sum$lum_mean)
# Call plot for RG-Opponency across lighting condition for ImageJ 
fig2c <- ggplot(imagej_lighting_sum, aes(x=lighting, y=lum_mean,col=square)) +
  geom_errorbar(aes(ymin=lum_mean-lum_SEM, ymax=lum_mean+lum_SEM),
                size=1, 
                width=0.1)+
  geom_point(aes(shape=square,color=square),size = 5)
# Call plot with extra graphing parameters
fig2c <- fig2c + 
  xlab("") + 
  labs(col="X-Rite ColorChecker Classic Square",shape="X-Rite ColorChecker Classic Square") +
  ylab("ImageJ: Luminance") + 
  scale_y_continuous(limits = c(.05, .35), 
                     breaks = seq(.05, .35, 0.1)) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width=10)) +
  scale_color_manual(values = c(tSkin, tModRed)) +
  scale_shape_manual(values =c(15,17)) + 
  geom_hline(yintercept=avg_ls_lum_ij, linetype="dashed", col=tSkin) + 
  geom_hline(yintercept=avg_mr_lum_ij, linetype="dashed", col=tModRed) + 
  ggtitle("")  + 
  theme(plot.margin=unit(c(0.5,1,0.5,1),"cm"),
        axis.text=element_text(size=14,
                               family = 'Arial'),
        axis.title.y = element_text(size=16,
                                    family = 'Arial',
                                    vjust=5),
        panel.background = element_blank(),
        axis.line=element_line(color = "black"),
        plot.title=element_text(size=18),
        legend.title = element_text(size=16, 
                                    face="bold",
                                    family = 'Arial'),
        legend.text = element_text(size=16,
                                   family = 'Arial'),
        legend.position="top") 
fig2c

# create multi-paneled plot for fig2a-c
fig2 <- ggarrange(fig2a, fig2b, fig2c,
                            nrow =1, ncol =3,
                            legend = "top",
                            common.legend = TRUE,
                            labels = c("A", "B", "C"),
                            font.label = list(size = 24,
                                              color = "black",
                                              face = "bold",
                                              family = 'Arial',
                                              vjust=1.5))
fig2

# save figure2
ggsave("figure2.pdf", plot = fig2, width=16, height=6, units="in", dpi=600, useDingbats=FALSE)
         
# remove data frames and objects
rm(adobe_lighting_ls_long, adobe_lighting_mr_long, adobe_lighting_sum,
   imagej_lighting_ls_long, imagej_lighting_mr_long, imagej_lighting_sum,
   avg_ls_rg_adobe, avg_mr_rg_adobe,
   avg_ls_rgopp_ij, avg_mr_rgopp_ij,
   avg_ls_lum_ij, avg_mr_lum_ij,
   lm_adobe_ls, lm_adobe_mr,
   lm_imagej_rgopp_ls, lm_imagej_rgopp_mr,
   lm_imagej_lum_ls, lm_imagej_lum_mr,
   fig2,fig2a, fig2b, fig2c,
   ad_ls_sum, backlit_cloud_diff_ad, global_diff_ad,
   ij_ls_sum, cloud_backlit_diff_ij, global_diff_ij,
   stat.test.2a, stat.test.2b.ls, stat.test.2b.mr)

#############################################################################
###                 Figure 3: Method accuracy                             ###
#############################################################################

# data frame used:
# method accuracy = data set with average subjective redness scores, Nix Red/Green, Adobe Red/Green, and ImageJ R-G Opponency for all images

# plot figure 3a
summary(method_accuracy$nix_rg) # find summary for graphing range
# Subjective redness vs. Nix scatterplot 
fig3a <- ggplot(method_accuracy,
                      aes(y=avg_subj_red,x=nix_rg,col=avg_subj_red)) +
  geom_point(size=5) + 
  ggtitle("") +
  scale_y_continuous(name="Subjective Redness Score",
                     limits=c(1,5),
                     breaks=c(1,2,3,4,5)) +
  scale_color_gradient(low=tRed,
                       high=tRed) + 
  scale_x_continuous(name="Nix: Red/Green",
                     limits=c(1.1,1.8),
                     breaks=seq(1.1,1.8,0.1)) + 
  geom_smooth(method=lm, se=FALSE, linetype="dashed",
              color=tBlack) +
  labs(col="Subjective Redness Score") + 
  theme(axis.text=element_text(size=18,
                               family = 'Arial'),
        axis.title=element_text(size=16,
                                family = 'Arial'),
        panel.background = element_blank(),
        axis.line=element_line(color = "black"),
        plot.title=element_text(size=24),
        legend.title=element_text(size=16,
                                  face="bold",
                                  family = 'Arial'),
        legend.text=element_text(size=16,
                                 'Arial'))
fig3a

# plot figure 3b
# Adobe v Nix 
summary(method_accuracy$ad_rg) # find range for graphing
fig3b <- ggplot(method_accuracy,
                    aes(y=ad_rg,x=nix_rg,col=ad_rg)) +
  geom_point(size=5) + 
  ggtitle("") +
  scale_y_continuous(name="Adobe: Red/Green",
                     limits=c(1.1,1.9),
                     breaks=seq(1.1,1.9,0.2)) +
  scale_color_gradient(low=tRed,
                       high=tRed) + 
  scale_x_continuous(name="Nix: Red/Green",
                     limits=c(1.1,1.8),
                     breaks= seq(1.1,1.8,0.1)) + 
  geom_smooth(method=lm, se=FALSE, linetype="dashed",
              color=tBlack) +
  labs(col="Adobe: Red/Green") + 
  theme(axis.text=element_text(size=18,
                               family = 'Arial'),
        axis.title=element_text(size=16,
                                family = 'Arial'),
        panel.background = element_blank(),
        axis.line=element_line(color = "black"),
        plot.title=element_text(size=24),
        legend.title=element_text(size=16,
                                  face="bold",
                                  family = 'Arial'),
        legend.text=element_text(size=16,
                                 family = 'Arial'))
fig3b

# plot figure 3c
# ImageJ v Nix 
summary(method_accuracy$ij_rg_opp) # find range for graphing
fig3c <- ggplot(method_accuracy,
                 aes(y=ij_rg_opp,x=nix_rg,col=ij_rg_opp)) +
  geom_point(size=5) + 
  ggtitle("") +
  scale_y_continuous(name="ImageJ: Red-Green Opponency",
                     limits=c(0.04,0.30),
                     breaks=seq(0.04,0.30,0.04)) +
  scale_color_gradient(low=tRed,
                       high=tRed) + 
  scale_x_continuous(name="Nix: Red/Green",
                     limits=c(1.1,1.8),
                     breaks= seq(1.1,1.8,0.1)) + 
  geom_smooth(method=lm, se=FALSE, linetype="dashed",
              color=tBlack) +
  labs(col="ImageJ: Red-Green Opponency") + 
  theme(axis.text=element_text(size=18,
                               family = 'Arial'),
        axis.title=element_text(size=16,
                                family = 'Arial'),
        panel.background = element_blank(),
        axis.line=element_line(color = "black"),
        plot.title=element_text(size=24,
                                family = 'Arial'),
        legend.title=element_text(size=16,
                                  face="bold",
                                  family = 'Arial'),
        legend.text=element_text(size=16,
                                 family = 'Arial'))
fig3c

# create multi-paneled plot for fig3a-c
fig3 <- ggarrange(fig3a, fig3b, fig3c,    
                         nrow =1, ncol =3,
                         legend = "none",
                         labels = c("A", "B", "C"), 
                         vjust = 1.5, 
                         font.label = list(size = 24, 
                                           color = "black", 
                                           face = "bold", 
                                           family = 'Arial'))
fig3

# save figure 3
ggsave("figure3.pdf", plot = fig3, width=16, height=6, units="in", dpi=600, useDingbats=FALSE)


### Run stats for Figure 3
# add three z-scored columns to method_accuracy, then run stats
# find summary stats for each metric
summary(method_accuracy$avg_subj_red)
sd(method_accuracy$avg_subj_red)

summary(method_accuracy$ad_rg)
sd(method_accuracy$ad_rg)

summary(method_accuracy$ij_rg_opp)
sd(method_accuracy$ij_rg_opp)

method_accuracy <- method_accuracy %>%
  mutate(avg_subj_red_z = ((avg_subj_red-2.591)/1.120749),
         ad_z = ((ad_rg-1.383)/0.1615402),
         ij_z = ((ij_rg_opp-0.13046)/0.05847613))

# run stats again with z-scores
# Fig3a: Nix Red/Green vs. subjective redness Z SCORES 
lm_nix_subjred_z <- lm(avg_subj_red_z ~ nix_rg, data=method_accuracy)
summary(lm_nix_subjred_z)

# Fig3b: Nix Red/Green vs. Adobe Red/Green Z SCORES 
lm_nix_adobe_z <- lm(ad_z ~ nix_rg, data=method_accuracy)
summary(lm_nix_adobe_z)

# panel C: Nix rg vs. ImageJ micaToolbox R-G Opponency Z SCORES
lm_nix_ij_z <- lm(ij_z ~ nix_rg, data=method_accuracy)
summary(lm_nix_ij_z)

# remove data frames to clean up the workspace
rm(method_accuracy,lm_nix_subjred_z, lm_nix_adobe_z, lm_nix_ij_z, fig3, fig3a, fig3b, fig3c)

#############################################################################
###    Intra-class correlation (ICC) for Subjective Redness               ###
#############################################################################

# data frame used:
# subj_red_icc = subjective redness scores of four observers

# run intra-class correlation between four observers
print(icc(subj_red_icc, model="twoway", type="consistency", unit="average"))

rm(subj_red_icc)

#############################################################################
###             Figure 4: Activity and Redness                            ###
#############################################################################

# data frames used:
# activity_redness = long data frame for the plot, paired baseline and postdisplay redness measured with Adobe
# activity_redness_ttest = summarized data frame for stats, paired baseline and postdisplay redness measured with Adobe

# plot figure 4
summary(activity_redness$rg) # find range for graphing
# call plot of paired baseline v post display redness
fig4 <- ggplot(data=activity_redness,
                        aes(x=activity, y=rg, group=set)) +
  geom_line(size=1, col=tRed) +
  geom_point(size=3, col=tBlack) +
  scale_x_discrete(labels=c("Baseline","Post-display"),
                   name="") +
  scale_y_continuous(name="Adobe: Red/Green",
                     limits = c(1.1, 2.1), breaks = seq(1.1,2.1,0.2)) +
  theme(axis.text=element_text(size=18,
                               family = "Arial"),
        axis.title=element_text(size=18,
                                family = "Arial"),
        panel.background = element_blank(),
        axis.line=element_line(color = "black"),
        plot.title=element_text(size=24,
                                family = "Arial"),
        legend.title=element_text(size=16,
                                  face="bold",
                                  family = "Arial"),
        legend.text=element_text(size=16,
                                 family = "Arial")) + 
  ggtitle("")
fig4

# save figure 4
ggsave("figure4.pdf", plot = fig4, width=8, height=6, units="in", dpi=600, useDingbats=FALSE)

### Run stats for Figure 4
# check normality for redness (rg)
# q-q plot for baseline
qplot(sample = activity_redness$rg[activity_redness$activity == "baseline"], stat = 'qq')
# q-q plot for postdisplay
qplot(sample = activity_redness$rg[activity_redness$activity == "postdisplay"], stat = 'qq')

# Check normality with Shaprio Wilk normality test
by(activity_redness$rg, activity_redness$activity, shapiro.test)

# run t-test
fig4_ttest <- t.test(activity_redness_ttest$postdisplay,
                     activity_redness_ttest$baseline,
                     paired=TRUE)
fig4_ttest

# remove data frames to clean up the workspace
rm(activity_redness, activity_redness_ttest, fig4, fig4_ttest)

#############################################################################
###             Figure 5: Heat manipulation and redness                   ###
#############################################################################

# data frames used:
# heat_manipulation = long data frame for the plot, paired baseline and heat pack treatment redness measurements
# heat_manipulation_ttest =summarized data frame for stats, paired baseline and heat pack treatment redness measurements

# plot figure 5
summary(heat_manipulation$rg) # find range for graphing
# reorder variables
heat_manipulation$temp_treatment <- factor(heat_manipulation$temp_treatment, levels = c("none", "heat"))
# call plot for Adobe Baseline vs. Heat Pack
fig5 <- ggplot(data=heat_manipulation,
                        aes(x=temp_treatment, y=rg, group=set)) +
  geom_line(size=1, col=tRed) +
  geom_point(size=3, col=tBlack) + 
  scale_x_discrete(labels=c("Baseline","Heat Application"),
                   name="") +
  scale_y_continuous(name="Adobe: Red/Green",
                     limits = c(1, 1.7), breaks = seq(1,1.7,0.1)) +
  theme(axis.text=element_text(size=18,
                               family = 'Arial'),
        axis.title=element_text(size=16,
                                family = 'Arial'),
        panel.background = element_blank(),
        axis.line=element_line(color = "black"),
        plot.title=element_text(size=24,
                                family = 'Arial'),
        legend.title=element_text(size=16,
                                  face="bold",
                                  family = 'Arial'),
        legend.text=element_text(size=16,
                                 family = 'Arial')) + 
  ggtitle("")
fig5 

# save figure 5
ggsave("figure5.pdf", plot = fig5, width=8, height=6, units="in", dpi=600, useDingbats=FALSE)

### Run stats for Figure 5
# check normality of redness (rg)
# q-q plot for none
qplot(sample = heat_manipulation$rg[heat_manipulation$temp_treatment == "none"], stat = 'qq')
# q-q plot for heat
qplot(sample = heat_manipulation$rg[heat_manipulation$temp_treatment == "heat"], stat = 'qq')

# Check normality with Shaprio Wilk normality test
by(heat_manipulation$rg, heat_manipulation$temp_treatment, shapiro.test)

# run t-test
fig5_ttest <- t.test(heat_manipulation_ttest$heat,
                     heat_manipulation_ttest$none,
                     paired=TRUE)
fig5_ttest

# remove data frames to clean up the workspace
rm(heat_manipulation, heat_manipulation_ttest,
   fig5, fig5_ttest)

#############################################################################
###                 Figure 6: Redness and skin temperature                ###
#############################################################################

# data frame used:
# redness_skin_temp = average redness (Red/Green) and chest skin temperature (C) for N=14 adult males

# plot figure 6
summary(redness_skin_temp$rg) # find ranges for graphing
summary(redness_skin_temp$chest_temp)
# call plot for chest skin temperature by redness
fig6 <- ggplot(redness_skin_temp,
               aes(x=rg, y=chest_temp, col=rg)) +
  geom_point(size=5) +
  ggtitle("") +
  scale_y_continuous(name="Chest skin temperature (°C)",
                     limits=c(29,35),
                     breaks = seq(29,35,1)) +
  scale_x_continuous(name="Adobe: Red/Green",
                     limits=c(1.3,1.9),
                     breaks= seq(1.3,1.9,0.2)) +
  scale_color_gradient(low=tRed,
                       high=tRed) + 
  geom_smooth(method=lm, se=FALSE, linetype="dashed",
              color=tBlack) +
  theme(axis.text=element_text(size=18,
                               family = 'Arial'),
        axis.title=element_text(size=18,
                                family = 'Arial'),
        panel.background = element_blank(),
        axis.line=element_line(color = "black"),
        plot.title=element_text(size=24,
                                family = 'Arial'),
        legend.position = "none") 
fig6 

# save figure 6
ggsave("figure6.pdf", plot = fig6, width=8, height=6, units="in", dpi=600, useDingbats=FALSE)

### Run stats for Figure 6

# run linear regression model for chest temp by redness
lm_redness_skin_temp <- lm(chest_temp ~ rg, data=redness_skin_temp)
summary(lm_redness_skin_temp)

# remove data frames to clean up the workspace
rm(redness_skin_temp, fig6, lm_redness_skin_temp)

#############################################################################
###   Figure S1: X-Rite ColorChecker chart vs. Nix Mini Color Sensor      ###
#############################################################################

# data frame used:
# xrite_nix_comparison = sRGB values published by X-Rite (chart) and RGB values measured with Nix for the 24 ColorChecker Classic squares

# plot fig S1a
# X-Rite ColorChecker Chart RED v Nix RED 
figs1a <- ggplot(xrite_nix_comparison,
                           aes(x=R_chart, y=R_nix, col=R_chart, fill=square_num)) +
  geom_text(aes(label=square_num),hjust=0,vjust=-1.5,col=tBlack) +
  geom_point(size=5) +
  ggtitle("") +
  scale_x_continuous(name="ColorChecker chart Red",
                     limits=c(0,250),
                     breaks= seq(0,250,50)) +
  scale_y_continuous(name="Nix Red",
                     limits=c(0,250),
                     breaks = seq(0,250,50)) +
  scale_color_gradient(low = tRed,
                       high = tRed) +
  geom_abline(slope=1,
              intercept=0,
              linetype="dashed",
              color=tBlack) +
  theme(axis.text=element_text(size=18,
                               family = 'Arial'),
        axis.title=element_text(size=18,
                                family = 'Arial'),
        panel.background = element_blank(),
        axis.line=element_line(color = "black"),
        plot.title=element_text(size=24,
                                family = 'Arial'),
        legend.position = "right") + 
  guides(col=FALSE) + 
  scale_fill_identity(name = 'ColorChecker square',
                      guide = 'legend',
                      labels = c("1. dark skin",
                                 "2. light skin",
                                 "3. blue sky",
                                 "4. foliage",
                                 "5. blue flower",
                                 "6. bluish green",
                                 "7. orange",
                                 "8.purplish blue",
                                 "9. moderate red",
                                 "10. purple",
                                 "11. yellow green",
                                 "12. orange yellow",
                                 "13. blue",
                                 "14. green",
                                 "15. red",
                                 "16. yellow",
                                 "17. magenta",
                                 "18. cyan",
                                 "19. white",
                                 "20. neutral 8",
                                 "21. neutral 6.5",
                                 "22. neutral 5",
                                 "23. neutral 3.5",
                                 "24. black"))

figs1a

# plot figs1b
#X-Rite ColorChecker Chart GREEN v Nix GREEN
figs1b <- ggplot(xrite_nix_comparison,
                 aes(x=G_chart, y=G_nix, col=G_chart, fill=square_num)) +
  geom_text(aes(label=square_num),hjust=0,vjust=-1.5,col=tBlack) +
  geom_point(size=5) +
  ggtitle("") +
  scale_x_continuous(name="ColorChecker chart Green",
                     limits=c(0,250),
                     breaks= seq(0,250,50)) +
  scale_y_continuous(name="Nix Green",
                     limits=c(0,250),
                     breaks = seq(0,250,50)) +
  scale_color_gradient(low=tGreen,
                       high=tGreen) +
  geom_abline(slope=1,
              intercept=0,
              linetype="dashed",
              color=tBlack) +
  theme(axis.text=element_text(size=18,
                               family = 'Arial'),
        axis.title=element_text(size=18,
                                family = 'Arial'),
        panel.background = element_blank(),
        axis.line=element_line(color = "black"),
        plot.title=element_text(size=24,
                                family = 'Arial'),
        legend.position = "right") +
  guides(col=FALSE) + 
  scale_fill_identity(name = 'ColorChecker square',
                      guide = 'legend',
                      labels = c("1. dark skin",
                                 "2. light skin",
                                 "3. blue sky",
                                 "4. foliage",
                                 "5. blue flower",
                                 "6. bluish green",
                                 "7. orange",
                                 "8.purplish blue",
                                 "9. moderate red",
                                 "10. purple",
                                 "11. yellow green",
                                 "12. orange yellow",
                                 "13. blue",
                                 "14. green",
                                 "15. red",
                                 "16. yellow",
                                 "17. magenta",
                                 "18. cyan",
                                 "19. white",
                                 "20. neutral 8",
                                 "21. neutral 6.5",
                                 "22. neutral 5",
                                 "23. neutral 3.5",
                                 "24. black"))

figs1b

# plot figs1c
# X-Rite ColorChecker Chart BLUE v Nix BLUE
figs1c <- ggplot(xrite_nix_comparison,
                 aes(x=B_chart, y=B_nix, col=B_chart, fill=square_num)) +
  geom_text(aes(label=square_num),hjust=0,vjust=-1.5,col=tBlack) +
  geom_point(size=5) +
  ggtitle("") +
  scale_x_continuous(name="ColorChecker chart Blue",
                     limits=c(0,250),
                     breaks= seq(0,250,50)) +
  scale_y_continuous(name="Nix Blue",
                     limits=c(0,250),
                     breaks = seq(0,250,50)) +
  scale_color_gradient(low=tBlue,
                       high=tBlue) +
  geom_abline(slope=1,
              intercept=0,
              linetype="dashed",
              color=tBlack) +
  theme(axis.text=element_text(size=18,
                               family = 'Arial'),
        axis.title=element_text(size=18,
                                family = 'Arial'),
        panel.background = element_blank(),
        axis.line=element_line(color = "black"),
        plot.title=element_text(size=24,
                                family = 'Arial'),
        legend.position = "right") +
  guides(col=FALSE) + 
  scale_fill_identity(name = 'ColorChecker square',
                      guide = 'legend',
                      labels = c("1. dark skin",
                                 "2. light skin",
                                 "3. blue sky",
                                 "4. foliage",
                                 "5. blue flower",
                                 "6. bluish green",
                                 "7. orange",
                                 "8.purplish blue",
                                 "9. moderate red",
                                 "10. purple",
                                 "11. yellow green",
                                 "12. orange yellow",
                                 "13. blue",
                                 "14. green",
                                 "15. red",
                                 "16. yellow",
                                 "17. magenta",
                                 "18. cyan",
                                 "19. white",
                                 "20. neutral 8",
                                 "21. neutral 6.5",
                                 "22. neutral 5",
                                 "23. neutral 3.5",
                                 "24. black"))
figs1c

# create multi-paneled plot for figs1a-c
# Multi-panel plot for RGB Nix v ColorChecker Chart
figs1 <- ggarrange(figs1a, figs1b, figs1c,
                   nrow =1, ncol =3,
                   legend = "right",
                   common.legend = TRUE,
                   labels = c("A", "B", "C"),
                   font.label = list(size = 24,
                                                color = "black",
                                                face = "bold",
                                                family = 'Arial',
                                                vjust=1.5))
figs1

# save figure S1
ggsave("figureS1.pdf", plot = figs1, width=16, height=6, units="in", dpi=600, useDingbats=FALSE)


### Run stats for Figure S1
#linear regression models for X-Rite ColorChecker chart v Nix
# Red
lm_R <- lm(R_nix ~ R_chart, data=xrite_nix_comparison)
summary(lm_R)
# Green
lm_G <- lm(G_nix ~ G_chart, data=xrite_nix_comparison)
summary(lm_G)
# Blue
lm_B <- lm(B_nix ~ B_chart, data=xrite_nix_comparison)
summary(lm_B)

# remove data frames to clean up the workspace
rm(xrite_nix_comparison,
   figs1a, figs1b, figs1c, figs1,
   lm_R, lm_G, lm_B)

#############################################################################
###     Figure S5: ImageJ Activity and Redness                            ###
#############################################################################

# data frame used: activity_redness_ij
# ImageJ MicaToolbox values for Activity and Redness dataset (complement to figure 4)

# PLOT IMAGEJ RG OPP FOR FIGURE S5 
summary(activity_redness_ij$rg_opp) # find range for graphing
# call plot of paired baseline v post display redness
figs5_ij_rgopp <- ggplot(data=activity_redness_ij,
               aes(x=activity, y=rg_opp, group=set)) +
  geom_line(size=1, col=tRed) +
  geom_point(size=3, col=tBlack) +
  scale_x_discrete(labels=c("Baseline","Post-display"),
                   name="") +
  scale_y_continuous(name="ImageJ: Red-Green Opponency",
                     limits = c(0.1, 0.25), breaks = seq(0.1,0.25,0.02)) +
  theme(axis.text=element_text(size=18,
                               family = "Arial"),
        axis.title=element_text(size=18,
                                family = "Arial"),
        panel.background = element_blank(),
        axis.line=element_line(color = "black"),
        plot.title=element_text(size=24,
                                family = "Arial"),
        legend.title=element_text(size=16,
                                  face="bold",
                                  family = "Arial"),
        legend.text=element_text(size=16,
                                 family = "Arial")) + 
  ggtitle("")
figs5_ij_rgopp

# PLOT IMAGEJ LUM FOR FIGURE S5
summary(activity_redness_ij$lum) # find range for graphing
# call plot of paired baseline v post display redness
figs5_ij_lum <- ggplot(data=activity_redness_ij,
                        aes(x=activity, y=lum, group=set)) +
  geom_line(size=1, col=tRed) +
  geom_point(size=3, col=tBlack) +
  scale_x_discrete(labels=c("Baseline","Post-display"),
                   name="") +
  scale_y_continuous(name="ImageJ: Luminance",
                     limits = c(0.05, 0.3), breaks = seq(0.05,0.3,0.05)) +
  theme(axis.text=element_text(size=18,
                               family = "Arial"),
        axis.title=element_text(size=18,
                                family = "Arial"),
        panel.background = element_blank(),
        axis.line=element_line(color = "black"),
        plot.title=element_text(size=24,
                                family = "Arial"),
        legend.title=element_text(size=16,
                                  face="bold",
                                  family = "Arial"),
        legend.text=element_text(size=16,
                                 family = "Arial")) + 
  ggtitle("")
figs5_ij_lum 

# multipanel plot for fig s5
figS5 <- ggarrange(figs5_ij_rgopp, figs5_ij_lum,
                  nrow=1, ncol=2,
                  labels = c("A", "B"),
                  font.label = list(size = 24,
                                    color = "black",
                                    face = "bold",
                                    family = 'Arial',
                                    vjust=1.5))
figS5 

# save figure S5
ggsave("figureS5.pdf", plot = figS5, width=16, height=6, units="in", dpi=600, useDingbats=FALSE)

# Stats for Figure S5
# t-test by rg-oppponency
activity_redness_ij_rgopp_ttest <- activity_redness_ij %>%
  select(set,id,activity,rg_opp)

activity_redness_ij_rgopp_ttest <- activity_redness_ij_rgopp_ttest %>%
  spread(activity, rg_opp)

figs5_ttest_rgopp <- t.test(activity_redness_ij_rgopp_ttest$postdisplay,
                           activity_redness_ij_rgopp_ttest$baseline,
                           paired = TRUE)
figs5_ttest_rgopp
# t-test by luminance
activity_redness_ij_lum_ttest <- activity_redness_ij %>%
  select(set,id,activity,lum)

activity_redness_ij_lum_ttest <- activity_redness_ij_lum_ttest %>%
  spread(activity, lum)

figs5_ttest_lum <- t.test(activity_redness_ij_lum_ttest$postdisplay, 
                         activity_redness_ij_lum_ttest$baseline, 
                         paired = TRUE)
figs5_ttest_lum

rm(activity_redness_ij, activity_redness_ij_lum_ttest, activity_redness_ij_rgopp_ttest,
   figS5, figs5_ij_lum, figs5_ij_rgopp,
   figs5_ttest_lum, figs5_ttest_rgopp)

#############################################################################
###     Figure S6: ImageJ Heat Manipulation and Redness                   ###
#############################################################################

# data frame used: heat_manipulation_ij
# ImageJ MicaToolbox values for Heat Manipulation and Redness data set (complement to figure 5)

# plot Figure S6A ImageJ Red-Green Opponency 
summary(heat_manipulation_ij$rg_opp) # find range for graphing
# reorder variables
heat_manipulation_ij$temp_treatment <- factor(heat_manipulation_ij$temp_treatment, levels = c("none", "heat"))
# call plot for IJ Baseline vs. Heat Pack
figs6_ij_rg_opp <- ggplot(data=heat_manipulation_ij,
               aes(x=temp_treatment, y=rg_opp, group=set)) +
  geom_line(size=1, col=tRed) +
  geom_point(size=3, col=tBlack) + 
  scale_x_discrete(labels=c("Baseline","Heat Application"),
                   name="") +
  scale_y_continuous(name="ImageJ: Red-Green Opponency",
                     limits = c(0.05, 0.25), breaks = seq(0.05,0.25,0.05)) +
  theme(axis.text=element_text(size=18,
                               family = 'Arial'),
        axis.title=element_text(size=16,
                                family = 'Arial'),
        panel.background = element_blank(),
        axis.line=element_line(color = "black"),
        plot.title=element_text(size=24,
                                family = 'Arial'),
        legend.title=element_text(size=16,
                                  face="bold",
                                  family = 'Arial'),
        legend.text=element_text(size=16,
                                 family = 'Arial')) + 
  ggtitle("")
figs6_ij_rg_op 

# plot figure S6B with ImageJ Luminance
summary(heat_manipulation_ij$lum) # find range for graphing
# call plot for IJ Baseline vs. Heat Pack
figs6_ij_lum <- ggplot(data=heat_manipulation_ij,
                         aes(x=temp_treatment, y=lum, group=set)) +
  geom_line(size=1, col=tRed) +
  geom_point(size=3, col=tBlack) + 
  scale_x_discrete(labels=c("Baseline","Heat Application"),
                   name="") +
  scale_y_continuous(name="ImageJ: Luminance",
                     limits = c(0.06, 0.36), breaks = seq(0.06,0.36,0.06)) +
  theme(axis.text=element_text(size=18,
                               family = 'Arial'),
        axis.title=element_text(size=16,
                                family = 'Arial'),
        panel.background = element_blank(),
        axis.line=element_line(color = "black"),
        plot.title=element_text(size=24,
                                family = 'Arial'),
        legend.title=element_text(size=16,
                                  face="bold",
                                  family = 'Arial'),
        legend.text=element_text(size=16,
                                 family = 'Arial')) + 
  ggtitle("")
figs6_ij_lum

# multipaneled plot for the supplementary material
figS6 <- ggarrange(figs6_ij_rg_opp, figs6_ij_lum,
                   nrow=1, ncol=2,
                   labels = c("A", "B"),
                   font.label = list(size = 24,
                                     color = "black",
                                     face = "bold",
                                     family = 'Arial',
                                     vjust=1.5))
figS6 

# save plot as pdf with dpi 600
ggsave("figureS6.pdf", plot = figS6, width=16, height=6, units="in", dpi=600, useDingbats=FALSE)

# Stats for Figure S6
# t-test by rg-oppponency
heat_manipulation_ij_rgopp_ttest <- heat_manipulation_ij %>%
  select(set,dart_id,temp_treatment,rg_opp)

heat_manipulation_ij_rgopp_ttest <- heat_manipulation_ij_rgopp_ttest %>%
  spread(temp_treatment, rg_opp)

figs6_ttest_rgopp <- t.test(heat_manipulation_ij_rgopp_ttest$heat,
                            heat_manipulation_ij_rgopp_ttest$none,
                            paired = TRUE)
figs6_ttest_rgopp
# t-test by luminance
heat_manipulation_ij_lum_ttest <- heat_manipulation_ij %>%
  select(set,dart_id,temp_treatment,lum)

heat_manipulation_ij_lum_ttest <- heat_manipulation_ij_lum_ttest %>%
  spread(temp_treatment, lum)

figs6_ttest_lum <- t.test(heat_manipulation_ij_lum_ttest$heat, 
                          heat_manipulation_ij_lum_ttest$none, 
                          paired = TRUE)
figs6_ttest_lum

rm(figS6, figs6_ij_rg_opp, figs6_ij_lum, 
   figs6_ttest_rgopp, figs6_ttest_lum,
   heat_manipulation_ij, heat_manipulation_ij_lum_ttest, heat_manipulation_ij_rgopp_ttest)

#############################################################################
###     Figure S7: ImageJ Redness and Skin Temperature                    ###
#############################################################################

# data frame used: skin_temp_ij
# ImageJ MicaToolbox values for Redness and Skin Temperature data set (complement to figure 6)

# plot figure S7 with IJ R-G Opp
summary(skin_temp_ij$rg_opp) # find ranges for graphing
summary(skin_temp_ij$chest_temp)
# call plot for chest skin temperature by redness
figs7_ij_rg_opp <- ggplot(skin_temp_ij,
               aes(x=rg_opp, y=chest_temp, col=rg_opp)) +
  geom_point(size=5) +
  ggtitle("") +
  scale_y_continuous(name="Chest skin temperature (°C)",
                     limits=c(29,35),
                     breaks = seq(29,35,1)) +
  scale_x_continuous(name="ImageJ: Red-Green Opponency",
                     limits=c(0.09,0.21),
                     breaks= seq(0.09,0.21,0.03)) +
  scale_color_gradient(low=tRed,
                       high=tRed) + 
  geom_smooth(method=lm, se=FALSE, linetype="dashed",
              color=tBlack) +
  theme(axis.text=element_text(size=18,
                               family = 'Arial'),
        axis.title=element_text(size=18,
                                family = 'Arial'),
        panel.background = element_blank(),
        axis.line=element_line(color = "black"),
        plot.title=element_text(size=24,
                                family = 'Arial'),
        legend.position = "none") 
figs7_ij_rg_opp

# plot figure S7 with IJ luminance
summary(skin_temp_ij$lum) # find ranges for graphing
# call plot for chest skin temperature by luminance
figs7_ij_lum <- ggplot(skin_temp_ij,
                         aes(x=lum, y=chest_temp, col=lum)) +
  geom_point(size=5) +
  ggtitle("") +
  scale_y_continuous(name="Chest skin temperature (°C)",
                     limits=c(29,35),
                     breaks = seq(29,35,1)) +
  scale_x_continuous(name="ImageJ: Luminance",
                     limits=c(0.09,0.19),
                     breaks= seq(0.09,0.18,0.03)) +
  scale_color_gradient(low=tRed,
                       high=tRed) + 
  geom_smooth(method=lm, se=FALSE, linetype="dashed",
              color=tBlack) +
  theme(axis.text=element_text(size=18,
                               family = 'Arial'),
        axis.title=element_text(size=18,
                                family = 'Arial'),
        panel.background = element_blank(),
        axis.line=element_line(color = "black"),
        plot.title=element_text(size=24,
                                family = 'Arial'),
        legend.position = "none") 
figs7_ij_lum

# multipaneled plot for the supplementary material
figS7 <- ggarrange(figs7_ij_rg_opp, figs7_ij_lum,
                   nrow=1, ncol=2,
                   labels = c("A", "B"),
                   font.label = list(size = 24,
                                     color = "black",
                                     face = "bold",
                                     family = 'Arial',
                                     vjust=1.5))
figS7

# save figure S7
ggsave("figureS7.pdf", plot = figS7, width=12, height=6, units="in", dpi=600, useDingbats=FALSE)

# Run stats for figure S7
# run linear regression for chest temp and Red-Green Opponency
lm_redness_skin_temp_ij <- lm(chest_temp ~ rg_opp, data=skin_temp_ij)
summary(lm_redness_skin_temp_ij)

# run linear regression for chest luminance
lm_lum_skin_temp_ij <- lm(chest_temp ~ lum, data=skin_temp_ij)
summary(lm_lum_skin_temp_ij)

rm(figS7, figs7_ij_lum, figs7_ij_rg_opp,
   lm_lum_skin_temp_ij, lm_redness_skin_temp_ij,
   skin_temp_ij)

#############################################################################
###                         END                                           ###
#############################################################################

