


library(tidyverse)
library(viridis)
library(cowplot)
library(knitr)
library(MASS)
library(ddalpha)
library(ggridges)
library(bigmds)

select <- dplyr::select #MASS masking

# --- Plot setupt ----
ax_fnt <- 10
title_fnt <- 10
sz <- 1

#Setup params
v_jit <- 0.125
title_fnt <- 10
fnt <- 3.25
sz <- 0.5
alf_lab <- 0.3
alf_rw <- 0.09
facet_fnt <- 8

set.seed(42)

kleg_jit <- 0.01

plot_subject <- "S042" #Subject to demo



turn_limit <- 6.342401 #From initial analysis
hill_limit <- 2.276141

# --- Setup params ----


#Variables that define gait pattern
gp_cols <- c("enhanced_speed", 
             "hrm_step_length", #speed + step length fully specifies cadence 
             "hrm_vertical_oscillation", 
             "stryd_stance_time",
             "stryd_leg_spring_stiffness") 
#Consider sens analysis where we dont use leg stiffness

label_names <- c("Speed (m/s)",
                 "Step Length (mm)",
                 "Vertical Oscillation (mm)",
                 "Stance Time (ms)",
                 "Leg Stiffness (kN/m)")

select_cols <- gp_cols


# (For univariate analysis)
#Percent overlapping within this range in reference distribution
univar_ref_pct <- 0.95

set.seed(42)


cohort1_inlab <- read_csv("data/cohort1_inlab.csv") %>%
  filter(subject == plot_subject)

cohort1_rw <- read_csv("data/cohort1_realworld.csv") %>%
  filter(subject == plot_subject)
#Don't need c2 course for this figure
cohort2_rw <- read_csv("data/cohort2_realworld.csv")


#Gaitpattern DFs for analysis 
cohort1_inlab_gp <- cohort1_inlab %>%
  select(subject, all_of(gp_cols)) 

cohort1_rw_gp <- cohort1_rw %>%
  select(subject, all_of(gp_cols)) %>%
  filter(stryd_stance_time <= 400,
         stryd_leg_spring_stiffness >= 6)


cohort2_rw_gp <- cohort2_rw %>%
  select(subject, all_of(gp_cols))


# Maybe rename to follor rmd convetino? 
inlab_gp <- cohort1_inlab_gp %>%
  select(all_of(gp_cols))
rw_gp <- cohort1_rw_gp %>%
  select(all_of(gp_cols))


#Here is how to "zip" vec of names to vec of actual variables

named_cols <- label_names
names(named_cols) <- select_cols




#Pivot to long
inlab_long <- inlab_gp %>%
  pivot_longer(everything(), names_to = "metric", values_to = "value") %>%
  mutate(metric = factor(metric, levels = select_cols))

realworld_long <- rw_gp %>%
  pivot_longer(everything(), names_to = "metric", values_to = "value") %>%
  mutate(metric = factor(metric, levels=select_cols))


#Plotting?
all_sub_data <- inlab_gp %>%
  mutate(condition = "inlab") %>%
  bind_rows(rw_gp %>% mutate(condition="realworld"))

all_sub_long <- all_sub_data %>%
  select(-condition) %>%
  pivot_longer(everything(), names_to = "metric", values_to = "value") %>%
  mutate(metric = factor(metric, levels=select_cols))


#Create flat/not turning rw indicators

filt_rw_df <- cohort1_rw %>%
  mutate(is_straight = ifelse(abs(turn_rate) <= turn_limit, 1, 0),
         is_flat = ifelse(abs(incline_pct_grade) <= hill_limit, 1,0)) %>%
  mutate(is_flat_and_straight = (is_straight == 1 & is_flat == 1))


percent_flat_and_straight = sum(filt_rw_df$is_flat_and_straight) / dim(filt_rw_df)[1]*100

rw_gp_flat_straight <- filt_rw_df %>%
  filter(is_flat_and_straight == 1) %>%
  select(all_of(select_cols)) 


#Get all realworld data but add indicator of flat or not

dp_temp_rw <- filt_rw_df %>%
  select(all_of(c("is_flat_and_straight", select_cols))) %>%
  mutate(condition = "realworld")

dp_temp_inlab <- cohort1_inlab %>%
  select(all_of(select_cols)) %>%
  mutate(is_flat_and_straight = TRUE,
         condition = "inlab") 

mds_df <- dp_temp_rw %>% bind_rows(dp_temp_inlab)


rw_fs_long <- rw_gp_flat_straight %>%
  pivot_longer(everything(), names_to = "metric", values_to = "value") %>%
  mutate(metric = factor(metric, levels=select_cols))


all_sub_data_flat_straight <- inlab_gp %>%
  mutate(condition = "inlab") %>%
  bind_rows(rw_gp_flat_straight %>% mutate(condition="realworld"))

all_sub_data_flat_straight_long <- all_sub_data_flat_straight %>%
  select(-condition) %>%
  pivot_longer(everything(), names_to = "metric", values_to = "value") %>%
  mutate(metric = factor(metric, levels=select_cols))






# -------------------------------------------------------
#
#                  Univariate analysis  
#
# -------------------------------------------------------

quant_df <- inlab_gp %>%
  pivot_longer(everything(), names_to = "metric", values_to = "value") %>%
  mutate(metric = factor(metric, levels = select_cols)) %>%
  group_by(metric) %>%
  summarize(quant_lo = quantile(value, 0.025),
            quant_hi = quantile(value, 0.975)) %>%
  mutate(metric = factor(metric, levels = select_cols))

#For labeling only
all_quant <- all_sub_long %>%
  group_by(metric) %>%
  summarize(metric_99 = quantile(value, 0.999),
            metric_max = max(value),
            metric_min = min(value))
#Ditto
flat_straight_quant <- all_sub_data_flat_straight_long %>%
  group_by(metric) %>%
  summarize(metric_99 = quantile(value, 0.999),
            metric_max = max(value),
            metric_min = min(value))


# Compute data in quantile ranges
for (i in 1:length(select_cols)){
  #For each metric, compute how much in realworld data lies within range of quant df
  this_metric <- select_cols[i]
  rw_values <- rw_gp %>%
    pull(this_metric)
  
  rw_flat_straight_values <- rw_gp_flat_straight %>%
    pull(this_metric)
  
  range_lo = quant_df[quant_df$metric == this_metric,]$quant_lo
  range_hi = quant_df[quant_df$metric == this_metric,]$quant_hi
  rw_in_range = sum(rw_values >= range_lo & rw_values <= range_hi)/length(rw_values)
  quant_df[quant_df$metric == this_metric,"percent_in_range"] <- rw_in_range
  
  #Repeat but for flat straightr restricted
  rw_flat_straight_in_range = sum(rw_flat_straight_values >= range_lo & rw_flat_straight_values <= range_hi)/length(rw_flat_straight_values)
  
  quant_df[quant_df$metric == this_metric,"percent_in_range_flat_straight"] <- rw_flat_straight_in_range
  
  #For plots
  all_quant[all_quant$metric == this_metric,"string_percent_in_range"] <- sprintf("%.1f%%",rw_in_range*100)
  all_quant[all_quant$metric == this_metric,"string_percent_in_range_flat_straight"] <- sprintf("%.1f%%",rw_flat_straight_in_range*100)
}


# ------------   Plotting results

#Add jitter
inlab_plot <- inlab_long %>%
  mutate(value = ifelse(metric == "stryd_leg_spring_stiffness", value + runif(n(), 
                                                                              min=-0.01, max=0.01),value))
realworld_plot <- realworld_long %>%
  mutate(value = ifelse(metric == "stryd_leg_spring_stiffness", value + runif(n(), 
                                                                              min=-0.01, max=0.01),value))

uni_plot <- inlab_plot %>%
  ggplot(aes(x=value, y=1)) + 
  geom_rect(data = quant_df, aes(xmin=quant_lo, xmax=quant_hi,
                                 ymin=-Inf, ymax=Inf),
            inherit.aes=FALSE,
            fill = "lightslateblue", alpha = 0.3) +
  #Inlab data
  geom_point(position = position_jitter(height=v_jit, width=0),
             color = "dodgerblue", alpha = alf_lab,
             size=sz, pch=16) + 
  #Realworld data
  geom_point(aes(x=value, y=1.5),
             inherit.aes=FALSE, data = realworld_plot,
             pch=16,
             color="forestgreen", alpha = alf_rw,
             position = position_jitter(height=v_jit, width=0),
             size=sz) +
  #Text to label percent - s/o to stackoverflow for slick Inf trick
  geom_text(aes(x=Inf, y=-Inf, label=string_percent_in_range), 
            data = all_quant, inherit.aes=FALSE, color="black",
            size=fnt, 
            hjust = 1, 
            vjust = -1) + 
  #Wrapping, formatting...
  facet_wrap(~metric, scales="free_x", ncol=1,
             labeller = as_labeller(named_cols)) + 
  scale_y_continuous(limits=c(0.75,1.75)) +
  ggtitle("Univariate analysis\nIn-lab vs. real-world data (same runner)") + 
  theme_bw() + 
  theme(legend.position = "none",
        plot.title = element_text(hjust=0.5, size=title_fnt),
        panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(color='black'),
        strip.text.x = element_text(size = facet_fnt))
#To effectively jitter across different scales you'd need a loop and cowplot
#uni_plot

uni_plot



ggsave("figures/univariate_demo_plot.png", 
       plot=uni_plot, dpi=600, 
       width=6.5, height=6.5, units="in")

