
library(tidyverse)
library(kableExtra)
library(ggstance)

#Load Aim 3 results (univar and depth)

univar_boot <- "univariate results/bootstrap_results_univariate_nboot_10000_pct_inrange_95.csv"
univar_data <- "univariate results/univariate_results_pct_inrange_95.csv"

#Update these as our more computationally expenisve fits finish
depth_boot <- "depth results/bootstrap_results_depth_ndir_1000_pct_thin_25_nboot_10000_nGP_5.csv"
depth_data <- "depth results/depth_results_ndir_1000_pct_thin_25_pct_inrange_95_nGP_5.csv"


univar_boot_df <- read_csv(univar_boot)
univar_data_df <- read_csv(univar_data)

depth_boot_df <- read_csv(depth_boot)
depth_data_df <- read_csv(depth_data)


depth_data_df %>% filter(analysis == "a_one_flat") %>% pull(depth_pct) %>% mean()







set.seed(1989)

# --- Make tables ---
univar_table <- univar_boot_df %>%
  pivot_longer(enhanced_speed:stryd_leg_spring_stiffness, 
               names_to = "metric", values_to = "value") %>%
  mutate(value = value*100) %>% #Percent
  pivot_wider(id_cols = c(analysis, metric),
              names_from = row_value,
              values_from = value)

depth_table <- depth_boot_df %>%
  mutate(depth = depth*100) %>% #Percent
  pivot_wider(id_cols = analysis,
              names_from = row_value,
              values_from = depth)

#Save tables
univar_table %>%
  kable(format="html", digits=1) %>% 
  save_kable("figures/aim3_univariate_bootstrap_results.html")

depth_table %>%
  kable(format="html", digits=1) %>% 
  save_kable("figures/aim3_depth_bootstrap_results.html")


#Setup data for plotting

metric_levels <- c("enhanced_speed","hrm_step_length","hrm_vertical_oscillation",
                   "stryd_stance_time","stryd_leg_spring_stiffness")

metric_names <- c("Speed","Step length","Vertical oscillation",
                   "Stance time","Leg stiffness")

#Purr stuff I don't really understand
name_map <- set_names(metric_levels, metric_names)

analysis_type_levels <- c("a_one", "a_two", "a_three", "a_four")


analysis_type_names <- c("Analysis 1: In-lab data vs. real-world data (same subject)", 
                         "Analysis 2: In-lab data (all subjects) vs. real-world data (new subject, same population)", 
                         "Analysis 3: In-lab data (all subjects) vs. real-world data (new subject, new population)",
                         "Analysis 4: Real-world data (Cohort 1) vs. real-world data (Cohort 2)") 




#Fill in later
analysis_map <- set_names(analysis_type_levels, analysis_type_names)

#Means and CIs
univar_plot_df <- univar_table %>%
  mutate(rw_condition = ifelse(str_ends(analysis, "_flat"), "Flat", "All"),
         analysis_type = str_replace(analysis, "_flat", "")) %>%
  mutate(metric = factor(metric, levels = metric_levels)) %>%
  mutate(metric = fct_recode(metric, !!!name_map)) %>% #!!! --> ???
  mutate(analysis_type = factor(analysis_type, levels=analysis_type_levels)) %>%
  mutate(analysis_type = fct_recode(analysis_type, !!!analysis_map)) %>%
  mutate(metric = factor(metric, levels = metric_names))

#Raw data for dots
raw_plot_data <- univar_data_df %>%
  pivot_longer(all_of(metric_levels), 
               names_to = "metric", values_to = "value") %>%
  mutate(value = value*100) %>%
  mutate(rw_condition = ifelse(str_ends(analysis, "_flat"), "Flat", "All"),
         analysis_type = str_replace(analysis, "_flat", "")) %>%
  mutate(metric = factor(metric, levels = metric_levels)) %>%
  mutate(metric = fct_recode(metric, !!!name_map)) %>%
  mutate(analysis_type = factor(analysis_type, levels=analysis_type_levels)) %>%
  mutate(analysis_type = fct_recode(analysis_type, !!!analysis_map)) %>% 
  mutate(metric = factor(metric, levels = metric_names))


#Plot setup
sz <- 1
x_breaks <- seq(0,100,by=25)
dodge_ht <- 0.25
jit_ht <- 0.25 

#Now similar with dots and crossbars
sz_pt <- 1.5
alf_pt <- 0.4

sz_ci <- 0.25
alf_ci <- 0.25
ci_ht <- 0.4

#Jitter height, shoudl be less than vertical dodge
dodge_ht <- 0.5
jit_ht <- 0.05 


title_fnt <- 12

dash_lwd <- 0.25

x_fnt <- 8 #labels of gait metrics

tick_lwd <- 0.25
facet_fnt <- 7.75

leg_fnt <- 9
leg_txt_fnt <- 8




x_va <- 12
y_va <- 0.55

lab_df <- data.frame(x=rep(x_va, times=4),
                     y= rep(y_va, times=4),
                     labels = c("A","B","C","D"),
                     analysis_type = analysis_type_names)


abcd_fnt <- 4 #SET ABCD FONT HERE
vline_lwd <- 0.8


# -- Univar plot 
univar_fig <- raw_plot_data %>%
  ggplot(aes(x=value, y=metric, color=rw_condition)) + 
  geom_vline(linetype = "dashed", xintercept = 95, linewidth=dash_lwd) + 
  geom_crossbar(data=univar_plot_df, mapping=aes(x=mean, y=factor(metric, levels=metric_names), 
                                                 fill=rw_condition,
                                                 xmin=p025, xmax=p975),
                position = ggstance::position_dodgev(height=dodge_ht),
                inherit.aes=FALSE,
                size=sz_ci, alpha = alf_ci, width=ci_ht
                ) + 
  geom_point(position=ggstance::position_jitterdodgev(jitter.height = jit_ht, 
                                                      jitter.width = 0,
                                                      dodge.height = dodge_ht), 
             size=sz_pt, alpha=alf_pt, pch=16) +
  scale_x_continuous(limits = c(0,100), breaks = x_breaks, expand=c(0,0),
                     name="Distribution overlap (%)") + 
  scale_y_discrete(position = "left") + 
  scale_color_brewer(palette="Set1", name="Running condition") + 
  scale_fill_brewer(palette="Set1", name="Running condition") + 
  #FINAL SENSORS PROOF HACK
  geom_text(aes(x=x,y=y,label=labels), inherit.aes = FALSE,
            data = lab_df, size = abcd_fnt, fontface="bold") + 
  #geom_hline(yintercept=5.53, linewidth=vline_lwd, color = "black") + 
  facet_wrap(~analysis_type, ncol=1) + 
  ggtitle("Univariate analysis") + 
  guides(fill = guide_legend(title.position="top", title.hjust = 0.5),
         color = guide_legend(title.position="top", title.hjust = 0.5)) + 
  coord_flip() + 
  theme_bw() + 
  theme(legend.position = "bottom",
        panel.grid = element_blank(),
        strip.text.x = element_text(color="black", size=facet_fnt, hjust=0.5),
        axis.title.x = element_blank(),
        axis.ticks = element_line(color="black", linewidth=tick_lwd),
        axis.text.y = element_text(color="black"),
        axis.text.x = element_text(color="black", size=x_fnt),
        legend.title = element_text(color="black", size=leg_fnt),
        legend.text = element_text(color='black', size=leg_txt_fnt),
        plot.title = element_text(hjust=0.5, size=title_fnt)
        )



univar_fig


# ---- Depth plot


#Redefine?
analysis_type_names <- c(" ", "  ", "   ", "    ") #Fill in later
analysis_map <- set_names(analysis_type_levels, analysis_type_names)


#Means and CIs
depth_plot_df <- depth_table %>%
  mutate(rw_condition = ifelse(str_ends(analysis, "_flat"), "Flat", "All"),
         analysis_type = str_replace(analysis, "_flat", "")) %>%
  mutate(analysis_type = factor(analysis_type, levels=analysis_type_levels)) %>%
  mutate(analysis_type = fct_recode(analysis_type, !!!analysis_map)) %>%
  mutate(metric = "Multivariate depth")

#Raw data for dots
depth_raw_plot_data <- depth_data_df %>%
  mutate(value = depth_pct*100) %>%
  mutate(rw_condition = ifelse(str_ends(analysis, "_flat"), "Flat", "All"),
         analysis_type = str_replace(analysis, "_flat", "")) %>%
  mutate(analysis_type = factor(analysis_type, levels=analysis_type_levels)) %>%
  mutate(analysis_type = fct_recode(analysis_type, !!!analysis_map)) %>% 
  mutate(metric = "Multivariate depth")


# -- Depth plot



# -- DEPTH plot 
depth_fig <- depth_raw_plot_data %>%
  ggplot(aes(x=value, y=metric, color=rw_condition)) + 
  geom_vline(linetype = "dashed", xintercept = 95, linewidth=dash_lwd) + 
  geom_crossbar(data = depth_plot_df, mapping=aes(x=mean, y=metric, 
                                                 fill=rw_condition,
                                                 xmin=p025, xmax=p975),
                position = ggstance::position_dodgev(height=dodge_ht),
                inherit.aes=FALSE,
                size=sz_ci, alpha = alf_ci, width=ci_ht
  ) + 
  geom_point(position=ggstance::position_jitterdodgev(jitter.height = jit_ht, 
                                                      jitter.width = 0,
                                                      dodge.height = dodge_ht), 
             size=sz_pt, alpha=alf_pt, pch=16) +
  scale_x_continuous(limits = c(0,100), breaks = x_breaks, expand=c(0,0),
                     name="Distribution overlap (%)", position="top") + 
  scale_y_discrete(position = "left") + 
  scale_color_brewer(palette="Set1", name="Running condition") + 
  scale_fill_brewer(palette="Set1", name="Running condition") + 
  facet_wrap(~analysis_type, ncol=1) + 
  guides(fill = guide_legend(title.position="top", title.hjust = 0.5),
         color = guide_legend(title.position="top", title.hjust = 0.5)) + 
  ggtitle("Depth analysis") + 
  coord_flip() + 
  theme_bw() + 
  theme(legend.position = "bottom",
        panel.grid = element_blank(),
        strip.text.x = element_text(color="black", size=facet_fnt),
        axis.text.y = element_text(color="black"),
        axis.text.x = element_text(color="black", size=x_fnt),
        axis.title.x = element_blank(),
        axis.ticks = element_line(color="black", linewidth=tick_lwd),
        axis.title.y = element_blank(),
        legend.title = element_text(color="black", size=leg_fnt),
        legend.text = element_text(color='black', size=leg_txt_fnt),
        plot.title = element_text(hjust=0.5, size=title_fnt)
  ) #etc...


depth_fig


#library(cowplot)
#plot_grid(univar_fig, depth_fig, n_cols = 1)

library(patchwork)
patch_plot <- univar_fig + depth_fig + plot_layout(widths = c(4, 1))

patch_plot

#ggsave...



ggsave("figures/main_analysis_figure.png", 
       plot=patch_plot, dpi=600, 
       width=6.5, height=6.5, units="in")



