# Make depth plot and MDS



library(tidyverse)
library(viridis)
library(cowplot)
library(knitr)
library(MASS)
library(ddalpha)
library(bigmds)
select <- dplyr::select #grr...MASS overwrites select

source("overlap_analysis_functions.R") #Who needs CRAN anyways?

#Variables that define gait pattern
gp_cols <- c("enhanced_speed", 
             "hrm_step_length", #speed + step length fully specifies cadence 
             "hrm_vertical_oscillation", 
             "stryd_stance_time",
             "stryd_leg_spring_stiffness") 
#Consider sens analysis where we dont use leg stiffness

label_names <- c("Speed",
                 "HRM: Step Length",
                 "HRM: Vertical Oscillation",
                 "Stryd: Stance Time",
                 "Stryd: Leg Stiffness")

#Plot setup
set.seed(42) #BigMDS involves some stochasticity
demo_subject <- "S057" #Ref vs results to get one with middling overlap
#S059 is a good one. also try 49, 57


#For plotting only! Not "real" analysis 
n_depth_dir <- 1000
n_mds <- 500 #max size of mds chunks?
#500/500 is waht plot params were optimized for, may need to tweak after doing 

# (For univariate analysis)
#Percent overlapping within this range in reference distribution
depth_ref_pct <- 0.95


#---------------------------
#   Load data
#---------------------------
cohort1_inlab <- read_csv("data/cohort1_inlab.csv")
cohort1_rw <- read_csv("data/cohort1_realworld.csv")
cohort2_course <- read_csv("data/cohort2_course.csv")
cohort2_rw <- read_csv("data/cohort2_realworld.csv")

cohort1_inlab_gp <- cohort1_inlab %>% select(subject, all_of(gp_cols))
cohort1_rw_gp <- cohort1_rw %>% select(subject, all_of(gp_cols))
cohort2_rw_gp <- cohort2_rw %>% select(subject, all_of(gp_cols))

mds_df <- bind_rows(cohort1_inlab_gp %>% mutate(cohort = "cohort1_inlab"), 
                    cohort1_rw_gp %>% mutate(cohort = "cohort1_rw")) %>%
  slice_sample(prop=1)


#---------------------------------
#         Do MDS (on all)
#---------------------------------

#Lets fit mds on everything so we can project to same space
all_data_Xc <- mds_df %>%
  select(all_of(gp_cols)) %>%
  as.matrix %>% scale(center = TRUE, scale=TRUE)

#lol what do these settings even do...
mds_fit <- fast_mds(x = all_data_Xc, l = n_mds, s_points = 2*2, r = 2, 
                    n_cores = 1, dist_fn = stats::dist)

#Slowwww - not sure how long will take
#true_fit <- cmdscale(dist(all_data_Xc), k=2) #do not do...
mds_df$mds_x = mds_fit$points[,1]
mds_df$mds_y = mds_fit$points[,2]

#Quick plot
mds_df %>%
  filter(subject == demo_subject ) %>%
  ggplot(aes(x=mds_x, y=mds_y, color=cohort)) + 
  geom_point(size=1, alpha = 0.5) + 
  facet_wrap(~cohort)


#---------------------------------
#   Split to IN/RW and do depth
#---------------------------------


#Split into reference data (inlab) and new data (realworld)
df_ref <- mds_df %>%
  filter(subject == demo_subject) %>%
  filter(cohort == "cohort1_inlab")

X_ref <- df_ref %>%
  select(all_of(gp_cols))

df_new <- mds_df %>%
  filter(subject == demo_subject) %>%
  filter(cohort == "cohort1_rw") 

X_new <- df_new %>%
  select(all_of(gp_cols))

df_ref$ref_self_depth <- depth.halfspace(x = X_ref, data = X_ref, 
                                         exact=FALSE, num.directions = n_depth_dir,
                                         seed=42) #1000 is default

depth_threshold <- quantile(df_ref$ref_self_depth, 1 - depth_ref_pct)

df_ref$above_depth_threshold <- (df_ref$ref_self_depth >= depth_threshold)

df_new$depth_vs_ref <- depth.halfspace(x = X_new, data = X_ref,
                                       exact=FALSE, num.directions = n_depth_dir,
                                       seed=42) #1000 is default
#Prop above threshold
rw_above_depth_threshold <- mean(df_new$depth_vs_ref >= depth_threshold)


#--------------------------------------------
#
#    Plot!!!!
#
#--------------------------------------------

# --- Setup plot ---


#Axis limits (X and y, should be equal and zero-centered)
lim_range <- c(-4,4)
lim_breaks <- seq(lim_range[1], lim_range[2], by=2)

#Inlab params (top row)
in_col <- "#E41A1C"
in_sz <- 1
in_alf <- 0.25

#RW params (top row)
rw_col <- "#377EB8"
rw_sz <- 1
rw_alf <- 0.25

# Inlab params (depth)
in_depth_sz <- 1.25
in_alf_min <- 0.35
in_alf_max <- 0.75

# RW params (depth)
rw_depth_sz <- 1
rw_alf_min <- 0.1
rw_alf_max <- 0.75

#Title
title_fnt <- 10


#Parameters for zero-depth points
zd_alf <- 0.2
zd_sz <- 1
zd_col <- "#4C4E52"
#text annotation
ann_fnt <- 3
ann_x <- -4
ann_y <- -3.9


#Split RW to depth = 0 and not
rw_zerodepth <- df_new %>%
  filter(depth_vs_ref == 0)
rw_nonzerodepth <- df_new %>%
  filter(depth_vs_ref != 0)

#If other colors needed
library(RColorBrewer)
col_vec <- brewer.pal(n=5,"Set1")
col_vec


#Margin fo bottom panel (elftr/rgiht)
br_margin <- 0.25

#Depth colorbar
d_max <- 0.4 #max depth on colorbar
colorbar_width <- 0.45
colorbar_height <- 0.15


#Unified themes
panel_theme <- theme(plot.title = element_text(hjust=0.5, size=title_fnt),
                     axis.title = element_blank(),
                     axis.text = element_blank(),
                     axis.ticks = element_blank())



# ------------------------------------------------------------------------


# ------ Inlab only, colored

P1 <- df_ref %>%
  ggplot(aes(x=mds_x, y=mds_y)) +
  #Inlab
  geom_point(pch=16, size=in_sz, color=in_col, 
             alpha = in_alf) +
  #Prettify
  scale_x_continuous(limits=lim_range, breaks = lim_breaks) +
  scale_y_continuous(limits=lim_range, breaks = lim_breaks) + 
  ggtitle("In-lab") + 
  theme_bw() + 
  panel_theme

P1


# ------ RW only, colored

P2 <- df_ref %>%
  ggplot(aes(x=mds_x, y=mds_y)) +
  #RW first on bottom
  geom_point(data = df_new, 
             pch=16, size=rw_sz, color=rw_col,
             alpha = rw_alf) + 
  #Prettify
  scale_x_continuous(limits=lim_range, breaks = lim_breaks) +
  scale_y_continuous(limits=lim_range, breaks = lim_breaks) + 
  ggtitle("Real-world") + 
  theme_bw() + 
  panel_theme

P2

# ------ RW + inlab, colored

P3 <- df_ref %>%
  ggplot(aes(x=mds_x, y=mds_y)) +
  #RW first on bottom
  geom_point(data = df_new, 
             pch=16, size=rw_sz, color=rw_col,
             alpha = rw_alf) + 
  #Inlab
  geom_point(pch=16, size=in_sz, color=in_col, 
             alpha = in_alf) +
  #Prettify
  scale_x_continuous(limits=lim_range) +
  scale_y_continuous(limits=lim_range) + 
  ggtitle("In-lab + real-world") + 
  theme_bw() + 
  panel_theme


P3

# -----------------------------------------------
#   Bottom panel


#Inlab, self depth
P4 <- df_ref %>%
  ggplot(aes(x=mds_x, y=mds_y, color=ref_self_depth, 
             alpha = ref_self_depth)) +
  geom_point(pch=16, size=in_depth_sz) + 
  scale_color_viridis(option = "plasma", name="Depth", limits=c(0,d_max)) +
  scale_alpha(range=c(in_alf_min,in_alf_max), guide="none") + 
  scale_x_continuous(limits=lim_range, breaks = lim_breaks) +
  scale_y_continuous(limits=lim_range, breaks = lim_breaks) + 
  guides(colour = guide_colorbar(title.position="top", title.hjust = 0.5)) + 
  ggtitle("In-lab depth") + 
  theme_bw() + 
  panel_theme + 
  theme(legend.position = "bottom",
        plot.margin = unit(c(0,br_margin,0,br_margin), "in"),
        legend.key.width = unit(colorbar_width, "in"),
        legend.key.height = unit(colorbar_height, "in"))

P4

# --- RW depth relative to inlab


P5 <- rw_nonzerodepth %>%
  ggplot(aes(x=mds_x, y=mds_y, color=depth_vs_ref, 
             alpha = depth_vs_ref)) +
  #Layer on zero-depth points first
  geom_point(data=rw_zerodepth, aes(x=mds_x, y=mds_y), inherit.aes=FALSE,
             pch=16, alpha=zd_alf, size=zd_sz, color=zd_col) + 
  #Now the nonzero depth RW points
  geom_point(pch=16, size=rw_depth_sz) + 
  #Annotate
  annotate(geom="text", x=ann_x, y=ann_y, label = "Gray: zero-depth data", 
           color = zd_col, size=ann_fnt, hjust=0, vjust=0) + 
  #Scales
  scale_color_viridis(option = "plasma", name="Depth", limits=c(0,d_max)) +
  scale_alpha(range=c(rw_alf_min,rw_alf_max), guide="none") +
  scale_x_continuous(limits=lim_range, breaks = lim_breaks) +
  scale_y_continuous(limits=lim_range, breaks = lim_breaks) + 
  guides(colour = guide_colorbar(title.position="top", title.hjust = 0.5)) + 
  ggtitle("Real world depth vs. in-lab data") + 
  theme_bw() + 
  panel_theme + 
  theme(legend.position = "bottom",
        plot.margin = unit(c(0,br_margin,0,br_margin), "in"),
        legend.key.width = unit(colorbar_width, "in"),
        legend.key.height = unit(colorbar_height, "in"))

#Plot margin is top right bottom left

P5

#use hack way to add title above to say we're in MDS space
#Maybe drop in rw_above_depth_threshold as tex tannote? 

suptitle_fnt <- 10

title <- ggdraw() + 
  draw_label(
    "2D Multidimensional scaling projection of gait pattern data from one subject",
    fontface = 'bold',
    x = 0.5,
    size=suptitle_fnt,
    hjust = 0.5
  ) 

title

top_row <- plot_grid(P1, P2, P3, ncol=3,
                     labels = c('A','B','C'))
bottom_row <- plot_grid(P4, P5, ncol=2,
                        labels = c('D','E'))

full_plot <- plot_grid(title, top_row, bottom_row, ncol=1,
                       rel_heights = c(0.14,2,3))

full_plot


ggsave("figures/depth_inlab_realworld_panel.png", 
       plot=full_plot, dpi=600, 
       width=6.5, height=6.5, units="in")




# --- Try all? 

col_vec <- brewer.pal(n=5,"Dark2")
col_vec

mds_df %>% glimpse()


all_sz <- 0.25
all_col <- "#7570B3"
all_alf <- 0.5

rw_loso_sz <- 2
rw_loso_alf <- 0.25
rw_loso_col <- "#377EB8"

inlab_loso_sz <- 2
inlab_loso_alf <- 0.25
inlab_loso_col <- "#E41A1C"


lim_range <- c(-6,6)
lim_breaks <- seq(lim_range[1], lim_range[2], by=2)

mds_df %>% 
  filter(subject != demo_subject,
         cohort == 'cohort1_inlab') %>%
  ggplot(aes(x=mds_x, y=mds_y)) + 
  geom_point(size = all_sz, alpha = all_alf, color = all_col, pch=16) + 
  geom_point(data = df_new, 
             pch=16, size=rw_loso_sz, color=rw_loso_col,
             alpha = rw_loso_alf) + 
  geom_point(data = df_ref, 
             pch=16, size=inlab_loso_sz, color=inlab_loso_col,
             alpha = inlab_loso_alf) + 
  scale_x_continuous(limits=lim_range, breaks = lim_breaks) +
  scale_y_continuous(limits=lim_range, breaks = lim_breaks) + 
  theme_bw() + 
  panel_theme





