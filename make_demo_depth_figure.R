# Toy example of depth analysis


library(tidyverse)
library(viridis)
library(cowplot)
library(knitr)
library(MASS)
library(ddalpha)

select <- dplyr::select #MASS masking

ax_fnt <- 12
title_fnt <- 12
title_fnt <- 12


depth_cmap <- "plasma" #plasma looks best, viridis is ok too

set.seed(42)


n_points <- 500
sigma <- matrix(c(0.8,0.4,0.4,1.5),2,2)
x <- mvrnorm(n_points, mu = c(0,0), Sigma = sigma)

x_depth <- depth.halfspace(x = x,data = x, exact=TRUE)
df <- as.data.frame(x)
df$depth = x_depth
deepest_ix <- which.max(x_depth)
deepest_point <- df %>% slice(deepest_ix)

ref_pt <- data.frame(V1=c(-1.85), V2 = c(-2))
ref_txt <- data.frame(V1=c(-1.85), V2 = c(-2.6)) #d
ref_txt_D <- data.frame(V1=c(3.5), V2 = c(3.5)) #D
ann_fnt <- 4

left_plot <- df %>%
  ggplot(aes(x=V1, y=V2)) + 
  geom_point(size=2) + 
  scale_x_continuous(limits = c(-5,5), name = "Variable 1") + 
  scale_y_continuous(limits = c(-5,5), name = "Variable 2") +  
  geom_point(aes(x=V1, y=V2), data=ref_pt, inherit.aes=FALSE,
             pch = 21, color="black", fill="red",
             size=4) + 
  annotate(geom="text", x=ref_txt$V1[1], y=ref_txt$V2[1],
           color="red", size=ann_fnt, label = "d",
           fontface = "bold.italic") + 
  annotate(geom="text", x=ref_txt_D$V1[1], y=ref_txt_D$V2[1],
           color="black", size=ann_fnt+1, label = "D",
           fontface = "bold.italic") + 
  scale_color_viridis(option=depth_cmap) + 
  ggtitle("Reference data") +
  coord_equal() + 
  theme_bw() + 
  theme(legend.position = "none",
        axis.title = element_text(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust=0.5))

right_plot <- df %>%
  ggplot(aes(x=V1, y=V2, color=depth)) + 
  geom_point(size=2) + 
  scale_x_continuous(limits = c(-5,5), name = "Variable 1") + 
  scale_y_continuous(limits = c(-5,5), name = "Variable 2") + 
  #lims(x=c(-5,5), y=c(-5,5)) + 
  #geom_text(aes(x=V1, y=V2, label="X"), data=deepest_point, inherit.aes=FALSE,
  #          fontface = "bold", size=5, color="red") +
  geom_abline(slope=-8/2.5, intercept=-8,
              linetype="solid", linewidth=1, color="blue") + 
  geom_point(aes(x=V1, y=V2), data=ref_pt, inherit.aes=FALSE,
             pch = 21, color="black", fill="red",
             size=4) + 
  ggtitle("Colored by depth") +
  coord_equal() + 
  scale_color_viridis(option=depth_cmap) + 
  theme_bw() + 
  theme(legend.position = "none",
        axis.title = element_text(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust=0.5))

plot_grid(left_plot, right_plot, ncol=2)






# ---------------------------------------------

newdata_color <- "black"


#New data offset and different disburtion
n_points <- 200
sigma <- matrix(c(1.3,-0.6,-0.6,0.35),2,2)
x_new <- mvrnorm(n_points, mu = c(1.5,-0.75), Sigma = sigma)
x_depth_new <- depth.halfspace(x = x_new,data = x, exact=TRUE)
df_new <- as.data.frame(x_new)
df_new$depth = x_depth_new
#deepest_ix <- which.max(x_depth)
#deepest_point <- df %>% slice(deepest_ix)


ref_txt_Dstar <- data.frame(V1=c(3.5), V2 = c(-3.5)) #D


left_plot_new <- df_new %>%
  ggplot(aes(x=V1, y=V2)) + 
  geom_point(data = df, color="gray",
             size=1) + 
  geom_point(size=2, color = newdata_color) + 
  annotate(geom="text", x=ref_txt_Dstar$V1[1], y=ref_txt_Dstar$V2[1],
           color="black", size=ann_fnt+1, label = "D*",
           fontface = "bold.italic") + 
  scale_x_continuous(limits = c(-5,5), name = "Variable 1") + 
  scale_y_continuous(limits = c(-5,5), name = "Variable 2") + 
  ggtitle("New data in black") +
  coord_equal() + 
  theme_bw() + 
  theme(legend.position = "none",
        axis.title = element_text(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust=0.5))

#Convex hull!
hull_ix <- chull(df$V1, df$V2)
hull_ix <- c(hull_ix,hull_ix[1]) #Whyyyyy do you need to append this?? R is so fussy
hull_poly <- df %>% slice(hull_ix)


depth_cutoff <- quantile(df$depth, 0.05)
df_new <- df_new %>%
  mutate(above_threshold_fct = ifelse(depth > depth_cutoff, "Above", "Below"))
df_new$above_cutoff = (df_new$depth > depth_cutoff)

demo_pct_above = sum(df_new$above_cutoff)/dim(df_new)[1]


#NEW convex hull of 95%
hull_df <- df %>% filter(depth > depth_cutoff)
hull_ix <- chull(hull_df$V1, hull_df$V2)
hull_ix <- c(hull_ix,hull_ix[1]) 
hull_poly <- hull_df %>% slice(hull_ix)

right_plot_new <- df_new %>%
  ggplot(aes(x=V1, y=V2, color=depth)) + 
  geom_point(data = df, color="gray",
             size=1) + 
  geom_point(size=2) + 
  geom_polygon(aes(x=V1, y=V2), data=hull_poly, inherit.aes=FALSE,
               fill=NA, color="forestgreen") +
  scale_x_continuous(limits = c(-5,5), name = "Variable 1") + 
  scale_y_continuous(limits = c(-5,5), name = "Variable 2") + 
  ggtitle("Depth vs. reference data") +
  scale_color_viridis(option=depth_cmap) + 
  coord_equal() + 
  theme_bw() + 
  theme(legend.position = "none",
        axis.title = element_text(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust=0.5))

# --- Assemble full plot 

quad_fig <- plot_grid(left_plot, right_plot, left_plot_new, right_plot_new, ncol=2,
                      labels = c('A','B','C','D'))



quad_fig

ggsave("figures/demo_depth_quadplot.png", 
       plot=quad_fig, dpi=600, 
       width=6.5, height=6.5, units="in")





