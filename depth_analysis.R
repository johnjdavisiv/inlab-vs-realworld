#Depth analysis

library(tidyverse)
library(ddalpha)
library(boot)

select <- dplyr::select #grr...MASS overwrites select
source("overlap_analysis_functions.R") 

# --- Setup params ----

#Variables that define gait pattern
gp_cols <- c("enhanced_speed", 
             "hrm_step_length", #speed + step length fully specifies cadence 
             "hrm_vertical_oscillation", 
             "stryd_stance_time",
             "stryd_leg_spring_stiffness")
#Adjust this list for GP sensitivity


label_names <- c("Speed",
                 "HRM: Step Length",
                 "HRM: Vertical Oscillation",
                 "Stryd: Stance Time",
                 "Stryd: Leg Stiffness")

# --- Depth analysis setup -------

n_depth_dir <- 1000 #N directions for random Tukey depth (paper: 1000)
R_boot <- 10000 #N bootstrap replicates (paper: 10000)

#Thin realworld data by this amount
rw_thin_pct <- 0.25 #keep this % of realworld data. (paper: 0.25). 
#Note: 0.25 takes ~30hrs on Ryzen 5900x. Recommend 0.05-0.10 for sensitivity, etc

#Also I've removed inlab thinning so could be even worse. A2 and A2f are the real laggards

#What percentile in counts as in distribution?
depth_ref_pct <- 0.95


set.seed(42)

cohort1_inlab <- read_csv("data/cohort1_inlab.csv")
cohort1_rw <- read_csv("data/cohort1_realworld.csv")
cohort2_course <- read_csv("data/cohort2_course.csv")
cohort2_rw <- read_csv("data/cohort2_realworld.csv")


#Gaitpattern DFs for analysis 
cohort1_inlab_gp <- cohort1_inlab %>%
  select(subject, all_of(gp_cols))
cohort1_rw_gp <- cohort1_rw %>%
  select(subject, all_of(gp_cols))
cohort2_rw_gp <- cohort2_rw %>%
  select(subject, all_of(gp_cols))


# -------------------------------------
#
#          Depth analysis 
#
# -------------------------------------

#Now, we have the following dataframes with clean data:
# cohort1_inlab
# cohort1_rw
# cohort2_course
# cohort2_rw

# And the gait pattern we care about is specified by variables in
# gp_cols


#do_depth_analysis <- function(df_reference, df_new, gp_cols, depth_ref_pct, n_depth_dir){}



# ----------------------------------------------------------------
#      Analysis 1: cohort1 In-lab vs real-world for same subject 
# ----------------------------------------------------------------

#Now, for each subject:

#Select just this subject's data
# compare inlab vs realworld and get depth

all_cohort1_subs <- cohort1_inlab_gp$subject %>% unique()
n_cohort1_subs <- length(all_cohort1_subs)


st <- Sys.time()
#  Time this way

depth_a_one_list <- list()

for (i in 1:n_cohort1_subs){
  this_subject <- all_cohort1_subs[i]
  print(sprintf("Depth analysis for A1 subject %s", this_subject))
  #Grab inlab and realworld for this subject 
  df_reference <- cohort1_inlab_gp %>% filter(subject == this_subject)
  df_new <- cohort1_rw_gp %>% filter(subject == this_subject)
  
  #Get percent of data in reference distribution
  subject_pct <- do_depth_analysis(df_reference, df_new, depth_ref_pct, n_depth_dir)
  
  depth_a_one_list[[i]] <- data.frame(subject = this_subject,
                                      depth_pct = subject_pct)
}

depth_a_one_df <- bind_rows(depth_a_one_list) 


# ----------------------------------------------------------------
#      Analysis 1 **FLAT**: cohort1 In-lab vs real-world for same subject 
# ----------------------------------------------------------------
#SAMe but flat only
#Now, for each subject:

#Select just this subject's data
# compare inlab vs realworld and get depth

depth_a_one_flat_list <- list()

for (i in 1:n_cohort1_subs){
  this_subject <- all_cohort1_subs[i]
  print(sprintf("Depth analysis for A1f subject %s", this_subject))
  #Grab inlab and realworld for this subject 
  df_reference <- cohort1_inlab_gp %>% filter(subject == this_subject)
  
  df_new <- cohort1_rw %>%
    filter(is_flat_and_straight == 1) %>% 
    select(subject, all_of(gp_cols)) %>%
    filter(subject == this_subject)
  
  #Get percent of data in reference distribution
  subject_pct <- do_depth_analysis(df_reference, df_new, depth_ref_pct, n_depth_dir)
  
  depth_a_one_flat_list[[i]] <- data.frame(subject = this_subject,
                                      depth_pct = subject_pct)
}

depth_a_one_flat_df <- bind_rows(depth_a_one_flat_list) 


# ----------------------------------------------------------------
#      Analysis 2: cohort1 In-lab (all but one) vs cohort1 RW (one new)
# ----------------------------------------------------------------

depth_a_two_list <- list()

for (i in 1:n_cohort1_subs){
  loop_start <- Sys.time()
  this_subject <- all_cohort1_subs[i]
  print(sprintf("Depth analysis for A2 subject %s", this_subject))
  
  #Comapre this subject (new data) to all other subjects (reference data)
  df_reference <- cohort1_inlab_gp %>% filter(subject != this_subject) %>%
    slice_sample(prop=rw_thin_pct)
  
  df_new <- cohort1_rw_gp %>% filter(subject == this_subject) %>% 
    slice_sample(prop=rw_thin_pct)
  
  #Get percent of data in reference distribution
  subject_pct <- do_depth_analysis(df_reference, df_new, depth_ref_pct, n_depth_dir)
  
  depth_a_two_list[[i]] <- data.frame(subject = this_subject,
                                        depth_pct = subject_pct)
  
  loop_end <- Sys.time()
  print(sprintf("A2 Time for one subject: %.1f sec",
                as.numeric(difftime(loop_end, loop_start, units="sec"))))
  
}

depth_a_two_df <- bind_rows(depth_a_two_list) 


# ----------------------------------------------------------------
#      Analysis 2 **flat/straight**: cohort1 In-lab (all but one) vs 
#          cohort1 RW flat and straight (one new)
# ----------------------------------------------------------------

#Now filtering RW so its only flat and taright
depth_a_two_flat_list <- list()

for (i in 1:n_cohort1_subs){
  loop_start <- Sys.time()
  
  this_subject <- all_cohort1_subs[i]
  print(sprintf("Depth analysis for A2f subject %s", this_subject))
  
  #Comapre this subject (new data) to all other subjects (reference data)
  df_reference <- cohort1_inlab_gp %>% filter(subject != this_subject) %>%
    slice_sample(prop=rw_thin_pct)
  
  df_new <- cohort1_rw %>%
    filter(is_flat_and_straight == 1) %>% 
    select(subject, all_of(gp_cols)) %>%
    filter(subject == this_subject) %>% 
    slice_sample(prop=rw_thin_pct)
  
  #Get percent of data in reference distribution
  subject_pct <- do_depth_analysis(df_reference, df_new, depth_ref_pct, n_depth_dir)
  
  depth_a_two_flat_list[[i]] <- data.frame(subject = this_subject,
                                             depth_pct = subject_pct)
  loop_end <- Sys.time()
  print(sprintf("A2f Time for one subject: %.1f sec",
                as.numeric(difftime(loop_end, loop_start, units="sec"))))
  
}

depth_a_two_flat_df <- bind_rows(depth_a_two_flat_list) 


# ----------------------------------------------------------------
#      Analysis 3: cohort1 inlab (all) vs cohort2 RW (all)
# ----------------------------------------------------------------

all_cohort2_subs <- cohort2_rw_gp$subject %>% unique()
n_cohort2_subs <- length(all_cohort2_subs)

depth_a_three_list <- list()


#Precalculate depth threshold so we don't repeat
df_reference <- cohort1_inlab_gp

reference_quantiles <- c((1-depth_ref_pct)/2, 1 - (1-depth_ref_pct)/2)

ref_x <- df_reference %>% select(-subject)
ref_depth <- depth.halfspace(x = ref_x, data = ref_x, 
                             exact=FALSE, num.directions = n_depth_dir,
                             seed=42) #1000 is default
depth_threshold <- quantile(ref_depth, 1 - depth_ref_pct)


for (i in 1:n_cohort2_subs){
  this_subject <- all_cohort2_subs[i]
  print(sprintf("Depth analysis for A3 subject %s", this_subject))
  
  df_new <- cohort2_rw_gp %>%
    filter(subject == this_subject) %>%
    slice_sample(prop=rw_thin_pct)
  
  #Get percent of data in reference distribution
  # --- Diferrent funciton!! --
  subject_pct <- do_precalc_depth_analysis(df_reference, df_new, depth_threshold, n_depth_dir)
  
  depth_a_three_list[[i]] <- data.frame(subject = this_subject,
                                       depth_pct = subject_pct)
}

depth_a_three_df <- bind_rows(depth_a_three_list) 



# ----------------------------------------------------------------
#      Analysis 3 **flat**: cohort1 inlab (all) vs cohort2 RW (flat/straight only)
# ----------------------------------------------------------------


depth_a_three_flat_list <- list()
#Can use same precalc depth threshold because ref distribution does not change

for (i in 1:n_cohort2_subs){
  this_subject <- all_cohort2_subs[i]
  print(sprintf("Depth analysis for A3f subject %s", this_subject))
  
  df_new <- cohort2_rw %>%
    filter(is_flat_and_straight == 1) %>% 
    select(subject, all_of(gp_cols)) %>%
    filter(subject == this_subject) %>%
    slice_sample(prop=rw_thin_pct)
  
  #Get percent of data in reference distribution
  subject_pct <- do_precalc_depth_analysis(df_reference, df_new, depth_threshold, n_depth_dir)
  
  depth_a_three_flat_list[[i]] <- data.frame(subject = this_subject,
                                            depth_pct = subject_pct)
}

depth_a_three_flat_df <- bind_rows(depth_a_three_flat_list) 


# ----------------------------------------------------------------
#      Analysis 4: cohort1 RW (all) vs cohort2 RW (all)
# ----------------------------------------------------------------


#Might have to downsample this one
#Also should be possible to refactor it to get a ~2x speedup because 
# reference df does not change

#Now filtering RW so its only flat and taright
depth_a_four_list <- list()

#Precalculate depth threshold so we don't repeat
df_reference <- cohort1_rw_gp %>%
  slice_sample(prop=rw_thin_pct)
reference_quantiles <- c((1-depth_ref_pct)/2, 1 - (1-depth_ref_pct)/2)

ref_x <- df_reference %>% select(-subject)
ref_depth <- depth.halfspace(x = ref_x, data = ref_x, 
                             exact=FALSE, num.directions = n_depth_dir,
                             seed=42) 
depth_threshold <- quantile(ref_depth, 1 - depth_ref_pct)


for (i in 1:n_cohort2_subs){
  this_subject <- all_cohort2_subs[i]
  print(sprintf("Depth analysis for A4 subject %s", this_subject))
  
  #Comapre this subject (new data) to all other subjects (reference data)
  df_new <- cohort2_rw_gp %>%
    filter(subject == this_subject) %>%
    slice_sample(prop=rw_thin_pct)
  
  #Get percent of data in reference distribution
  subject_pct <- do_precalc_depth_analysis(df_reference, df_new, 
                                           depth_threshold, n_depth_dir)
  
  depth_a_four_list[[i]] <- data.frame(subject = this_subject,
                                       depth_pct = subject_pct)
}

depth_a_four_df <- bind_rows(depth_a_four_list) 




# ----------------------------------------------------------------
#      Analysis 4 **flat**: cohort1 rw (flat/straight only) vs cohort2 RW (flat/straight only)
# ----------------------------------------------------------------


#Now filtering RW so its only flat and taright
depth_a_four_flat_list <- list()

#Precalculate depth threshold so we don't repeat
# DOES need to be new this time b/c reference is flat/straight only
df_reference <- cohort1_rw %>%
  filter(is_flat_and_straight == 1) %>% 
  select(subject, all_of(gp_cols)) %>%
  slice_sample(prop=rw_thin_pct)

reference_quantiles <- c((1-depth_ref_pct)/2, 1 - (1-depth_ref_pct)/2)

ref_x <- df_reference %>% select(-subject)
ref_depth <- depth.halfspace(x = ref_x, data = ref_x, 
                             exact=FALSE, num.directions = n_depth_dir,
                             seed=42) 
depth_threshold <- quantile(ref_depth, 1 - depth_ref_pct)

for (i in 1:n_cohort2_subs){
  this_subject <- all_cohort2_subs[i]
  print(sprintf("Depth analysis for A4f subject %s", this_subject))
  
  #Comapre this subject (new data) to all other subjects (reference data)
  df_new <- cohort2_rw %>%
    filter(is_flat_and_straight == 1) %>% 
    select(subject, all_of(gp_cols)) %>%
    filter(subject == this_subject) %>%
    slice_sample(prop=rw_thin_pct)
  
  #Get percent of data in reference distribution
  subject_pct <- do_precalc_depth_analysis(df_reference, df_new, 
                                           depth_threshold, n_depth_dir)
  
  depth_a_four_flat_list[[i]] <- data.frame(subject = this_subject,
                                            depth_pct = subject_pct)
}

depth_a_four_flat_df <- bind_rows(depth_a_four_flat_list) 

# --------------------------------------------------
#
#              Bootstrapping depth results
#
# --------------------------------------------------


#Now... bootstrap these just like we did with univariate results
depth_all_results <- bind_rows(
  depth_a_one_df %>% mutate(analysis = "a_one"),
  depth_a_one_flat_df %>% mutate(analysis = "a_one_flat"),
  depth_a_two_df %>% mutate(analysis = "a_two"),
  depth_a_two_flat_df %>% mutate(analysis = "a_two_flat"),
  depth_a_three_df %>% mutate(analysis = "a_three"),
  depth_a_three_flat_df %>% mutate(analysis = "a_three_flat"),
  depth_a_four_df %>% mutate(analysis = "a_four"),
  depth_a_four_flat_df %>% mutate(analysis = "a_four_flat")
)

#Write depth results
depth_all_results
depth_save_name <- paste("depth results/",
                         "depth_results_ndir_", 
                         sprintf("%d", n_depth_dir),
                         "_pct_thin_", sprintf("%.0f", rw_thin_pct*100),
                         "_pct_inrange_", sprintf("%.0f", depth_ref_pct*100),
                         "_nGP_", sprintf("%d", length(gp_cols)),
                         ".csv", sep="")
write.csv(depth_all_results, depth_save_name, row.names=FALSE)

#Log elapsed time
et <- Sys.time()
e_time <- as.numeric(difftime(et, st, units="secs"))
time_string <- sprintf("num_dir %d pct_thin %.2f total_time %.1f sec",
                       n_depth_dir, rw_thin_pct, e_time)
# Format the date and time as a string
walltime_filename <- paste("depth results/",
                           "walltime_ndir_", sprintf("%d", n_depth_dir),
                           "_pct_thin_",sprintf("%.0f", rw_thin_pct*100),
                           "_date_",
                           format(Sys.time(), "%Y_%m_%d_time_%H_%M_%S"),
                           ".txt", sep="")
writeLines(time_string, walltime_filename)

# ---------------------------------------------------------
#
#    DEPTH: Bootstrap some means and CIs
#
# ---------------------------------------------------------


#Ugh need fast method here...

depth_bootci <- function(depth_a_df, analysis_name, R=500){
  
  row_value <- c("p025", "mean", "p975")
  
  boot_fn <- function(data, index){
    #Hopefully can access gp outside of local scope? 
    #Returns mean of both error metrics in bootstrapped sample
    mean_vec <- data[index,] %>% 
      select(depth_pct) %>%
      summarize_all(mean) %>%
      unlist(.,use.names=FALSE) 
    return(mean_vec)
  }
  #Actual booting here
  boot_out <- boot(depth_a_df, boot_fn, R=R)
  all_ci <- apply(boot_out$t, 2, quantile, probs = c(0.025, 0.975))
  all_ci_df <- as.data.frame(rbind(all_ci[1,], boot_out$t0, all_ci[2,]))
  colnames(all_ci_df) <- "depth"
  boot_res <- cbind(row_value, all_ci_df) %>% mutate(analysis = analysis_name)
  
  return(boot_res)
}


print(paste("Bootstrapping", sprintf("%d", R_boot), "replicates..."))

depth_a_one_boot <- depth_bootci(depth_a_one_df, "a_one", R=R_boot)
depth_a_one_flat_boot <- depth_bootci(depth_a_one_df, "a_one_flat", R=R_boot)

depth_a_two_boot <- depth_bootci(depth_a_two_df, "a_two", R=R_boot)
depth_a_two_flat_boot <- depth_bootci(depth_a_two_flat_df, "a_two_flat", R=R_boot)

depth_a_three_boot <- depth_bootci(depth_a_three_df, "a_three", R=R_boot)
depth_a_three_flat_boot <- depth_bootci(depth_a_three_flat_df, "a_three_flat", R=R_boot)

depth_a_four_boot <- depth_bootci(depth_a_four_df, "a_four", R=R_boot)
depth_a_four_flat_boot <- depth_bootci(depth_a_four_flat_df, "a_four_flat", R=R_boot)

#Needs a pivot but can do later when plotting
depth_all_boot_res <- bind_rows(depth_a_one_boot, depth_a_one_flat_boot,
                                depth_a_two_boot, depth_a_two_flat_boot,
                                depth_a_three_boot, depth_a_three_flat_boot,
                                depth_a_four_boot, depth_a_four_flat_boot
                                )

boot_save_name <- paste("depth results/", 
                        "bootstrap_results_depth_ndir_",
                        sprintf("%d", n_depth_dir),
                        "_pct_thin_", sprintf("%.0f", rw_thin_pct*100),
                        "_nboot_", sprintf("%d", R_boot), 
                        "_nGP_", sprintf("%d", length(gp_cols)), 
                        ".csv", sep="")
write.csv(depth_all_boot_res, boot_save_name, row.names=FALSE)







