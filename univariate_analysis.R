#Univariate analysis


library(tidyverse)
library(boot)

select <- dplyr::select #MASS overwrites select
source("overlap_analysis_functions.R") 

# --- Setup params ----

R_boot <- 10000 #bootstrap replicates (Paper: 10000)

#Variables that define gait pattern
gp_cols <- c("enhanced_speed", 
             "hrm_step_length", #speed + step length fully specifies cadence 
             "hrm_vertical_oscillation", 
             "stryd_stance_time",
             "stryd_leg_spring_stiffness") 


# (For univariate analysis)
#Percent overlapping within this range in reference distribution
univar_ref_pct <- 0.95

set.seed(42) #For bootstrap reproducibility


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
#          Univariate analysis
#
# -------------------------------------



# ----------------------------------------------------------------
#      Analysis 1: cohort1 In-lab vs real-world for same subject 
# ----------------------------------------------------------------

#Now, for each subject:

#Select just this subject's data
#Compare univariate distribution for each gait metric
#Record percent overlap 

all_cohort1_subs <- cohort1_inlab_gp$subject %>% unique()
n_cohort1_subs <- length(all_cohort1_subs)

a_one_list <- list()


for (i in 1:n_cohort1_subs){
  this_subject <- all_cohort1_subs[i]
  
  #Grab inlab and realworld for this subject 
  df_reference <- cohort1_inlab_gp %>% filter(subject == this_subject)
  df_new <- cohort1_rw %>%
    #filter(is_flat_and_straight == 1) %>% 
    select(subject, all_of(gp_cols)) %>%
    filter(subject == this_subject)
  
  #univar_ref_pct is what range (centered) we want as "within distribution". Can be up to 100%.
  quant_df_subject <- do_univar_analysis(df_reference, df_new, gp_cols, univar_ref_pct)
  
  quant_df_long <- quant_df_subject %>% 
    select(metric, percent_in_range) %>%
    mutate(subject = this_subject)
  
  a_one_list[[i]] <- quant_df_long
}

a_one_df <- bind_rows(a_one_list) %>%
  pivot_wider(names_from = metric, values_from = percent_in_range)


# ----------------------------------------------------------------
#      Analysis 1 ** FLAT **: cohort1 In-lab vs real-world for same subject 
# ----------------------------------------------------------------

#Now, for each subject:

#Select just this subject's data
#Compare univariate distribution for each gait metric
#Record percent overlap 

a_one_flat_list <- list()

for (i in 1:n_cohort1_subs){
  this_subject <- all_cohort1_subs[i]
  
  #Grab inlab and realworld for this subject 
  df_reference <- cohort1_inlab_gp %>% filter(subject == this_subject)
  df_new <- cohort1_rw %>%
    filter(is_flat_and_straight == 1) %>% 
    select(subject, all_of(gp_cols)) %>%
    filter(subject == this_subject)
  
  #univar_ref_pct is what range (centered) we want as "within distribution". Can be up to 100%.
  quant_df_subject <- do_univar_analysis(df_reference, df_new, gp_cols, univar_ref_pct)
  
  quant_df_long <- quant_df_subject %>% 
    select(metric, percent_in_range) %>%
    mutate(subject = this_subject)
  
  a_one_flat_list[[i]] <- quant_df_long
}

a_one_flat_df <- bind_rows(a_one_flat_list) %>%
  pivot_wider(names_from = metric, values_from = percent_in_range)



# ----------------------------------------------------------------
#      Analysis 2: cohort1 In-lab (all but one) vs cohort1 RW (one new)
# ----------------------------------------------------------------

#This time its very much same EXCEPT ref dist is the left-otus ubject! 


a_two_list <- list()

for (i in 1:n_cohort1_subs){
  this_subject <- all_cohort1_subs[i]
  
  #Comapre this subject (new data) to all other subjects (reference data)
  df_reference <- cohort1_inlab_gp %>% filter(subject != this_subject)
  df_new <- cohort1_rw_gp %>% filter(subject == this_subject)
  
  #univar_ref_pct is what range (centered) we want as "within distribution". Can be up to 100%.
  quant_df_subject <- do_univar_analysis(df_reference, df_new, gp_cols, univar_ref_pct)
  
  quant_df_long <- quant_df_subject %>% 
    select(metric, percent_in_range) %>%
    mutate(subject = this_subject)
  a_two_list[[i]] <- quant_df_long
}

a_two_df <- bind_rows(a_two_list) %>%
  pivot_wider(names_from = metric, values_from = percent_in_range)


# ----------------------------------------------------------------
#      Analysis 2 **flat/straight**: cohort1 In-lab (all but one) vs cohort1 RW flat and straight (one new)
# ----------------------------------------------------------------

#Now filtering RW so its only flat and straight
a_two_flat_list <- list()

for (i in 1:n_cohort1_subs){
  this_subject <- all_cohort1_subs[i]
  
  #Comapre this subject (new data) to all other subjects (reference data)
  df_reference <- cohort1_inlab_gp %>% filter(subject != this_subject)
  df_new <- cohort1_rw %>%
    filter(is_flat_and_straight == 1) %>% 
    select(subject, all_of(gp_cols)) %>%
    filter(subject == this_subject)
  
  #univar_ref_pct is what range (centered) we want as "within distribution". Can be up to 100%.
  quant_df_subject <- do_univar_analysis(df_reference, df_new, gp_cols, univar_ref_pct)
  
  quant_df_long <- quant_df_subject %>% 
    select(metric, percent_in_range) %>%
    mutate(subject = this_subject)
  a_two_flat_list[[i]] <- quant_df_long
}

a_two_flat_df <- bind_rows(a_two_flat_list) %>%
  pivot_wider(names_from = metric, values_from = percent_in_range)


# ----------------------------------------------------------------
#      Analysis 3: cohort1 inlab (all) vs cohort2 RW (all), subjectwise
# ----------------------------------------------------------------


all_cohort2_subs <- cohort2_rw_gp$subject %>% unique()
n_cohort2_subs <- length(all_cohort2_subs)


#Now filtering RW so its only flat and taright
a_three_list <- list()

for (i in 1:n_cohort2_subs){
  this_subject <- all_cohort2_subs[i]
  
  #Comapre this subject (new data) to all other subjects (reference data)
  df_reference <- cohort1_inlab_gp
  df_new <- cohort2_rw_gp %>%
    filter(subject == this_subject)
  
  #univar_ref_pct is what range (centered) we want as "within distribution". Can be up to 100%.
  quant_df_subject <- do_univar_analysis(df_reference, df_new, gp_cols, univar_ref_pct)
  
  quant_df_long <- quant_df_subject %>% 
    select(metric, percent_in_range) %>%
    mutate(subject = this_subject)
  a_three_list[[i]] <- quant_df_long
}

a_three_df <- bind_rows(a_three_list) %>%
  pivot_wider(names_from = metric, values_from = percent_in_range)



# ----------------------------------------------------------------
#      Analysis 3 **flat**: Cohort2 inlab (all) vs cohort2 RW (flat/straight only)
# ----------------------------------------------------------------


#Now filtering RW so its only flat and taright
a_three_flat_list <- list()

for (i in 1:n_cohort2_subs){
  this_subject <- all_cohort2_subs[i]
  
  #Comapre this subject (new data) to all other subjects (reference data)
  df_reference <- cohort1_inlab_gp
  df_new <- cohort2_rw %>%
    filter(is_flat_and_straight == 1) %>% 
    select(subject, all_of(gp_cols)) %>%
    filter(subject == this_subject)
  
  #univar_ref_pct is what range (centered) we want as "within distribution". Can be up to 100%.
  quant_df_subject <- do_univar_analysis(df_reference, df_new, gp_cols, univar_ref_pct)
  
  quant_df_long <- quant_df_subject %>% 
    select(metric, percent_in_range) %>%
    mutate(subject = this_subject)
  a_three_flat_list[[i]] <- quant_df_long
}

a_three_flat_df <- bind_rows(a_three_flat_list) %>%
  pivot_wider(names_from = metric, values_from = percent_in_range)



# ----------------------------------------------------------------
#      Analysis 4: cohort1 RW (all) vs cohort2 RW (all)
# ----------------------------------------------------------------


a_four_list <- list()

for (i in 1:n_cohort2_subs){
  this_subject <- all_cohort2_subs[i]
  
  #Comapre this subject (new data) to all other subjects (reference data)
  df_reference <- cohort1_rw_gp
  df_new <- cohort2_rw_gp %>%
    filter(subject == this_subject)
  
  #univar_ref_pct is what range (centered) we want as "within distribution". Can be up to 100%.
  quant_df_subject <- do_univar_analysis(df_reference, df_new, gp_cols, univar_ref_pct)
  
  quant_df_long <- quant_df_subject %>% 
    select(metric, percent_in_range) %>%
    mutate(subject = this_subject)
  a_four_list[[i]] <- quant_df_long
}

a_four_df <- bind_rows(a_four_list) %>%
  pivot_wider(names_from = metric, values_from = percent_in_range)



# ----------------------------------------------------------------
#      Analysis 4 **flat**: cohort1 rw (flat/straight only) vs cohort2 RW (flat/straight only)
# ----------------------------------------------------------------


#Now filtering RW so its only flat and taright
a_four_flat_list <- list()

for (i in 1:n_cohort2_subs){
  this_subject <- all_cohort2_subs[i]
  
  #Comapre this subject (new data) to all other subjects (reference data)
  df_reference <- cohort1_rw %>%
    filter(is_flat_and_straight == 1) %>% 
    select(subject, all_of(gp_cols)) 
  
  df_new <- cohort2_rw %>%
    filter(is_flat_and_straight == 1) %>% 
    select(subject, all_of(gp_cols)) %>%
    filter(subject == this_subject)
  
  #univar_ref_pct is what range (centered) we want as "within distribution". Can be up to 100%.
  quant_df_subject <- do_univar_analysis(df_reference, df_new, gp_cols, univar_ref_pct)
  
  quant_df_long <- quant_df_subject %>% 
    select(metric, percent_in_range) %>%
    mutate(subject = this_subject)
  a_four_flat_list[[i]] <- quant_df_long
}

a_four_flat_df <- bind_rows(a_four_flat_list) %>%
  pivot_wider(names_from = metric, values_from = percent_in_range)


# ---- 

a_one_df
a_one_flat_df
a_two_df
a_two_flat_df
a_three_df
a_three_flat_df
a_four_df
a_four_flat_df


univar_all_results <- bind_rows(
  a_one_df %>% mutate(analysis = "a_one"),
  a_one_flat_df %>% mutate(analysis = "a_one_flat"),
  a_two_df %>% mutate(analysis = "a_two"),
  a_two_flat_df %>% mutate(analysis = "a_two_flat"),
  a_three_df %>% mutate(analysis = "a_three"),
  a_three_flat_df %>% mutate(analysis = "a_three_flat"),
  a_four_df %>% mutate(analysis = "a_four"),
  a_four_flat_df %>% mutate(analysis = "a_four_flat")
)

#Write depth results
univar_all_results

univar_save_name <- paste("univariate results/",
                          "univariate_results_pct_inrange_", 
                          sprintf("%.0f", univar_ref_pct*100),
                          ".csv", sep="")

write.csv(univar_all_results, univar_save_name, row.names=FALSE)

#And for each analysis, for each column (feature), we want a mean + 95% Ci from bootstrapping



# ---------------------------------------------------------
#
#    UNIVARIATE: Bootstrap some means and CIs
#
# ---------------------------------------------------------


block_bootci <- function(a_df, gp_cols, analysis_name, R=500){
  
  row_value <- c("p025", "mean", "p975")
  sel_cols <- gp_cols
  
  boot_fn <- function(data, index){
    #Hopefully can access gp outside of local scope? 
    #Returns mean of both error metrics in bootstrapped sample
    mean_vec <- data[index,] %>% 
      select(all_of(sel_cols)) %>%
      summarize_all(mean) %>%
      unlist(.,use.names=FALSE) 
    return(mean_vec)
  }
  #Actual booting here
  boot_out <- boot(a_df, boot_fn, R=R)
  all_ci <- apply(boot_out$t, 2, quantile, probs = c(0.025, 0.975))
  all_ci_df <- as.data.frame(rbind(all_ci[1,], boot_out$t0, all_ci[2,]))
  colnames(all_ci_df) <- gp_cols
  boot_res <- cbind(row_value, all_ci_df) %>% mutate(analysis = analysis_name)
  
  return(boot_res)
}


print(paste("Bootstrapping", sprintf("%d", R_boot), "replicates..."))

a_one_boot <- block_bootci(a_one_df, gp_cols, "a_one", R=R_boot)
a_one_flat_boot <- block_bootci(a_one_flat_df, gp_cols, "a_one_flat", R=R_boot)

a_two_boot <- block_bootci(a_two_df, gp_cols, "a_two", R=R_boot)
a_two_flat_boot <- block_bootci(a_two_flat_df, gp_cols, "a_two_flat", R=R_boot)

a_three_boot <- block_bootci(a_three_df, gp_cols, "a_three", R=R_boot)
a_three_flat_boot <- block_bootci(a_three_flat_df, gp_cols, "a_three_flat", R=R_boot)

a_four_boot <- block_bootci(a_four_df, gp_cols, "a_four", R=R_boot)
a_four_flat_boot <- block_bootci(a_four_flat_df, gp_cols, "a_four_flat", R=R_boot)


#Needs a pivot but can do later when plotting
all_boot_res <- bind_rows(a_one_boot, a_one_flat_boot,
                          a_two_boot, a_two_flat_boot,
                          a_three_boot, a_three_flat_boot,
                          a_four_boot, a_four_flat_boot)


boot_save_name <- paste("univariate results/",
                        "bootstrap_results_univariate_nboot_", 
                        sprintf("%d", R_boot), 
                        "_pct_inrange_", sprintf("%.0f", univar_ref_pct*100),
                        ".csv", sep="")
write.csv(all_boot_res, boot_save_name, row.names=FALSE)


