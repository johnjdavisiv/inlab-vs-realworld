

library(tidyverse)

do_univar_analysis <- function(df_reference, df_new, gp_cols, univar_ref_pct){
  
  
  #!! Expects subject to still be in the reference dataframe !!
  
  #Take referenc percent (0.95 for example) as input
  reference_quantiles <- c((1-univar_ref_pct)/2, 1 - (1-univar_ref_pct)/2)
  
  #Pivot to long
  df_reference_long <- df_reference %>%
    select(-subject) %>% 
    pivot_longer(everything(), names_to = "metric", values_to = "value") %>%
    mutate(metric = factor(metric, levels = gp_cols))
    
  df_new_long <- df_new %>%
    select(-subject) %>% 
    pivot_longer(everything(), names_to = "metric", values_to = "value") %>%
    mutate(metric = factor(metric, levels = gp_cols))
    
    #Get quantiles from reference distribution
  quant_df <- df_reference %>%
    select(-subject) %>% 
    pivot_longer(everything(), names_to = "metric", values_to = "value") %>%
    group_by(metric) %>%
    summarize(quant_lo = quantile(value, reference_quantiles[1]),
              quant_hi = quantile(value, reference_quantiles[2])) %>%
    mutate(metric = factor(metric, levels = gp_cols))
    
  
  # Compute data in quantile ranges
  for (i in 1:length(gp_cols)){
    #For each metric, compute how much in realworld data lies within range of quant df
    this_metric <- gp_cols[i]
    
    new_data_values <- df_new %>%
      pull(this_metric)
    
    range_lo = quant_df[quant_df$metric == this_metric,]$quant_lo
    range_hi = quant_df[quant_df$metric == this_metric,]$quant_hi
    new_data_in_range = sum(new_data_values >= range_lo & new_data_values <= range_hi)/length(new_data_values)
    quant_df[quant_df$metric == this_metric,"percent_in_range"] <- new_data_in_range
  }
  return(quant_df)
}


do_depth_analysis <- function(df_reference, df_new, depth_ref_pct=0.95, n_depth_dir){
  #!! Expects subject to still be in the reference dataframe !!
  #Take referenc percent (0.95 for example) as input
  reference_quantiles <- c((1-depth_ref_pct)/2, 1 - (1-depth_ref_pct)/2)
  
  #Depth of "x" (new data) w.r.t. "data" (reference data)
  ref_x <- df_reference %>% select(-subject)
  x_new <- df_new %>% select(-subject)
  
  #First, get depth of ref_x relative to itself so we can set cutoff threshold
  ref_depth <- depth.halfspace(x = ref_x, data = ref_x, 
                               exact=FALSE, num.directions = n_depth_dir,
                               seed=42) #1000 is default
  
  depth_threshold <- quantile(ref_depth, 1 - depth_ref_pct)
  
  #Now calculate depth of new data w.r.t. reference distribution
  new_depth <- depth.halfspace(x = x_new, data = ref_x, 
                               exact=FALSE, num.directions = n_depth_dir,
                               seed=42) #1000 is default
  
  newdata_pct_above <- sum(new_depth >= depth_threshold)/length(new_depth)
  return(newdata_pct_above)
}

do_qh_depth_analysis <- function(df_reference, df_new, depth_ref_pct=0.95){
  #Take reference percent (0.95 for example) as input
  reference_quantiles <- c((1-depth_ref_pct)/2, 1 - (1-depth_ref_pct)/2)

  #Depth of "x" (new data) w.r.t. "data" (reference data)
  ref_x <- df_reference %>% select(-subject)
  x_new <- df_new %>% select(-subject)
  
  #First, get depth of ref_x relative to itself so we can set cutoff threshold
  ref_depth <- depth.qhpeeling(x = ref_x, data = ref_x) #1000 is default
  depth_threshold <- quantile(ref_depth, 1 - depth_ref_pct)
  
  #Now calculate depth of new data w.r.t. reference distribution
  new_depth <- depth.qhpeeling(x = x_new, data = ref_x) #1000 is default
  newdata_pct_above <- sum(new_depth >= depth_threshold)/length(new_depth)
  
  return(newdata_pct_above)
}




do_precalc_depth_analysis <- function(df_reference, df_new, depth_threshold, n_depth_dir){
  #For situations in loops where reference data is always the same.
  
  
  #Depth of "x" (new data) w.r.t. "data" (reference data)
  ref_x <- df_reference %>% select(-subject)
  x_new <- df_new %>% select(-subject)
  
  #Now calculate depth of new data w.r.t. reference distribution
  new_depth <- depth.halfspace(x = x_new, data = ref_x, 
                               exact=FALSE, num.directions = n_depth_dir,
                               seed=42) #1000 is default
  
  newdata_pct_above <- sum(new_depth >= depth_threshold)/length(new_depth)
  return(newdata_pct_above)
  
}



