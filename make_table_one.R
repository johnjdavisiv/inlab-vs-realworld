

#Make aim 3 table one
library(tidyverse)
select <- dplyr::select

base_path <- "C:/Users/johnj/Google Drive/IU Grad school/Dissertation/Data/"


parse_pace <- function(pace_string){
  pace_min <- sub(":.*", "", pace_string) %>% as.integer()
  pace_sec <- sub(".*:", "", pace_string) %>% as.integer()
  pace_min_mi <- pace_min + pace_sec/60
  pace_m_s <- 1/(pace_min_mi)*1609.344/60
  return(pace_m_s)
}


#Qualtrics data
qtx_file <- "JDX_enrollment_data_2022_05_18.csv"
qtx_data <- read_csv(paste(base_path, qtx_file, sep="")) %>% 
  select(subject, years_running_experience, self_identified_running_category,
         typical_weekly_miles, typical_week_n_runs) %>%
  mutate(running_category = str_replace(self_identified_running_category,
                                        "I am a[n]? ", "")) %>%
  mutate(running_category = str_replace(running_category, " runner", "")) %>%
  select(-self_identified_running_category)





#For Cohort 1
use_sub_file <- "C:/Users/johnj/Google Drive/IU Grad school/Dissertation/Data/JDX_aims_use_subjects.csv"
use_subs_df <- read_csv(use_sub_file) 
all_subs <- use_subs_df$subject[use_subs_df$aim3_use==1]

anthro_file <- "JDX_subject_anthropometrics.csv"



#For Cohort 2
use_sub_file2 <- "C:/Users/johnj/Google Drive/IU Grad school/Dissertation/Data/JDX_IU_cohort_use_subject.csv"
use_subs_df2 <- read_csv(use_sub_file2) 
all_subs2 <- use_subs_df2$subject[use_subs_df2$aim3_use==1]

anthro_df <- read_csv(paste(base_path, anthro_file,sep=""), 
                      col_types = cols(preferred_pace = col_character())) %>%
  mutate(preferred_speed_m_s = parse_pace(preferred_pace)) %>%
  filter(subject %in% all_subs | grepl("^V", subject)) %>%
  left_join(qtx_data, by="subject") %>%
  mutate(weekly_mileage_km = typical_weekly_miles*1.609344,
         years_experience = years_running_experience) %>%
  mutate(bmi=mass_kg/height_m^2) %>%
  select(subject, age,height_m, mass_kg, bmi,
         weekly_mileage_km, years_experience, running_category, sex)

C1_df <- anthro_df %>%
  filter(!grepl("^V", subject)) %>%
  select(-subject)

C2_df <- anthro_df %>%
  filter(grepl("^V", subject)) %>%
  filter(subject %in% all_subs2) %>%
  select(-subject) %>%
  drop_na(age)



# --- table 1, cohort 1

C1_table1_df <- C1_df %>%
  select(-sex, -running_category) %>%
  summarize(across(everything(),  list(max = max, 
                                       q75 = function(x) quantile(x, 0.75, na.rm=TRUE),
                                       median = median,
                                       q25 = function(x) quantile(x,0.25, na.rm=TRUE),
                                       min=min), .names = "{.col}_{.fn}")) %>%
  pivot_longer(everything(), names_to = c(".value", "variable"),  names_pattern = "^(.+)_(.+)$") %>%
  mutate(sex = "",
         running_category = "")

C1_table1_df$sex[1] <- sprintf("%d M, %d F", sum(C1_df$sex=="M"), sum(C1_df$sex=="F"))

#Category string? 
cat_counts <- C1_df %>% 
  mutate(running_category = factor(running_category, levels = c("novice",
                                                                "recreational",
                                                                "competitive",
                                                                "elite"))) %>%
  group_by(running_category, .drop=FALSE) %>%
  count()

for (i in 1:4){
  C1_table1_df$running_category[i] <- sprintf("%s: %d", cat_counts$running_category[i],
                                           cat_counts$n[i])
}


# --- table 1, cohort 2

C2_table1_df <- C2_df %>%
  select(-sex, -running_category) %>%
  summarize(across(everything(),  list(max = max, 
                                       q75 = function(x) quantile(x, 0.75, na.rm=TRUE),
                                       median = median,
                                       q25 = function(x) quantile(x,0.25, na.rm=TRUE),
                                       min=min), .names = "{.col}_{.fn}")) %>%
  pivot_longer(everything(), names_to = c(".value", "variable"),  names_pattern = "^(.+)_(.+)$") %>%
  mutate(sex = "",
         running_category = "")

C2_table1_df$sex[1] <- sprintf("%d M, %d F", sum(C2_df$sex=="M"), sum(C2_df$sex=="F"))

#Category string? 
cat_counts <- C2_df %>% 
  mutate(running_category = factor(running_category, levels = c("novice",
                                                                "recreational",
                                                                "competitive",
                                                                "elite"))) %>%
  group_by(running_category, .drop=FALSE) %>%
  count()

for (i in 1:4){
  C2_table1_df$running_category[i] <- sprintf("%s: %d", cat_counts$running_category[i],
                                              cat_counts$n[i])
}


library(knitr)
library(kableExtra)


C1_table1_df %>% kable(format="html", digits=2) %>% save_kable("figures/cohort_1_anthro_table_export.html")
C2_table1_df %>% kable(format="html", digits=2) %>% save_kable("figures/cohort_2_anthro_table_export.html")





