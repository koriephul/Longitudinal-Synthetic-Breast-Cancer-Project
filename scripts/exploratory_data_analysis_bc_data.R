########################################
#      EXPLORATORY DATA ANALYSIS       #
########################################

library(see)
library(gt)
library(ggrepel)
library(lattice)



head(bc_analysis_df)

# number of observations: 4980
# number of variables: 12
glimpse(bc_analysis_df)


# What does each variable look like? 

## - what is the average, and average deviation from the mean? mean and std. 
## - what does the distribution look like?
## - are there any common categories
## - how are different categories distributed?

#----------------------------------------
#           SUMMARY STATISTICS          #
#----------------------------------------

# categorical variable: counts
cat_vars <- c(
  "Molecular_Subtype",
  "Response_Status",
  "Tumor_Grade",
  "BRCA_Mutation",
  "Treatment_Round",
  "Regimen"
)

bc_cat_table <- bc_analysis_df %>%
  select(all_of(cat_vars)) %>%
  mutate(across(everything(),as.character)) %>% 
  pivot_longer(
    cols = everything(),
    names_to = "Variable",
    values_to = "Category"
  ) %>%
  count(Variable, Category) %>%
  group_by(Variable) %>%
  ungroup() %>%
  gt() %>%
  tab_header(
    title = "Descriptive Statistics",
    subtitle = "Categorical Variables"
  ) %>%
  cols_label(
    Variable = "Variable",
    Category = "Category",
    n = "Count",
  ) %>%
  fmt_number(
    columns = n,
    decimals = 1
  )

bc_cat_table


# continuous variables

num_vars <- c(
  "Age_at_Diagnosis",
  "Initial_Tumor_Size_mm",
  "CA15_3_Level",
  "Tumor_Shrinkage_Pct"
)


bc_num_table <- bc_analysis_df %>%
  select(all_of(num_vars)) %>%
  mutate(across(everything(),as.numeric)) %>% 
  pivot_longer(
    cols = everything(),
    names_to = "Variable",
    values_to = "Measurements") %>%
  group_by(Variable) %>%
  summarise(
    n = n(),
    mean = mean(Measurements, na.rm = TRUE),
    sd = sd( Measurements,na.rm = TRUE),
    min = min(Measurements, na.rm = TRUE),
    p_0.25 = quantile(Measurements, probs = 0.25,na.rm = TRUE),
    median = median(Measurements,na.rm = TRUE),
    p_0.75 = quantile(Measurements, probs = 0.75,na.rm = TRUE),
    max = max(Measurements, na.rm = TRUE),
    .groups = "drop"
    )%>%
  gt() %>%
  tab_header(
    title = "Descriptive Statistics",
    subtitle = "Numerical Variables"
  ) %>%
  cols_label(
    Variable = "Variable",
    n = "N",
    mean = "Mean",
    sd = "Standard Deviation",
    min = "Minimum",
    p_0.25 = "25th Percentile",
    median = "Median",
    p_0.75 = "75th Percentile",
    max = "Maximum"
  ) %>%
  fmt_number(
    columns = c(n, mean, sd, min, p_0.25, median, p_0.75,max),
    decimals = 1
  )

bc_num_table

bc_age <- bc_analysis_df %>% 
  filter(Age_at_Diagnosis >= 80) %>% 
  select(Patient_ID,Age_at_Diagnosis, Tumor_Grade,Response_Status)

#---------------------------------------------------------
# NOTE:
# Potential outlier:
# One patient diagnosed at age 101.
# Value retained because biologically plausible.
# Sensitivity analysis may be conducted later.
#---------------------------------------------------------


#######################################
#                PLOTS                #
#######################################


# Using ggplot

# Scatter Plot 

ggplot(bc_analysis_df, aes(Tumor_Shrinkage_Pct,CA15_3_Level, group = Patient_ID))+
  geom_line(alpha = 0.15)+
  geom_point(aes(color = Treatment_Round), alpha = 0.5)+
  geom_smooth(aes(group = 1),method = "lm",color = "black", se = FALSE)+
  labs(
    x = "Tumor Shrinkage %",
    y = "CA15_3 Blood Serum Levels",
    title = "Relationship between Blood Serum Levels and Tumor Shrinkage"
  )

ggplot(bc_analysis_df, aes(Tumor_Shrinkage_Pct,CA15_3_Level))+
  geom_point(alpha = 0.5)+
  geom_smooth(color = "red",method = "lm", se = FALSE)+
  labs(
    x = "Tumor Shrinkage %",
    y = "CA15_3 Blood Serum Levels",
    title = "Relationship between Blood Serum Levels and Tumor Shrinkage"
  )+
  facet_wrap(~Treatment_Round)

# Spaghetti Plots

ggplot(bc_analysis_df, aes(Treatment_Round,CA15_3_Level, group = Patient_ID))+
  geom_line(alpha = 0.15)+
  geom_point(aes(color = Treatment_Round),alpha = 0.2)+
  geom_smooth(method = "lm",color = "black", linewidth = 0.01,se = FALSE)+
  labs(
    x = "Treatment_ Round",
    y = "CA15_3 Blood Serum Levels",
    title = "Relationship between Blood Serum Levels and Treatment Round"
  )


ggplot(bc_analysis_df, aes(Treatment_Round,Tumor_Shrinkage_Pct, group = Patient_ID))+
  geom_line(alpha = 0.15)+
  geom_point(aes(color = Treatment_Round),alpha = 0.5)+
  geom_smooth(method = "lm",color = "black", linewidth = 0.01,se = FALSE)+
  labs(
    x = "Treatment Round",
    y = "Tumor Shrinkage %",
    title = "Relationship between Tumor Shrinkage and Treatment Round"
  )

# subsetted Spaghetti plots to see subject-specific variability better

set.seed(42)

sample_ids <- sample(unique(bc_analysis_df$Patient_ID), size = 200)

bc_sample <- bc_analysis_df %>% 
  filter(Patient_ID %in% sample_ids)

ggplot(bc_sample, aes(Treatment_Round,CA15_3_Level, group = Patient_ID))+
  geom_line(alpha = 0.15)+
  geom_point(aes(color = Treatment_Round),alpha = 0.2)+
  geom_smooth(method = "lm",color = "black", linewidth = 0.01,se = FALSE)+
  labs(
    x = "Treatment_ Round",
    y = "CA15_3 Blood Serum Levels",
    title = "Relationship between Blood Serum Levels and Treatment Round"
  )

ggplot(bc_sample, aes(Treatment_Round,Tumor_Shrinkage_Pct, group = Patient_ID))+
  geom_line(alpha = 0.15)+
  geom_point(aes(color = Treatment_Round),alpha = 0.5)+
  geom_smooth(method = "lm",color = "black", linewidth = 0.01,se = FALSE)+
  labs(
    x = "Treatment Round",
    y = "Tumor Shrinkage %",
    title = "Relationship between Tumor Shrinkage and Treatment Round"
  )


# Violin Plot 

outliers_bc <- bc_analysis_df %>% 
  group_by(Treatment_Round) %>% 
  mutate(
    is_outlier = CA15_3_Level %in% boxplot.stats(CA15_3_Level)$out
  ) %>% 
  filter(is_outlier == TRUE) %>% 
  ungroup() %>% 
  select(Patient_ID,Treatment_Round,CA15_3_Level,Tumor_Shrinkage_Pct)
  
outliers_bc_tabl <- outliers_bc %>% 
  gt() %>%
  tab_header(
    title = "Statistical Outliers",
  ) %>%
  cols_label(
    Patient_ID = "Pat_ID",
    Treatment_Round = "Trt_Rd",
    CA15_3_Level = "CA15_3_Biomrk",
    Tumor_Shrinkage_Pct = "Tumor_Shrnk_%"
  ) %>%
  fmt_number(
    columns = c(CA15_3_Level,Tumor_Shrinkage_Pct),
    decimals = 1
  )

outliers_bc_tabl

violin_plt <- ggplot(bc_analysis_df, aes(Tumor_Shrinkage_Pct,CA15_3_Level)) +
  geom_violinhalf(fill = "lightblue", color = "black",alpha = 0.6 )+
  geom_boxplot(width = 0.05,
               outlier.colour = "red",outlier.shape = "triangle",)+
  facet_wrap(~Treatment_Round)+
  coord_flip()

violin_plt



#outliers 

outliers_sum <- outliers_bc %>% 
  summarise(
    mean_CA15 = mean(CA15_3_Level),
    mean_shrinkage = mean(Tumor_Shrinkage_Pct),
  )

mean_comp <- bc_analysis_df %>% 
  group_by(Treatment_Round) %>% 
  summarise(
    mean_ca15 = mean(CA15_3_Level),
    mean_tum = mean(Tumor_Shrinkage_Pct)
  )

outliers_mean <- outliers_bc %>% 
  group_by(Treatment_Round) %>% 
  summarise(
    mean_CA15 = mean(CA15_3_Level),
    mean_shrinkage = mean(Tumor_Shrinkage_Pct),
  )

overall_mean <- mean_comp %>% 
  left_join(
    outliers_mean, by = "Treatment_Round"
  )



