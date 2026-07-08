############################
# STATISTICAL MODELS       #
############################

#Packages needed:
library(nlme)

head(bc_analysis_df)

#Functions for diagnostic plots: 

diagnostic_plt <- function(df,model){
  
  residualval <- resid(model)
  fittedval <- fitted(model)
  
  plot_df <- df %>% 
    mutate(
      resmod = residualval,
      fitmod = fittedval
    )
  
  re <- ranef(model)[[1]]
  re_df <- data.frame(random_intercept = as.numeric(re))
  
  # Residual vs fitted plot
  p1 <- ggplot(plot_df, aes(x = fitmod, y = resmod)) +
    geom_point(alpha = 0.7, color = "grey35") +
    geom_smooth(aes(group = 1), color = "grey20", linewidth = 0.8, se = FALSE) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    labs(
      x = "Fitted values",
      y = "Residuals",
      title = "Residuals vs Fitted"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey85"),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 11),
      plot.title = element_text(size = 13, face = "bold")
    )
  
  # QQ Plot for residuals
  p2 <- ggplot(plot_df, aes(sample = resmod)) +
    geom_qq(alpha = 0.7, color = "grey35") +
    geom_qq_line(color = "grey20", linewidth = 0.8) +
    labs(
      x = "Theoretical quantiles",
      y = "Sample quantiles",
      title = "QQ Plot of Residuals"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey85"),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 11),
      plot.title = element_text(size = 13, face = "bold")
    )
  
  # QQ Plot for random effects
  p3 <- ggplot(re_df, aes(sample = random_intercept)) +
    geom_qq(alpha = 0.7, color = "grey35") +
    geom_qq_line(color = "grey20", linewidth = 0.8) +
    labs(
      x = "Theoretical quantiles",
      y = "Sample quantiles",
      title = "QQ Plot of Random Effects"
    )+
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey85"),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 11),
      plot.title = element_text(size = 13, face = "bold")
    )
  list(
    resid_fit = p1,
    fixed_qq_plot = p2,
    re_qq_plot = p3
  )
  
}


#Baseline Model: No covariates (Null Model)

base_model <- lme(fixed = CA15_3_Level~1,
                  random = ~1 | Patient_ID,
                  data = bc_analysis_df,
                  method = "REML")

diagnostic_plt(bc_analysis_df,base_model)
summary(base_model)

#Model 1: Linear Mixed Effect Models with random intercept (baseline allowed to vary)

model1 <- lme(fixed = CA15_3_Level ~ 
                Tumor_Shrinkage_Pct +
                Treatment_Round + 
                Age_at_Diagnosis + 
                Initial_Tumor_Size_mm + 
                Side_Effect_Severity + 
                BRCA_Mutation + 
                Tumor_Grade+
                Molecular_Subtype,
              random = ~ 1 | Patient_ID,
              data = bc_analysis_df,
              method = "REML")

diagnostic_plt(bc_analysis_df,model1)
summary(model1)

#Model 2: Linear Mixed Effect Model with random slope + random intercept (Trajectories and baselines allowed to vary)

model2 <- lme(fixed = CA15_3_Level ~ 
                Tumor_Shrinkage_Pct +
                Treatment_Round + 
                Age_at_Diagnosis + 
                Initial_Tumor_Size_mm + 
                Side_Effect_Severity + 
                BRCA_Mutation + 
                Tumor_Grade+
                Molecular_Subtype,
              random = ~ Treatment_Round | Patient_ID,
              data = bc_analysis_df,
              method = "REML")

diagnostic_plt(bc_analysis_df,model2)
summary(model2)

#Model 3: Linear Mixed Effect Model with random intercept + random slope (multiple features trajectories and baseline are allowed to vary)

model3 <- lme(fixed = CA15_3_Level ~ 
                Tumor_Shrinkage_Pct +
                Treatment_Round + 
                Tumor_Shrinkage_Pct:Treatment_Round+
                Age_at_Diagnosis + 
                Initial_Tumor_Size_mm + 
                Side_Effect_Severity + 
                BRCA_Mutation + 
                Tumor_Grade+
                Molecular_Subtype,
              random = ~ Treatment_Round| Patient_ID,
              data = bc_analysis_df,
              method = "REML")

diagnostic_plt(bc_analysis_df,model3)
summary(model3)

#Model 4: Linear Mixed Effect Model with random intercept + random slope (with squared term)

model3 <- lme(fixed = CA15_3_Level ~ 
                Tumor_Shrinkage_Pct +
                Treatment_Round + 
                Tumor_Shrinkage_Pct:Treatment_Round+
                Age_at_Diagnosis + 
                Initial_Tumor_Size_mm + 
                Side_Effect_Severity + 
                BRCA_Mutation + 
                Tumor_Grade+
                Molecular_Subtype,
              random = ~ Treatment_Round| Patient_ID,
              data = bc_analysis_df,
              method = "REML")

diagnostic_plt(bc_analysis_df,model3)
summary(model3)

AIC(model2,model3)











