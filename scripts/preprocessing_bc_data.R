######################################
#        PREPROCESSING  DATA         #
######################################


# Packages Needed for Data Manipulation:

install.packages("tidyverse")
library(tidyverse)

#Read in datasets 
patients_df <- read_csv("data/raw/breast_cancer_dataset/patient_profiles.csv")
treatment_df <- read_csv("data/raw/breast_cancer_dataset/treatment_journey.csv")


glimpse(patients_df)
glimpse(treatment_df)

#######################################
#       JOINING DATASETS              #
#######################################


# check number of unique patients 
n_distinct(patients_df$Patient_ID)
n_distinct(treatment_df$Patient_ID)

#create joined dataset for analysis
bc_analysis_df <- treatment_df %>% 
  left_join(
    patients_df, by = "Patient_ID"
  )

dim(bc_analysis_df) 
#4981   12


######################################
#          DATA CLEANING             #
######################################


# Missing values: No missing values
sapply(bc_analysis_df, function(x) sum(is.na(x)))



# Numeric Ranges
sapply(
  bc_analysis_df[, sapply(bc_analysis_df, is.numeric)],
  range,
  na.rm = TRUE
)

sapply(
 bc_analysis_df[, sapply(bc_analysis_df, is.numeric)],
 function(x) sum(x < 0, na.rm = TRUE))

imp_val <- bc_analysis_df %>% 
  filter(
    Tumor_Shrinkage_Pct < 0 | Tumor_Shrinkage_Pct > 100 | CA15_3_Level < 0
  )

#------------------------------------------------------------------------------
#Note:
# BC-00598 exclude due to violation of constraints. Consists of illogical values.
# such as 103% shrinkage and CA15_3 marker = -1.49 on a scale of (0-30 ideally)
#-------------------------------------------------------------------------------

#filter out dataset to include observations within range
bc_analysis_df <- bc_analysis_df %>% 
  filter(
    Tumor_Shrinkage_Pct <= 100, CA15_3_Level >= 0
  )


# Duplicated Rows: No duplicates 
sum(duplicated(bc_analysis_df))

dup <- bc_analysis_df %>% 
  filter(duplicated(.))

# Duplicated  patient-round records: None
dup_id <- count(bc_analysis_df,Patient_ID) %>% 
  filter(n>2)

dup_id_trt <- count(bc_analysis_df,Patient_ID,Treatment_Round) %>% 
  filter(n>1)

# Unique Categories
unique(bc_analysis_df["Molecular_Subtype"])
unique(bc_analysis_df["Regimen"])
unique(bc_analysis_df["Response_Status"])

count(bc_analysis_df, Molecular_Subtype, sort = TRUE)
count(bc_analysis_df, Regimen, sort = TRUE)
count(bc_analysis_df, Response_Status, sort = TRUE)



###################################################
#           ENCODING DATA TYPES                   #
###################################################
glimpse(patients_df)
glimpse(treatment_df)

# Rows: 2,500
# Columns: 6
# $ Patient_ID            <chr> "BC-00001", "BC-00002", "BC-0.
# $ Age_at_Diagnosis      <dbl> 60, 53, 62, 73, 52, 52, 73, 6.
# $ Molecular_Subtype     <chr> "Triple-Negative", "HER2-Enri.
# $ BRCA_Mutation         <dbl> 0, 0, 0, 0, 1, 0, 0, 0, 0, 1,.
# $ Tumor_Grade           <dbl> 1, 1, 1, 1, 1, 3, 2, 1, 2, 3,.
# $ Initial_Tumor_Size_mm <dbl> 15.11, 11.67, 44.78, 11.24, 1.

# Rows: 4,981
# Columns: 7
# $ Patient_ID           <chr> "BC-00001", "BC-00001", "BC-00.
# $ Treatment_Round      <dbl> 1, 2, 1, 1, 2, 1, 2, 1, 2, 1, .
# $ Regimen              <chr> "AC-T (Adriamycin/Cytoxan/Taxo.
# $ Tumor_Shrinkage_Pct  <dbl> 37.36, 22.61, 85.66, 30.25, 32.
# $ CA15_3_Level         <dbl> 28.13, 21.77, 10.99, 61.34, 41.
# $ Side_Effect_Severity <dbl> 4, 7, 6, 9, 3, 8, 5, 8, 7, 6, .
# $ Response_Status      <chr> "Partial", "Partial", "Complet.

# Need to encode Patient ID as factor (specific to each patient)
# molecular subtype needs to be encoded as one hot encoding/dummy (no order)
# BRCA mutation is binary 0,1 (already one hot encoded for us)
# Grade is a scale: 1-3 should be categorical (ordinal since grade intensifies)

# Trt round: sequential indicator round 1 vs 2 time (factor)
# Regimen: therapy cocktail (nominal no order - one hot encoding/dummy)
# tumor shrink: continuous measurement
# ca15_3 level: concentration continuous measurement
# Side effect: scale 1-10 (categorical ordered)
# response status: categorical (nominal no order)


# categorical variables

bc_analysis_df <- bc_analysis_df %>% 
  mutate(
    Patient_ID = factor(Patient_ID),
    Molecular_Subtype = factor(Molecular_Subtype),
    BRCA_Mutation = factor(BRCA_Mutation, levels = c(0,1),labels = c("No","Yes")),
    Tumor_Grade = ordered(Tumor_Grade, levels = c(1,2,3)),
    Treatment_Round = factor(Treatment_Round, levels = c(1,2), labels = c("Round_1","Round_2")),
    Regimen = factor(Regimen),
    Side_Effect_Severity = ordered(Side_Effect_Severity, levels = 1:10 ),
    Response_Status = factor(Response_Status)
  )


glimpse(bc_analysis_df)

#save the cleaned dataset
saveRDS(bc_analysis_df,"data/processed/bc_analysis_clean.rds")

test_df <- readRDS("data/processed/bc_analysis_clean.rds")
glimpse(test_df)


