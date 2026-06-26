######################################
#       DOWNLOADING  DATA            #
######################################

#import data from Kaggle

#create workflow folders  
folders <- c(
  "data/raw",
  "data/processed",
  "scripts",
  "R",
  "output",
  "docs"
)

lapply(folders, dir.create, recursive = TRUE, showWarnings = FALSE)

#download and unzip Kaggle dataset in data/raw
system(
  "kaggle datasets download -d waddahali/breast-cancer-treatment-journey -p data/raw --unzip"
)

#show downloaded files
list.files("data/raw/breast_cancer_dataset")
