### This R script file is for testing accuracy of algorithm-based classifications 
### by comparing them with manual classifications. 
###
### The code can not be reproduced using the data stored at the GitHub repository, as it lacks text fields 
### due to the copyright issue. 

### Loading a package and the data.
library(tidyverse)

### Setting the path to the root folder.
root_path <- "~/Research_data/Hybrida/tumor_analysis/"
### Change the above according to your root folder location.
### If you use Windows, change all occurrences of "/" in file paths to "\\".

### Loading the input file.
load(paste0(root_path, "R_results/tumor_types_F"))

colnames(tumor_types_F)

### Selecting required columns.
accuracy_test_TM <- tumor_types_F %>% 
  select(ID, text_all, researched_tumor_type, tumor_organ_type, metastasis_organ_type, corpus_F) 

### Setting random seed for the first subset of documents.
set.seed(113)

### Shuffling the row order.
nrow1 <- sample(nrow(accuracy_test_TM))

at_TM1 <- accuracy_test_TM[nrow1, ]

### Below, the comment in each of the three lines shows:
### 1) A comment 
### 2) manual classification of tumor_organ_type, 
### 3) algorithm-based classification of tumor_organ_type  
### 4) Comparison
at_TM1[1, "text_all"]
at_TM1[1, 3:4]
## 
## mammary
## mammary
## OK

at_TM1[2, "text_all"]
at_TM1[2, 3:4]
## 
## unidentified
## unidentified
## OK

at_TM1[3, "text_all"]
at_TM1[3, 3:4]
## 
## unidentified
## unidentified
## OK

at_TM1[4, "text_all"]
at_TM1[4, 3:4]
## 
## liver
## liver
## No

at_TM1[5, "text_all"]
at_TM1[5, 3:4]
## The paper describe gatrointestinal and pacreatic cancer organoids, but study lung cancer.
## lung
## multiple organs
## No

at_TM1[6, "text_all"]
at_TM1[6, 3:4]
## Talking about cholangiocarcinoma and gallbladder carcinoma.
## HPB
## HPB
## OK

at_TM1[7, "text_all"]
at_TM1[7, 3:4]
## 
## esophagus
## esophagus
## OK

at_TM1[8, "text_all"]
at_TM1[8, 3:4]
## 
## unidentified
## unidentified
## OK

at_TM1[9, "text_all"]
at_TM1[9, 3:4]
## 
## mammary
## mammary
## OK

at_TM1[10, "text_all"]
at_TM1[10, 3:4]
## 
## unidentified
## unidentified
## OK

at_TM1[11, "text_all"]
at_TM1[11, 3:4]
## The paper is about cancer with organoid growth pattern.
## unidentified
## neuroendocrine
## No

at_TM1[12, "text_all"]
at_TM1[12, 3:4]
## 
## mammary
## mammary
## OK

at_TM1[13, "text_all"]
at_TM1[13, 3:4]
## The paper is about cancer with organoid pattern.
## unidentified
## unidentified
## OK

at_TM1[14, "text_all"]
at_TM1[14, 3:4]
## 
## large intestine
## large intestine
## OK

at_TM1[15, "text_all"]
at_TM1[15, 3:4]
## Review.
## stomach
## stomach
## OK

at_TM1[16, "text_all"]
at_TM1[16, 3:4]
## 
## salivary gland
## salivary gland
## OK

at_TM1[17, "text_all"]
at_TM1[17, 3:4]
## 
## brain
## brain
## OK

at_TM1[18, "text_all"]
at_TM1[18, 3:4]
## 
## pancreas
## pancreas
## OK

at_TM1[19, "text_all"]
at_TM1[19, 3:4]
##
## large intestine
## large intestine
## OK

at_TM1[20, "text_all"]
at_TM1[20, 3:4]
##
## large intestine
## large intestine
## OK

at_TM1[21, "text_all"]
at_TM1[21, 3:4]
## 
## unidentified
## unidentified
## OK

at_TM1[22, "text_all"]
at_TM1[22, 3:4]
##
## pancreatic duct
## pancreatic duct
## OK

at_TM1[23, "text_all"]
at_TM1[23, 3:4]
## 
## multiple organs
## pancreas
## NO

at_TM1[24, "text_all"]
at_TM1[24, 3:4]
## 
## rectum
## rectum
## OK


## Summary so far;
## 20/24





### Second subset
set.seed(73)

### Shuffling the row order.
nrow2 <- sample(nrow(accuracy_test_TM))

at_TM2 <- accuracy_test_TM[nrow2, ]

at_TM2[1, "text_all"]
at_TM2[1, 3:4]
## 
## bladder
## bladder
## OK

at_TM2[2, "text_all"]
at_TM2[2, 3:4]
## The paper seems to use many different tumor types, including colorectal cancer.
## multiple
## large intestine
## NO

at_TM2[3, "text_all"]
at_TM2[3, 3:4]
## 
## pancreatic duct
## pancreatic duct
## OK

at_TM2[4, "text_all"]
at_TM2[4, 3:4]
## 
## mammary
## mammary
## OK

at_TM2[5, "text_all"]
at_TM2[5, 3:4]
## 
## bone
## bone
## OK

at_TM2[6, "text_all"]
at_TM2[6, 3:4]
## 
## large intestine
## large intestine
## OK

at_TM2[7, "text_all"]
at_TM2[7, 3:4]
## 
## mammary
## mammary
## OK

at_TM2[8, "text_all"]
at_TM2[8, 3:4]
##
## multiple
## large intestine
## No

at_TM2[9, "text_all"]
at_TM2[9, 3:4]
## 
## large intestine
## large intestine
## OK

at_TM2[10, "text_all"]
at_TM2[10, 3:4]
## 
## salivary gland
## salivary gland
## OK

at_TM2[11, "text_all"]
at_TM2[11, 3:4]
##
## neural
## neural
## OK

at_TM2[12, "text_all"]
at_TM2[12, 3:4]
## large intestine
## large intestine
## OK

at_TM2[13, "text_all"]
at_TM2[13, 3:4]
##
## unidentified
## unidentified
## OK

at_TM2[14, "text_all"]
at_TM2[14, 3:4]
## 
## neural
## neural
## OK

at_TM2[15, "text_all"]
at_TM2[15, 3:4]
##
## lung
## lung
## OK

at_TM2[16, "text_all"]
at_TM2[16, 3:4]
##
## unidentified
## unidentified
## OK

at_TM2[17, "text_all"]
at_TM2[17, 3:4]
## 
## large intestine
## large intestine
## OK

at_TM2[18, "text_all"]
at_TM2[18, 3:4]
## 
## prostate
## prostate
## OK

at_TM2[19, "text_all"]
at_TM2[19, 3:4]
## 
## dermal
## dermal
## OK

at_TM2[20, "text_all"]
at_TM2[20, 3:4]
## 
## pancreatic duct
## pancreatic duct
## OK

at_TM2[21, "text_all"]
at_TM2[21, 3:4]
##
## large intestine
## colon
## OK

at_TM2[22, "text_all"]
at_TM2[22, 3:4]
##
## mammary
## mammary
## OK

at_TM2[23, "text_all"]
at_TM2[23, 3:4]
##
## large intestine
## large intestine
## OK

at_TM2[24, "text_all"]
at_TM2[24, 3:4]
##
## large intestine
## large intestine
## OK

##
## 22/24





### The third subset.
set.seed(33)

### Shuffling the row order.
nrow3 <- sample(nrow(accuracy_test_TM))

at_TM3 <- accuracy_test_TM[nrow3, ]

at_TM3[1, "text_all"]
at_TM3[1, 3:4]
## 
## stomach
## stomach
## OK

at_TM3[2, "text_all"]
at_TM3[2, 3:4]
## 
## large intestine
## large intestine
## OK

at_TM3[3, "text_all"]
at_TM3[3, 3:4]
## 
## unidentified
## unidentified
## OK

at_TM3[4, "text_all"]
at_TM3[4, 3:4]
## 
## unidentified
## unidentified
## OK

at_TM3[5, "text_all"]
at_TM3[5, 3:4]
## 
## ovary
## ovary
## OK

at_TM3[6, "text_all"]
at_TM3[6, 3:4]
## 
## large intestine
## large intestine
## OK

at_TM3[7, "text_all"]
at_TM3[7, 3:4]
## 
## unidentified
## unidentified
## OK

at_TM3[8, "text_all"]
at_TM3[8, 3:4]
## 
## mammary
## mammary
## OK

at_TM3[9, "text_all"]
at_TM3[9, 3:4]
## 
## gastrointestinal
## gastrointestinal
## OK

at_TM3[10, "text_all"]
at_TM3[10, 3:4]
## 
## unidentified
## unidentified
## OK

at_TM3[11, "text_all"]
at_TM3[11, 3:4]
## 
## unidentified
## unidentified
## OK

at_TM3[12, "text_all"]
at_TM3[12, 3:4]
## The paper discuss rare cancers, mentioning sarcoma.
## unidentified
## unidentified
## OK

at_TM3[13, "text_all"]
at_TM3[13, 3:4]
## 
## unidentified
## unidentified
## OK

at_TM3[14, "text_all"]
at_TM3[14, 3:4]
## 
## gastrointestinal
## unidentified
## No

at_TM3[15, "text_all"]
at_TM3[15, 3:4]
## 
## dermal
## dermal
## OK

at_TM3[16, "text_all"]
at_TM3[16, 3:4]
## 
## prostate
## prostate
## OK

at_TM3[17, "text_all"]
at_TM3[17, 3:4]
## 
## prostate
## prostate
## OK

at_TM3[18, "text_all"]
at_TM3[18, 3:4]
## 
## intestine
## large intestine
## OK

at_TM3[19, "text_all"]
at_TM3[19, 3:4]
## 
## large intestine
## large intestine
## OK

at_TM3[20, "text_all"]
at_TM3[20, 3:4]
## 
## mammary
## mammary
## OK

at_TM3[21, "text_all"]
at_TM3[21, 3:4]
## The paper is actually about tumor-derived organotypic cell clusters, which the authors claim to be superior to tumor organoids.
## unidentified
## mammary
## No

at_TM3[22, "text_all"]
at_TM3[22, 3:4]
## 
## mammary
## mammary
## OK

at_TM3[23, "text_all"]
at_TM3[23, 3:4]
## 
## pancreas
## pancreas
## OK

at_TM3[24, "text_all"]
at_TM3[24, 3:4]
## 
## unidentified
## unidentified
## OK

## Summary
## 22/24






### Calculating averages
###
### tumor organ type
mean(c(20/24, 22/24, 22/24))
## [1] 0.8888889
sd(c(20/24, 22/24, 22/24))
## [1] 0.04811252





