### This R script file is to identify combinations of primary and metastatic cancers 
### that are modelled as tumor organoids/ToC.

### Loading a package.
library(tidyverse)
### Setting the path to the root folder.
root_path <- "~/Research_data/Hybrida/tumor_analysis/"
### Change the above according to your root folder location.

### Loading required files.
load(paste0(root_path, "R_results/tumor_types_F"))
  
colnames(tumor_types_F)

### From the corpus, only selecting research articles where metastatic tumors were identified.
metastasis_RA <- tumor_types_F %>% 
  filter(type == "Research article") %>% 
  filter(!metastasis_organ_type == "unidentified")

### Checking primary cancers that are modelled.
metastasis_RA %>% count(tumor_organ_type)

colnames(tumor_types_F)

### Adjusting the organ classifications to group primary/metastatic cancers.
metastasis_all_links <- metastasis_RA %>% 
  ### Changing the NA value of primary tumor models to "unidentified".
  mutate(tumor_organ_1 = replace_na(tumor_organ_1, "unidentified")) %>% 
  ### selecting recuired columns.
  select(ID, corpus_F, any_of(c(paste0("tumor_organ_", c(1:9)), paste0("metastasis_organ_", c(1:9))))) %>% 
  ### Converting the primary tumor model types into a longer format.
  pivot_longer(starts_with("tumor_organ_"), values_to = "tumor_organ", values_drop_na = TRUE) %>% 
  select(!name) %>% 
  ### Converting the metastatic tumor model types into a longer format.
  pivot_longer(starts_with("metastasis_organ_"), values_to = "metastasis_organ", values_drop_na = TRUE) %>% 
  select(!name) %>% 
  ### changing column values so that organ substructures are considered as organs they belong to.
  mutate(across(c(tumor_organ, metastasis_organ), ~ ifelse(. == "alveolus", "lung", .))) %>% 
  mutate(across(c(tumor_organ, metastasis_organ), ~ ifelse(. %in% c("blood", "microvascular", "blood vessel"), "vascular", .))) %>% 
  mutate(across(c(tumor_organ, metastasis_organ), ~ ifelse(. %in% c("colon", "rectum"), "large intestine", .))) %>% 
  mutate(across(c(tumor_organ, metastasis_organ), ~ ifelse(. %in% c("lymphatic vessel", "lymph node"), "lymphatic", .))) %>% 
  mutate(across(c(tumor_organ, metastasis_organ), ~ ifelse(. == "pancreatic duct", "pancreas", .))) %>% 
  mutate(across(c(tumor_organ, metastasis_organ), ~ ifelse(. == "bone marrow", "bone", .))) %>% 
  distinct()

### Removing metastatic combinations involving "unidentified" (as primary or metastatic tumor) or 
### "vascular" (only as metastatic tumor) groups, as they do not represent organs.
metastasis_selected_links <- metastasis_all_links %>% 
  filter(!tumor_organ == "unidentified", 
         !metastasis_organ %in% c("vascular", "unidentified")) %>% 
  ### Counting the number of total metastasis papers per corpus_F.
  group_by(corpus_F) %>% 
  mutate(metastasis_total = n_distinct(ID)) %>% 
  ungroup() %>% 
  ### Counting the number of metastasis papers that study each of primary/metastatic combinations.
  group_by(corpus_F, tumor_organ, metastasis_organ) %>% 
  mutate(count = n()) %>% 
  ungroup()

### Summarizing the data frame to show the number of research articles.
metastasis_counts_simplified <- metastasis_selected_links %>% 
  select(!ID) %>% 
  distinct() %>% 
  arrange(corpus_F, - count)
### This data frame was used to discuss how frequently primary/metastatic combinations are studied.