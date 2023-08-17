### This R script file is to draw a bar chart comparing tumor organoid models, ToC models, models used in clinical research, 
### global incidence and mortality of tumors. 
###
### The global incidence and mortality of tumors were taken from: 
### Sung et al., 2021
### "Global Cancer Statistics 2020: GLOBOCAN Estimates of Incidence and Mortality Worldwide for 36 Cancers in 185 Countries"
###
### The code uses ./R_results/tumor_types_F as an input.
###
### Produced graphs were saved at:
### ./results/organ_types/

##########
###
### 1. Comparing tumor types in tumor organoid research and global incidence/mortality
###
##########

### Loading a package
library(tidyverse)

### Setting the path to the root folder.
root_path <- "~/Research_data/Hybrida/tumor_analysis/"
### Change the above according to your root folder location.
### If you use Windows, change all occurrences of "/" in file paths to "\\".

### Loading the input files.
load(paste0(root_path, "R_results/tumor_types_F"))
### If you want to reproduce the figures, change the above line to the following two lines.
### load(paste0(root_path, "R_results/tumor_types_P"))
### tumor_types_F <- tumor_types_P

load(paste0(root_path, "R_results/clinical_trials_P"))

edge_all <- read.csv(paste0(root_path, "csv/edge_all.csv"))


### Choosing common tumor types (accounting for > 1% of total tumor occurrence according to Sung et al., 2021) to plot.
common_tumors <- c("bladder", "neural", "cervix", "uterus", "dermal", "esophagus", "kidney", "large intestine", 
                   "liver", "lung", "lymphatic", "mammary", "oral", "ovary", "pancreas", "prostate", "stomach", 
                   "thyroid", "vascular")

### The publication counts and proportions of each type of tumor_group were counted.
### tumor_group grouping is adjusted to adhere to Sung et al.
tumor_counts <- tumor_types_F %>% 
  left_join(., edge_all[, c(2, 4)], by = c("tumor_organ_type" = "to")) %>% 
  filter(type == "Research article") %>% 
  filter(!is.na(tumor_group), 
         !tumor_group == "unidentified") %>% 
  ### "brain" tumors are grouped together with "neural" tumor.
  mutate(tumor_group = ifelse(tumor_group == "brain", "neural", tumor_group)) %>%   
  ### "biliary" tumors (e.g., cholangiocarcinoma) are included in liver tumors.
  mutate(tumor_group = ifelse(tumor_group == "biliary", "liver", tumor_group)) %>% 
  ### Uncommon tumor groups are changed to "other".
  mutate(tumor_group = ifelse(tumor_group %in% common_tumors, tumor_group, "other")) %>% 
  group_by(corpus_F) %>% 
  count(tumor_group) %>% 
  mutate(total_n = sum(n)) %>% 
  mutate(percentage = n / total_n) %>% 
  ungroup() 

sort(unique(tumor_counts$tumor_group))
##  [1] "bladder"         "cervix"         
## [3] "dermal"          "esophagus"      
## [5] "kidney"          "large intestine"
## [7] "liver"           "lung"           
## [9] "lymphatic"       "mammary"        
## [11] "neural"          "oral"           
## [13] "other"           "ovary"          
## [15] "pancreas"        "prostate"       
## [17] "stomach"         "thyroid"        
## [19] "uterus"          "vascular"    


### Percentage of global incidence and mortality (Sung et al., 2021) were made into data frames.
### Note that percentages for dermal, large intestine, and lymphatic were calculated by adding 
### (Nonmelanoma of skin + Melanoma of skin), (Colon + Rectum), and (Non-Hodgkin lymphoma + Hodgkin lymphoma),  
### respectively. 
tumor_incidence <- data.frame(corpus_F = "incidence", 
                              tumor_group = sort(unique(tumor_counts$tumor_group)), 
                              percentage = c(0.03, 0.031, 0.079, 0.031, 0.022, 
                                             0.098, 0.047, 0.114, 0.032, 0.117, 
                                             0.016, 0.020, 0.115, 0.016, 0.026, 
                                             0.073, 0.056, 0.03, 0.022, 0.025))

tumor_mortality <-  data.frame(corpus_F = "mortality", 
                               tumor_group = sort(unique(tumor_counts$tumor_group)), 
                               percentage = c(0.021, 0.034, 0.012, 0.055, 0.018, 
                                              0.092, 0.083, 0.18, 0.028, 0.069, 
                                              0.025, 0.018, 0.137, 0.021, 0.047, 
                                              0.038, 0.077, 0.004, 0.01, 0.031))

### Tumor models in clinical trials are adjusted similarly
clinical_trials <- clinical_trials_P %>% 
  ### selecting required columns
  select(ID, tumor_group) %>% 
  ### Removing unclassified clinical trials.
  filter(!is.na(tumor_group), 
         !tumor_group == "unidentified") %>% 
  distinct() %>% 
  ### Counting the number of occurrences of each document, in order to determine documents studying multiple tumor types.
  group_by(ID) %>% 
  mutate(n_copy = n()) %>% 
  ungroup() %>% 
  ### Including "biliary" in "liver".
  ### Changing writing styles.
  mutate(tumor_group = ifelse(tumor_group == "biliary", "liver", tumor_group)) %>% 
  mutate(tumor_group = ifelse(tumor_group %in% common_tumors, tumor_group, "other")) %>% 
  ### If a document has a copy in another row (i.e., if a document studies more than one tumor type), 
  ### it is considered as studying multiple tumor types and is included in "other".
  mutate(tumor_group = ifelse(n_copy > 1, "other", tumor_group)) %>% 
  distinct() %>% 
  ### Counting the number of clinical trials in each tumor_group.
  count(tumor_group) %>% 
  mutate(total_n = sum(n)) %>% 
  mutate(percentage = n / total_n) %>% 
  ungroup() %>% 
  mutate(corpus_F = "clinical trial") %>% 
  select(corpus_F, tumor_group, n, total_n, percentage)
  

### Tumor types, excluding "other", are reordered by percentage to be later used to order category levels in bar charts.
tumor_categories <- tumor_incidence %>% 
  filter(!tumor_group == "other") %>% 
  arrange(- percentage)

### Combining counts of tumor organoids and ToCs with incidence/mortality data frames.
tumor_summary <- tumor_counts %>% 
  rbind(., clinical_trials) %>% 
  bind_rows(., tumor_incidence) %>% 
  bind_rows(., tumor_mortality)

### Saving as a csv file.
write.csv(tumor_summary, file = paste0(root_path, "results/csv/tumor_comparison.csv"), 
          row.names = FALSE)

### Plotting a bar chart showing the number of documents in tumor_organoid, ToC, and clinical trials.
tumor_publication_counts <- tumor_summary %>% 
  filter(corpus_F %in% c("tumor_organoid", "ToC", "clinical trial")) %>% 
  select(corpus_F, total_n) %>% 
  mutate(x_group = ifelse(corpus_F == "clinical trial", corpus_F, "research article")) %>% 
  mutate(x_label = ifelse(corpus_F == "clinical trial", "Tumor organoids + TOC", 
                          ifelse(corpus_F == "tumor_organoid", "Tumor organoids", corpus_F))) %>% 
  distinct() %>% 
  ggplot(aes(x = factor(x_label, levels = c("Tumor organoids", "ToC", "Tumor organoids + TOC")), y = total_n)) + 
  geom_bar(stat = "identity") + 
  labs(title = "A. The numbers of research articles and clinical trials") + 
  xlab("") + 
  ylab("the number of documents") + 
  facet_grid(cols = vars(factor(x_group, levels = c("research article", "clinical trial"))), scales = "free", space = "free", switch = "x") + 
  theme(strip.placement = "outside", 
        plot.title = element_text(size = 7, face = 2), 
        text = element_text(size = 7))

ggsave(tumor_publication_counts, 
       filename = paste0(root_path, "results/organ_types/tumor_publication_counts.pdf"),  
       width = 85, height = 60, units = "mm")

### Plotting bar charts comparing tumor types studied, incidence, and mortality.
tumor_group_incidence <- tumor_summary %>% 
  ggplot(aes(x = factor(corpus_F, 
                        levels = c("incidence", "mortality", "tumor_organoid", "ToC", "clinical trial")), 
             y = percentage, 
             fill = factor(tumor_group, 
                           levels = c(tumor_categories$tumor_group, "other")))) + 
  geom_bar(stat = "identity", color = "black", linewidth = 0.2) + 
  labs(
    #title = "Global incidence, mortality, researched tumor models, and clinical trials by tumor types", 
    fill = "Tumor types"
    ) + 
  xlab(NULL) + 
  scale_x_discrete(labels = c("incidence" = "Global incidence", "mortality" = "Global mortality", 
                              "tumor_organoid" = "Tumor organoids", "ToC" = "ToC", "clinical trial" = "Clinical trial")) + 
  scale_fill_manual(values = c("red", "orange", "yellow", "green", "blue", 
                               "purple", "deeppink", "coral1", "wheat3", "springgreen3", 
                               "navyblue", "cyan", "hotpink", "brown2", "goldenrod4", 
                               "forestgreen", "deepskyblue", "brown", "tomato1", "grey70")) + 
  theme(text = element_text(size = 10), 
        legend.key.size = unit(5, "mm"))

ggsave(tumor_group_incidence, 
       filename = paste0(root_path, "results/organ_types/tumor_group_incidence.pdf"),  
       width = 178, height = 250, units = "mm")


