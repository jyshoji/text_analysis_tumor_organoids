### This R script file is for comparing increases in publication counts in tumor organoid research 
### and tumor research as a whole. 

##########
###
### 1. Calculating the ratio of publications using organoids/OoC in tumor research
###
##########

### Loading a package
library(tidyverse)

### Setting the path to the root folder.
root_path <- "~/Research_data/Hybrida/tumor_analysis/"
### Change the above according to your root folder location.
### If you use Windows, change all occurrences of "/" in file paths to "\\".

load(paste0(root_path, "R_results/tumor_types_P"))

### Importing the publication count data of tumor research.
tumor_publication <- read.csv(paste0(root_path, "csv/PubMed_Timeline_Results_by_Year.csv"), skip = 1)

### Adjusting the publication count data.
tumor_publication_count <- tumor_publication %>% 
  # Assigning the corpus name "tumor research" to the data.
  mutate(corpus_F = "tumor research", 
         type = "Research article") %>% 
  # changing column names to be consistent with the tumor organoid/tumor-on-chip corpus.
  rename(year = Year, 
         n = Count) %>% 
  # reordering rows by year
  arrange(year) %>% 
  # changing the column order.
  select(4, 3, 1, 2) %>% 
  rename(tumor_all = n)

### Calculating publication counts of tumor organoid/tumor-on-chip publications in each corpus.
tumor_organoid_ratio <- tumor_types_P %>% 
  # calculating publication counts in each year for each corpus.
  group_by(type, corpus_F) %>% 
  count(year) %>% 
  ungroup() %>% 
  # Combining with the above publication counts for tumor research.
  filter(corpus_F %in% c("tumor_organoid", "ToC")) %>% 
  filter(type == "Research article") %>% 
  mutate(year = as.integer(year)) %>% 
  left_join(., tumor_publication_count[, c(1, 3, 4)], by = c("type", "year")) %>% 
  mutate(percentage = 100 * n / tumor_all)




##########
###
### 2. predicting future publication growth in tumor organoids research and tumor research as a whole.
###
##########

### Showing the numbers of yearly research article publications in tumor organoid and tumor research.
tumor_organoid_modified <- tumor_organoid_ratio %>% 
  filter(corpus_F == "tumor_organoid") %>% 
  filter(year %in% c(2011:2022)) %>% 
  select(year, n) 

tumor_research_modified <- tumor_publication_count %>% 
  filter(year %in% c(2011:2022)) %>% 
  select(year, tumor_all) %>% 
  rename(n = tumor_all)


### Calculating exponential regression models
tumor_organoid_model  <- lm(log(n) ~ year, data = tumor_organoid_modified)

tumor_research_model <- lm(log(n) ~ year, data = tumor_research_modified)

summary(tumor_organoid_model)
summary(tumor_research_model)

### Predicting the yearly publications based on the regression models.
tumor_organoid_extrapolate <- data.frame(year = c(2023:2030), n = exp(predict(tumor_organoid_model, data.frame(year = c(2023:2030))))) %>% 
  rbind(tumor_organoid_modified, .)

tumor_research_extrapolate <- data.frame(year = c(2023:2030), n = exp(predict(tumor_research_model, data.frame(year = c(2023:2030))))) %>% 
  rbind(tumor_research_modified, .)

### Combining the data frames to facilitate comparison.
tumor_organoid_projection <- left_join(tumor_organoid_extrapolate, tumor_research_extrapolate %>% rename(n2 = n), by = "year") %>% 
  mutate(percentage = n*100 / n2)



