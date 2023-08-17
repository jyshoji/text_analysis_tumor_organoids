### This R script file is for identifying research topics that appear in academic publications on 
### tumor organoids or tumor-on-chip.
### The code can not be reproduced using the data stored at the GitHub repository, as it lacks text fields 
### due to the copyright issue. 
###
### Overall, frequently occurring phrases were extracted and checked manually, and key research topics were selected.
###
### The outcomes of the code here were used to make lists of research topics to capture in "./R/research_topics_tumor.R".

### Loading packages
library(tidyverse)
library(tidytext)

### Setting the path to the root folder.
root_path <- "~/Research_data/Hybrida/tumor_analysis/"
### Change the above according to your root folder location.
### If you use Windows, change all occurrences of "/" in file paths to "\\".

### Loading the input file.
load(paste0(root_path, "R_results/tumor_types_F"))

tumor_term_all <- read.csv(paste0(root_path, "csv/tumor_terms.csv")) 

### Making a vector containing the force-united organ names.
pre_2words_w <- c("pancreaticduct", "bileduct", "collectingduct", "breastduct", "biliarytract", "reproductivetract", 
                  "oviduct", "neuraltube", "renaltubule", "proximaltubule", "distaltubule", "salivarygland", "parotidgland", 
                  "submandibulargland", "lacrimalgland" , "venomgland" , "endometrialgland", "gastricgland", "mammarygland", 
                  "sebaceousgland", "sweatgland", "thyroidgland", "smallintestine", "largeintestine", "vascularnetwork", 
                  "corticostriatalnetwork", "cornealbarrier", "bloodbrainbarrier", "placentalbarrier", "dorsalrootganglion", 
                  "bloodvessel", "lymphaticvessel", "tumorvessel", "perivascularniche", "metastaticniche", "tastebud", 
                  "circumvallatepapilla", "spinalcord", "opticcup", "bonemarrow", "synovialjoint", "choroidplexus", 
                  "dentalpulp", "neuromuscularjunction", "neurovascularunit", "lymphnode", "fetalmembrane", "opticvesicle")

### Loading the list of words before organoid/onchip, generated in the above section 1.
pre_inc_pre <- read.csv(paste0(root_path, "csv/pre_words_F.csv")) %>% 
  filter(include %in% c("y", "c"))

### Combining the above list of words with the list of force-united organ names.
pre_inc2_w <- c(pre_inc_pre$pre_word, pre_2words_w)

### Loading stop words.
### "Stop words" here mean very common words such as "in", "a", etc.
data(stop_words)






### Splitting sentences into phrases
phrases <- tumor_types_F %>% 
  select(ID, text_all_lower) %>% 
  ### As "cell/cells" and "organoid/organoids" frequently appear, they are all changed to singular forms.
  mutate(text_all_lower = gsub("\\bcells\\b", "cell", text_all_lower, ignore.case = TRUE)) %>% 
  mutate(text_all_lower = gsub("\\borganoids\\b", "organoid", text_all_lower, ignore.case = TRUE)) %>%   
  ### Splitting sentences into phrases at either full stop, colon, semicolon, opening and closing parentheses.
  unnest_tokens(phrase, text_all_lower, token = stringr::str_split, pattern = "\\.|;|:|\\(|\\)")




##########
###
### Pentagrams
###
##########

### Splitting phrases into pentagrams.
pentagrams <- phrases %>% 
  unnest_tokens(pentagram, phrase, token = "ngrams", n = 5) %>% 
  separate(pentagram, c("word1", "word2", "word3", "word4", "word5"), sep = " ") 

### Discarding pentagrams that include stop words.
pentagrams_filtered <- pentagrams %>% 
  filter(!is.na(word1)) %>% 
  filter(!word1 %in% stop_words$word & 
           !word2 %in% stop_words$word & 
           !word3 %in% stop_words$word & 
           !word4 %in% stop_words$word & 
           !word5 %in% stop_words$word)

### Counting the number of papers that mention the pentagrams.
pentagrams_counted <- pentagrams_filtered %>% 
  unite(pentagram, word1, word2, word3, word4, word5, sep = " ") %>% 
  distinct(.) %>% 
  count(pentagram, sort = TRUE) %>% 
  filter(n > 1)
### Pentagrams that occur in > 3 papers were manually checked.


pentagrams_tumor_counted <- pentagrams %>% 
  filter(!is.na(word1), 
         !word1 %in% stop_words$word, 
         !word2 %in% stop_words$word, 
         !word3 %in% stop_words$word, 
         word4 %in% pre_inc2_w, 
         word5 %in% tumor_term_all$tumor) %>% 
  mutate(word5 = ifelse(word5 == "carcinogenesis", word5, 
                        gsub("s$", "", word5))) %>% 
  distinct() %>% 
  unite(pentagram, word1, word2, word3, word4, word5, sep = " ") %>% 
  count(pentagram, sort = TRUE) %>% 
  filter(n > 1)



##########
###
### Tetragrams
###
##########

### Splitting phrases into tetragrams.
tetragrams <- phrases %>% 
  unnest_tokens(tetragram, phrase, token = "ngrams", n = 4) %>% 
  separate(tetragram, c("word1", "word2", "word3", "word4"), sep = " ") 

### Discarding tetragrams that include stop words
tetragrams_filtered <- tetragrams %>% 
  filter(!is.na(word1)) %>% 
  filter(!word1 %in% stop_words$word & 
           !word2 %in% stop_words$word & 
           !word3 %in% stop_words$word & 
           !word4 %in% stop_words$word)

### Counting the number of papers that mention the tetragrams.
tetragrams_counted <- tetragrams_filtered %>% 
  unite(tetragram, word1, word2, word3, word4, sep = " ") %>% 
  distinct(.) %>% 
  count(tetragram, sort = TRUE) %>% 
  filter(n > 1)
### Tetragrams that occur in > 4 papers were manually checked.


tetragrams_tumor_counted <- tetragrams %>% 
  filter(!is.na(word1), 
         !word1 %in% stop_words$word, 
         !word2 %in% stop_words$word, 
         word3 %in% pre_inc2_w, 
         word4 %in% tumor_term_all$tumor) %>% 
  mutate(word4 = ifelse(word4 == "carcinogenesis", word4, 
                        gsub("s$", "", word4))) %>% 
  distinct() %>% 
  unite(tetragram, word1, word2, word3, word4, sep = " ") %>% 
  count(tetragram, sort = TRUE) %>% 
  filter(n > 3)


##########
###
### trigrams
###
##########

### Splitting phrases into trigrams.
trigrams <- phrases %>% 
  unnest_tokens(trigram, phrase, token = "ngrams", n = 3) %>% 
  separate(trigram, c("word1", "word2", "word3"), sep = " ") 

### Discarding trigrams that include stop words 
trigrams_filtered <- trigrams %>% 
  filter(!is.na(word1)) %>% 
  filter(!word1 %in% stop_words$word & 
           !word2 %in% stop_words$word & 
           !word3 %in% stop_words$word)

### Counting the number of papers that mention the trigrams.
trigrams_counted <- trigrams_filtered %>% 
  unite(trigram, word1, word2, word3, sep = " ") %>% 
  distinct(.) %>% 
  count(trigram, sort = TRUE) %>% 
  filter(n > 1)
### Trigrams that occur in > 9 papers were manually checked.

trigrams_tumor_counted <- trigrams %>% 
  filter(!is.na(word1), 
         !word1 %in% stop_words$word, 
         word2 %in% pre_inc2_w, 
         word3 %in% tumor_term_all$tumor) %>% 
  mutate(word3 = ifelse(word3 == "carcinogenesis", word3, 
                        gsub("s$", "", word3))) %>% 
  distinct() %>% 
  unite(trigram, word1, word2, word3, sep = " ") %>% 
  count(trigram, sort = TRUE) %>% 
  filter(n > 5)




##########
###
### bigrams
###
##########

### Splitting phrases into bigrams.
bigrams <- phrases %>% 
  unnest_tokens(bigram, phrase, token = "ngrams", n = 2) %>% 
  separate(bigram, c("word1", "word2"), sep = " ") 

### Discarding bigrams that include stop words 
bigrams_filtered <- bigrams %>% 
  filter(!is.na(word1)) %>% 
  filter(!word1 %in% stop_words$word & 
           !word2 %in% stop_words$word)

### Counting the number of papers that mention the bigrams.
bigrams_counted <- bigrams_filtered %>% 
  unite(bigram, word1, word2, sep = " ") %>% 
  distinct(.) %>% 
  count(bigram, sort = TRUE) %>% 
  filter(n > 1)
### Bigrams that occur in > 24 papers were manually checked.

bigrams_tumor_counted <- bigrams %>% 
  filter(!is.na(word1), 
         word1 %in% pre_inc2_w, 
         word2 %in% tumor_term_all$tumor) %>% 
  mutate(word2 = ifelse(word2 == "carcinogenesis", word2, 
                        gsub("s$", "", word2))) %>% 
  distinct() %>% 
  unite(bigram, word1, word2, sep = " ") %>% 
  count(bigram, sort = TRUE) %>% 
  filter(n > 3)



##########
###
### words
###
##########

### Splitting phrases into words.
words <- phrases %>% 
  unnest_tokens(word, phrase, token = "words") 

### Discarding words 
words_filtered <- words %>% 
  filter(!is.na(word)) %>% 
  filter(!word %in% stop_words$word) 

### Counting the number of papers that mention the words.
words_counted <- words_filtered %>% 
  mutate(word = gsub("s$", "", word)) %>% 
  distinct(.) %>% 
  count(word, sort = TRUE) %>% 
  filter(n > 1)
### Words that occur in > 99 papers were manually checked.




##########
###
### Only looking at the keyword field
###
##########

keywords <- tumor_types_F %>% 
  select(ID, keywords) %>% 
  mutate(keywords = gsub("\\bcells\\b", "cell", keywords, ignore.case = TRUE)) %>% 
  mutate(keywords = gsub("\\borganoids\\b", "organoid", keywords, ignore.case = TRUE)) %>%   
  unnest_tokens(phrase, keywords, token = stringr::str_split, pattern = "\\.|,|;|:|\\(|\\)|\\band\\b") %>% 
  filter(!is.na(phrase)) %>% 
  mutate(phrase = gsub("^ +| +$", "", phrase)) %>% 
  distinct(.) %>% 
  count(phrase, sort = TRUE) %>% 
  filter(n > 1)
### Keywords that occur in > 4 papers were manually checked.








##########
###
###
###
##########

capital_words <- str_extract_all(tumor_types_F$text_all, "\\b[A-Z]{2,}s?\\b")

t2 <- sapply(capital_words, function(x) gsub("s$", "", x))

capital_words_w <- unique(gsub("s", "", capital_words)) %>% 
  sapply(., function(x) paste0("\\b", x, "\\b"))


capital_words_unique <- sapply(capital_words, function(x) gsub("s$", "", x)) %>% 
  sapply(., unique)

capital_words_counts <- data.frame(table(unlist(capital_words_unique))) %>% 
  arrange(- Freq) %>% 
  filter(Freq > 2)

PD <- tumor_types_F %>% 
  filter(grepl("\\bGEMMs?\\b", text_all))

PD[1:10, "text_all"]


t1 <- tumor_types_F %>% 
  filter(grepl("cancer[- ]associated fibroblast", text_all_lower))
## 73 academic publications. (CAF)

t5 <- tumor_types_F %>% 
  filter(grepl("intra[- ]?tumor heterogeneity", text_all_lower))
## 31

## tissue engineering
## immunotherapy
## chemotherapy (chemotherapeutic)
## chemoresistance (chemo-therapy resistance)
## radiotherapy
## targeted therapy
## adoptive cell therapy
## combination therapy
## personalized medicine (personalized therapy, personalized treatment)
## precision medicine (precision oncology)
## microfluidics
## patient-derived organoid
## patient-derived tumor organoid
## patient-derived xenograft
## patient derived tumor xenograft
## drug screening
## drug testing
## drug delivery (drug transport)
## drug discovery
## drug development
## drug sensitivity
## drug efficacy
## tumor microenvironment (cancer microenvironment, tumor immune microenvironment)
## metastasis
## intra-tumor heterogeneity
## tumor heterogeneity
## cancer-associated fibroblast
## Cancer stem cell　(cancer stem-like cell)
## circulating tumor cell
## preclinical models
## bioprinting
## drug resistance
## angiogenesis
## crisprs
## nanoparticles
## apoptosis
## autophagy
## biobank
## hypoxia
## epithelial-mesenchymal transition (epithelial-to-mesenchymal transition)
## invasion
## epigenetics
## exosome
## extracellular vesicles
## high throughput screening
## high-content screening
## machine learning (deep learning)
## salmonella
## helicobacter pylori
## stem cell niche
## mechanobiology
## oncolytic virus
## reactive oxygen species (24)
## single cell rna sequencing (single cell rna seq)
## gene set enrichment analysis
## locally advanced rectal cancer
## genetically engineered mouse models (40)
## engineered mouse models (40)
## cell cycle arrest (27)
## cancer genome atlas (23)
## immune checkpoint blockade/inhibitor
## tumor initiating cell (18)
## tumor stroma interactions (16)
## patient specific
## optical metabolic imaging (14)
## tumor infiltrating lymphocytes (13)


##########
###
### Listing terms to identify.
###
##########

tumor_RA <- list(
  squamous_cell_carcinoma = c("squamous cell carcinomas?"), 
  head_and_neck_squamous_cell_carcinoma = c("head and neck squamous cell carcinomas?"), 
  oral_squamous_cell_carcinoma = c("oral squamous cell carcinomas?"), 
  esophageal_squamous_cell_carcinoma = c("esophageal squamous cell carcinomas?"), 
  
  nonsmall_cell_lung_cancer = c("nonsmall[- ]cell lung cancers?"), 
  
  er_positive_breast_cancer = c("estrogen receptor positive breast cancers?", "er\\+ breast cancers?"), 
  triple_negative_breast_cancer = c("triple[- ]negative breast cancers?"), 
  
  diffuse_type_gastric_cancer = c("diffuse[- ]type gastric cancers?"), 
  locally_advanced_rectal_cancer = c("locally advanced rectal cancers?"), 
  
  high_grade_serous_ovarian_cancer = c("high[- ]grade serous ovarian carcinomas?", "high[- ]grade serous ovarian cancers?"), 
  
  castration_resistant_prostate_cancer = c("castration[- ]resistant prostate cancers?", "castrate[- ]resistant prostate cancers?"), 
  metastatic_castration_resistant_prostate_cancer = c("metastatic castration[- ]resistant prostate cancers?", 
                                                      "metastatic castrate[- ]resistant prostate cancers?"), 
  bone_metastatic_prostate_cancer = c("bone metastatic prostate cancers?"), 
  neuroendocrine_prostate_cancer = c("neuroendocrine prostate cancers?"), 
  
  renal_cell_carcinoma = c("renal cell carcinomas?"), 
  clear_cell_renal_cell_carcinoma = c("clear cell renal cell carcinomas?"), 
  muscle_invasive_bladder_cancer = c("muscle[- ]invasive bladder cancers?"), 
  nonmuscle_invasive_bladder_cancer = c("nonmuscle[- ]invasive bladder cancers?"), 
  
  signet_ring_cell_carcinoma = c("signet[- ]ring cell carcinomas?"), 
  small_cell_carcinoma = c("small[- ]cell carcinomas?")
)  %>% 
  lapply(., function(x) paste0("\\b", x, "\\b", collapse = "|"))


pre_TF_tumor <- as.data.frame(sapply(research_topics_w, function(x) grepl(x, tolower(all_corpus$text_all_lower)))) %>% 
  ### "immune cells" is also captured in "key" sentences only, to see if it can mostly capture immune cells as components of organoids.
  mutate(immune_cells_key = grepl("immune cells?", tolower(all_corpus$text_all_key))) %>% 
  rename_with(., ~ gsub("^", "TF_", .))








##########
###
### Identifying words ending with "-oma" to consider as tumor.
###
##########

### Making a vector containing potential tumor-related words.
tumor_words <- c("cancers?", "tumou?rs?", "[a-z]*omas?", "neoplasms?", "neoplastic", 
                 "metastas[ei]s", "metastatic", "tumorigenesis", "carcinogenesis", 
                 "oncogenesis", "oncology", "carcinoids?")

### Capturing the words in the above vector
tumor_detected <- unique(unlist(str_extract_all(tolower(tumor_types_F$text_all_lower), paste0("\\b", tumor_words, "\\b", collapse = "|"))))

### Making a data frame containing the captured words.
tumor_detected_df <- data.frame(tumor = tumor_detected, include = "")

### Saving the data frame.
write.csv(tumor_detected_df, file = paste0(root_path, "csv/temps/tumor_detected_df.csv"), row.names = FALSE)

### The file was manually checked so that words to be considered as tumor have "y" in the "include" column.
### The file was saved as "./csv/tumor_detected_F.csv".