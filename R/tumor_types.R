### This R script file is for identifying organ types modelled as tumor organoid or 
### tumor-organ-on-chip (ToC) in academic publications.
###
### Although organ types of the models were already determined, they included both tumor and non-tumor models.
### Here, the code aims to identify organs modelled as their tumorous representations.
### To this end, researched tumor types in academic publications are determined, and organ models matching the 
### researched tumor types are considered as tumor models.

### The file uses "/final_analysis/R_results/organ_types_F" as an input.
###
### The file includes following steps: 
### 1. Uniting two-word organ names.
### 2. Extracting key phrases where organ names will be identified in order to determine researched tumor types.
### 3. Extracting key phrases for metastasis
### 4. Listing words to capture.
### 5. Identifying organ types of researched tumors.
### 6. Identifying tumor models of organs.
### 7. Making the tumor organ classification.
###
### The outcome of the codes are saved as:
### "/tumor_analysis/R_results/tumor_types_F"
### which were used for many of following analysis.


### Loading packages
library(tidyverse)
library(tidytext)

### Setting the path to the root folder.
root_path <- "~/Research_data/Hybrida/tumor_analysis/"
### Change the above according to your root folder location.
### If you use Windows, change all occurrences of "/" in file paths to "\\".

### Loading the input file.
load("~/Research_data/Hybrida/final_analysis/R_results/organ_types_F")
### If you want to reproduce the analysis using the open-access publications that we provide in the GitHub repository, 
### change the above line to; 
### load(paste0(root_path, "R_results/organ_types_CC"))
###
### If you are performing full reproduction of the analysis, set the file path of your organ_types_F file.

### Loading required accessory files.
###
### The list of organ names.
pre_inc_pre <- read.csv(paste0(root_path, "csv/pre_words_F.csv")) %>% 
  filter(include %in% c("y", "c"))
### The list of tumor-related terms, with corresponding organ names when applicable.
tumor_term_all <- read.csv(paste0(root_path, "csv/tumor_terms.csv")) 
### The list of organ names showing categories they belong to.
edge_all <- read.csv(paste0(root_path, "csv/edge_all.csv"))

colnames(organ_types_F)




##########
###
### 1. Uniting two-word organ names.
###
##########

### Uniting two-word organ names into single words.
### Differently from the code in our previous analysis on non-tumor models, the code here does not distinguish titles from 
### keywords + abstract.
### Also, "are" separating keywords were changed to ",", so that co-occurrences of organ names and tumor-related terms 
### are looked for within each keyword rather than across the keyword field.
organ_types_T <- organ_types_F %>% 
  ### Selecting academic publications on tumor organoids and ToC.
  filter(corpus_F %in% c("tumor_organoid", "ToC")) %>% 
  ### "and" in the keyword field is replaced with ",". 
  ### Also, changing the keyword field into lower cases.
  mutate(keywords_lower = gsub(" and ", ", ", tolower(keywords_mod))) %>% 
  ### Making the abstract field in lower cases.
  mutate(abstract_lower = tolower(abstract_mod)) %>% 
  ### combining the text fields in lower cases.
  mutate(text_all_lower = paste(title_lower, keywords_lower, abstract_lower, sep = "; ")) %>% 
  ### Uniting two-word organ names.
  ### These lines are identical to those used in final_analysis/R/organ_types.R.
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "\\)?[- ]organoid", " organoid"))) %>% 
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "pancreatic[- ]ductal|pancreatic[- ]ducts?", "pancreaticduct"))) %>% 
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "liver[- ]ductal|liver[- ]ducts?|bile[- ]ductal|bile[- ]ductular|bile[- ]ducts?|hepatobiliary[- ]ductal|hepatobiliary[- ]ducts?", "bileduct"))) %>%  
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "collecting[- ]ductal|collecting[- ]ducts?", "collectingduct"))) %>% 
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "breast[- ]ductal|breast[- ]ducts?", "breastduct"))) %>%  
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "biliary[- ]tracts?", "biliarytract"))) %>% 
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "reproductive[- ]tracts?", "reproductivetract"))) %>%  
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "fallopian[- ]tubes?|uterine[- ]tubes?", "oviduct"))) %>% 
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "neural[- ]tubes?", "neuraltube"))) %>% 
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "renal[- ]tubules?|renal[- ]tubular|kidney[- ]tubules?|kidney[- ]tubular", "renaltubule"))) %>% 
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "proximal[- ]tubules?", "proximaltubule"))) %>% 
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "distal[- ]tubules?", "distaltubule"))) %>% 
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "salivary[- ]glands?|salivary[- ]glandular", "salivarygland"))) %>% 
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "parotid[- ]glands?|parotid[- ]glandular", "parotidgland"))) %>% 
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "submandibular[- ]glands?|submandibular[- ]glandular", "submandibulargland"))) %>% 
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "lacrimal[- ]glands?|lacrimal[- ]glandular", "lacrimalgland"))) %>%  
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "venom[- ]glands?|venom[- ]glandular", "venomgland"))) %>%  
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "endometrial[- ]glands?|endometrial[- ]glandular", "endometrialgland"))) %>% 
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "gastric[- ]glands?|gastric[- ]glandular", "gastricgland"))) %>% 
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "mammary[- ]glands?|mammary[- ]glandular", "mammarygland"))) %>% 
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "sebaceous[- ]glands?|sebaceous[- ]glandular", "sebaceousgland"))) %>% 
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "sweat[- ]glands?|sweat[- ]glandular|eccrine[- ]glands?|eccrine[- ]glandular", "sweatgland"))) %>%   
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "thyroid[- ]glands?|thyroid[- ]glandular", "thyroidgland"))) %>%    
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "small[- ]intestinal|small[- ]intestines?", "smallintestine"))) %>% 
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "large[- ]intestinal|large[- ]intestines?", "largeintestine"))) %>% 
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "vascular[- ]networks?", "vascularnetwork"))) %>% 
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "corticostriatal[- ]networks?", "corticostriatalnetwork"))) %>% 
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "corneal[- ]barriers?", "cornealbarrier"))) %>%  
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "blood[- ]brain[- ]barriers?", "bloodbrainbarrier"))) %>% 
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "blood[- ]retinal?[- ]barriers?", "bloodretinalbarrier"))) %>% 
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "blood[- ]air[- ]barriers?|air[- ]blood[- ]barriers?|alveolar[- ]capillary barriers?", "bloodairbarrier"))) %>% 
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "placental[- ]barriers?", "placentalbarrier"))) %>%  
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "basal ganglion|basal ganglia", "basalganglion"))) %>%    
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "dorsal root ganglion|dorsal root ganglia|sensory ganglion|sensory ganglia", "dorsalrootganglion"))) %>%    
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "blood[- ]vessels?", "bloodvessel"))) %>%   
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "lymph[- ]vessels?|lymphatic[- ]vessels?|lymphatic[- ]vasculatures?", "lymphaticvessel"))) %>%     
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "tumou?r[- ]vessels?", "tumorvessel"))) %>% 
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "perivascular[- ]niches?", "perivascularniche"))) %>%  
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "metastatic[- ]niches?", "metastaticniche"))) %>% 
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "taste[- ]buds?", "tastebud"))) %>% 
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "circumvallate[- ]papillae?", "circumvallatepapilla"))) %>% 
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "spinal[- ]cords?", "spinalcord"))) %>% 
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "optic[- ]cups?|retinal?[- ]cup", "opticcup"))) %>% 
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "bone[- ]marrows?", "bonemarrow"))) %>% 
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "synovial[- ]joints?", "synovialjoint"))) %>% 
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "gall[- ]bladder", "gallbladder"))) %>%  
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "choroid[- ]plexus", "choroidplexus"))) %>%    
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "dental pulps?", "dentalpulp"))) %>%  
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "neuromuscular[- ]junctions?", "neuromuscularjunction"))) %>% 
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "neurovascular[- ]units?", "neurovascularunit"))) %>%   
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "lymph[- ]nodes?|lymph[- ]glands?|lymph[- ]glandular", "lymphnode"))) %>%    
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "fetal[- ]membranes?", "fetalmembrane"))) %>%  
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "optic[- ]vesicles?", "opticvesicle"))) %>%   
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "enteroid", "smallintestine organoid"))) %>% 
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "colonoid", "colon organoid"))) %>% 
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "embryoid", "embryonic organoid"))) %>% 
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "cerebroid", "cerebral organoid"))) %>% 
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "cardioid", "heart organoid"))) %>% 
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "tumou?roid", "tumor organoid"))) %>% 
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "organoids\\b", "organoid"))) %>% 
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "assembloids\\b", "assembloid")))

### Making a vector containing the force-united organ names.
### The line is identical to that used in final_analysis/R/organ_types.R.
pre_2words_w <- c("pancreaticduct", "bileduct", "collectingduct", "breastduct", "biliarytract", "reproductivetract", 
                  "oviduct", "neuraltube", "renaltubule", "proximaltubule", "distaltubule", "salivarygland", "parotidgland", 
                  "submandibulargland", "lacrimalgland" , "venomgland" , "endometrialgland", "gastricgland", "mammarygland", 
                  "sebaceousgland", "sweatgland", "thyroidgland", "smallintestine", "largeintestine", "vascularnetwork", 
                  "corticostriatalnetwork", "cornealbarrier", "bloodbrainbarrier", "placentalbarrier", "dorsalrootganglion", 
                  "bloodvessel", "lymphaticvessel", "tumorvessel", "perivascularniche", "metastaticniche", "tastebud", 
                  "circumvallatepapilla", "spinalcord", "opticcup", "bonemarrow", "synovialjoint", "choroidplexus", 
                  "dentalpulp", "neuromuscularjunction", "neurovascularunit", "lymphnode", "fetalmembrane", "opticvesicle", 
                  "bloodairbarrier")


### Combining the above list of organ names with the list of force-united organ names.
pre_inc2_w <- c(pre_inc_pre$pre_word, pre_2words_w)
### This lists all the words that will be considered as organ names.





##########
###
### 2. Extracting key phrases where organ names will be identified in order to determine researched tumor types.
###
##########

### Below, from the combined text field, following sentences will be extracted.
### XXX: any one word; AAA: any one word in the pre_inc2_w; TTT: tumor term in the tumor_term_all
### 1. (start of a sentence) AAA TTT
### 2. XXX AAA TTT 
### 3. XXX and AAA TTT 
### 4. XXX or AAA TTT 
### 5. TTT of XXX XXX
### 6. TTT of XXX
### 7. cancerous XXX

### Separating the character strings in the combined text field into phrases.
phrases <- organ_types_T %>% 
  select(ID, text_all_lower) %>% 
  filter(!is.na(text_all_lower)) %>% 
  unnest_tokens(phrase, text_all_lower, token = stringr::str_split, pattern = "\\.|;|:|,")

### Extracting tetragrams from the phrases.
tetragrams_all <- phrases %>% 
  unnest_tokens(tetragram, phrase, token = "ngrams", n = 4) %>% 
  separate(tetragram, c("word1", "word2", "word3", "word4"), sep = " ") %>% 
  filter(!is.na(word1))

### Extracting tetragrams corresponding to the above types 3 and 4.
tetragrams_1 <- tetragrams_all %>% 
  ### selecting tetragrams ending with a tumor-related term, with a preceding word being included in the organ name list, 
  ### with the further preceding word being either "and" or "or".
  filter(word4 %in% tumor_term_all$tumor, 
         word3 %in% pre_inc2_w, 
         word2 %in% c("and", "or")) %>% 
  ### Converting plural forms of word4 into singular forms.
  mutate(word4 = ifelse(word4 == "carcinogenesis", word4, gsub("s$", "", word4))) %>% 
  ### Making a column to store the tumor-related term detected here.
  mutate(tumor_term = word4) %>% 
  distinct()

### Extracting tetragrams corresponding to the above type 5.
tetragrams_2 <- tetragrams_all %>% 
  filter(word1 %in% tumor_term_all$tumor, 
         word2 == "of") %>% 
  mutate(word1 = ifelse(word1 == "carcinogenesis", word1, gsub("s$", "", word1))) %>% 
  mutate(tumor_term = word1) %>% 
  distinct() 

### Extracting all trigrams
trigrams_all <- phrases %>% 
  unnest_tokens(trigram, phrase, token = "ngrams", n = 3) 

### Extracting trigrams corresponding to the above type 2.
trigrams_1 <- trigrams_all %>% 
  separate(trigram, c("word2", "word3", "word4"), sep = " ") %>% 
  filter(!is.na(word2)) %>% 
  filter(word4 %in% tumor_term_all$tumor, 
         word3 %in% pre_inc2_w) %>% 
  mutate(word4 = ifelse(word4 == "carcinogenesis", word4, gsub("s$", "", word4))) %>% 
  mutate(tumor_term = word4) %>% 
  distinct()

### Extracting trigrams corresponding to the above type 6.
trigrams_2 <- trigrams_all %>% 
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>% 
  filter(!is.na(word1)) %>% 
  filter(word1 %in% tumor_term_all$tumor, 
         word2 == "of") %>% 
  mutate(word1 = ifelse(word1 == "carcinogenesis", word1, gsub("s$", "", word1))) %>% 
  mutate(tumor_term = word1) %>% 
  distinct()

### Combining tetragrams_1 and trigrams_1.
tetra_trigrams_1 <- bind_rows(tetragrams_1, trigrams_1) %>% 
  arrange(ID, word1) %>% 
  ### Removing duplicate rows.
  filter(!(is.na(word1) & duplicated(.[, c(1, 3:6)]))) %>% 
  mutate(word1 = replace_na(word1, ""))

### combining tetragrams_2 and trigrams_2, and combining with the above tetra_trigrams_1.
tetra_trigrams_combined <- bind_rows(tetragrams_2, trigrams_2) %>% 
  arrange(ID, word4) %>% 
  filter(!(is.na(word4) & duplicated(.[, c(1:4, 6)]))) %>% 
  mutate(word4 = replace_na(word4, "")) %>% 
  ### adding the above data object of combined tetragrams_1 and trigrams_1.
  rbind(., tetra_trigrams_1) %>% 
  arrange(ID, word1)

### Extracting bigrams.
bigrams_all <- phrases %>% 
  unnest_tokens(bigram, phrase, token = "ngrams", n = 2) %>% 
  separate(bigram, c("word3", "word4"), sep = " ") %>% 
  filter(!is.na(word3))

### Extracting the bigrams corresponding to the above type 1.
bigrams_1 <- bigrams_all %>% 
  filter(word4 %in% tumor_term_all$tumor) %>% 
  mutate(word4 = ifelse(word4 == "carcinogenesis", word4, gsub("s$", "", word4))) %>% 
  mutate(tumor_term = word4) %>% 
  distinct()

### Extracting the bigrams corresponding to the above type 6, and combining them with the above bigrams.
bigrams_combined <- bigrams_all %>% 
  filter(word3 == "cancerous") %>% 
  mutate(tumor_term = "cancerous") %>% 
  distinct() %>% 
  ### combining with the above bigrams_1
  rbind(., bigrams_1) %>% 
  arrange(ID, word3)

### From the list of tumor-related terms, only selecting terms that are associated with 
### specific organs (!include == "ns").
tumor_term_nns <- tumor_term_all %>% 
  filter(!include == "ns")

### Identifying the selected tumor-related terms in the combined text field.
words <- phrases %>% 
  unnest_tokens(tumor_term, phrase, token = "words") %>% 
  filter(tumor_term %in% tumor_term_nns$tumor) %>% 
  mutate(tumor_term = ifelse(tumor_term == "carcinogenesis", tumor_term, gsub("s$", "", tumor_term))) %>% 
  distinct()

### Combining the identified tumor-related terms and the bigrams, removing duplicate entries.
mono_bigrams <- bind_rows(bigrams_combined, words) %>% 
  arrange(ID, word3) %>% 
  filter(!(is.na(word3) & is.na(word4) & duplicated(.[, c(1, 4)]))) %>% 
  mutate(across(c("word3", "word4"), ~ replace_na(., "")))

### Combining the all n-grams
all_grams <- bind_rows(tetra_trigrams_combined, mono_bigrams) %>% 
  arrange(ID, word2) %>% 
  filter(!(is.na(word2) & duplicated(.[, c(1, 4:6)]))) %>% 
  mutate(across(c(word1, word2), ~ replace_na(., ""))) %>% 
  ### Adding the "include" column from the tumor_term_all which shows organs related to tumor-related terms.
  ### Later, when the organ types of researched tumor can not be determined, this column will be used to 
  ### determine the organ type.
  left_join(., tumor_term_all, by = c("tumor_term" = "tumor")) %>% 
  ### Only keeping values in the include column that correspond to organ names.
  mutate(include = ifelse(is.na(include) | include == "ns", "", include)) %>% 
  ### Uniting the words into phrases.
  unite(tetragram, word1, word2, word3, word4, sep = " ") %>% 
  ### White space at the beginning of the strings is removed.
  mutate(tetragram = gsub("^ *", "", tetragram)) %>% 
  group_by(ID) %>% 
  ### Combining all extracted phrases into a single column (pw_sentence).
  ### Also combining tumor-related terms, and "include" values.
  ### This is to convert the data frame into one row-one publication format.
  mutate(pw_sentence = paste(tetragram, collapse = ";"), 
         tumor_term_combined = paste(unique(tumor_term), collapse = ";"), 
         organ_from_tumor = paste(unique(include), collapse = ";")) %>% 
  ungroup() %>% 
  select(!c(tetragram, tumor_term, include)) %>% 
  distinct() %>% 
  ### Removing unnecessary separators ";" that do not separate non-blank values.
  mutate(organ_from_tumor = gsub(";(?![a-z])", "", organ_from_tumor, perl = TRUE)) %>% 
  mutate(organ_from_tumor = gsub("(?<![a-z]);", "", organ_from_tumor, perl = TRUE)) %>% 
  distinct()
  
colnames(all_grams)

### Adding the resulting data frame to the corpus.
pw_tumor_corpus <- organ_types_T %>% 
  left_join(., all_grams, by = "ID") %>% 
  mutate(across(c("pw_sentence", "tumor_term_combined", "organ_from_tumor"), 
                ~ replace_na(., "")))

colnames(pw_tumor_corpus)





##########
###
### 3. Extracting key phrases for metastasis
###
##########

### To later determine organs related to metastasis, key phrases related to metastasis are extracted.
###
### First, metastasis-related terms are identified.
metastasis_w <- unique(unlist(str_extract_all(pw_tumor_corpus$text_all_lower, "\\bmetasta[st][a-z]+\\b"))) %>% 
  sapply(., function(x) paste0("\\b", x, "\\b"))

### Key phrases related to metastasis are extracted.
### This calculation takes a few minutes.
pw_metastasis_corpus <- pw_tumor_corpus %>% 
  mutate(metastasis_title = 
           ifelse(is.na(title_lower), NA, 
                  ifelse(grepl(paste0(metastasis_w, collapse = "|"), title_lower), title_lower, ""))) %>% 
  mutate(metastasis_keywords = 
           ifelse(is.na(keywords_lower), NA, 
                  apply(
                    str_extract_all(keywords_lower, pattern = paste0("[a-z -]*", metastasis_w, "[a-z -]*", collapse = "|"), simplify = TRUE), 
                    1, paste, collapse = "; "))) %>% 
  mutate(metastasis_abstract = 
           ifelse(is.na(abstract_lower), NA, 
                  apply(
                    str_extract_all(abstract_lower, pattern = paste0("[^.]*", metastasis_w, "[^.]*\\.", collapse = "|"), simplify = TRUE), 
                    1, paste, collapse = "; "))) %>% 
  mutate(metastasis_text_all = paste(metastasis_title, metastasis_keywords, metastasis_abstract, sep = "; "))

save(pw_metastasis_corpus, file = paste0(root_path, "R_temps/pw_metastasis_corpus"))






##########
###
### 4. Listing words to capture.
###
### The word lists are essentially same as those in the previous analysis on non-tumor models, except that 
### some unnecessary words were removed.
###
##########
load(paste0(root_path, "R_temps/pw_metastasis_corpus"))

p_ganglion_w <- c("ganglion", "ganglia", "ganglionic(?! eminence)")
p_epidermis_w <- c("epidermi[cs]", "epidermal(?! growth)", "epidermi[ds]es", "keratinocytes?")
p_hair_w <- c("hairs?(?! cell)")


### Making a nested list of all terms (both full terms and abbreviations) of organ names.
all_terms <- list(
  ### brain
  brain_w = c("brains?", "cyborg", "ovb"), 
  # abbreviation
  forebrain_w = c("forebrains?"), 
  cerebrum_w = c("cerebrums?", "cerebral?", "telencephalons?", "telencephalic", "endbrains?"), 
  cortex_w = c("cortex", "cortices", "cortical", "neocortex", "neocortices", "neocortical"), 
  # cortex should be considered only when "brain/cerebrum" appear somewhere in texts
  corticostriatal_network_w = c("corticostriatalnetwork", "cortico[- ]striatal"),   
  hippocampus_w = c("hippocampus", "hippocampi", "hippocampal"), 
  basal_ganglion_w = c("basalganglion", "bg"), 
  # abbreviation
  striatum_w = c("striatum", "striatal?"),   
  diencephalon_w = c("diencephalons?", "diencephalic"), 
  thalamus_w = c("thalamus", "thalamuses", "thalamic?"), 
  hypothalamus_w = c("hypothalamus", "hypothalamic?", "hp"), 
  # abbreviation
  pituitary_w = c("pituitary", "pituitaries", "hypophys[ei]s", "hypophyseal"), 
  brainstem_w = c("brain ?stems?"), 
  midbrain_w = c("midbrains?"), 
  ventral_midbrain_w = c("ventral midbrains?", "vm"), 
  hindbrain_w = c("hindbrains?"), 
  cerebellum_w = c("cerebellums?", "cerebellar?"), 
  
  ### barriers, meninges, and  neuroimmune
  bbb_w = c("bloodbrainbarrier", "h?bbbs?"), 
  choroid_plexus_w = c("choroidplexus", "choroidplexuses", "chps?", "cerebrospinal fluids?", "csfs?"),
  # Capturing only when either "choroidplexus" or "cerebrospinal fluid" appears.
  glymphatic_w = c("glymphatics?"), 
  gliovascular_unit_w = c("gliovascular units?", "gvus?"), 
  meninges_w = c("meninges", "meninx", "meninge?al"), 
  neuroimmune_w = c("neuroimmune"), 
  
  ### nerve
  nerve_w = c("nerves?", "nervous", "neuro", "neuroepithelial", "neurons?", "neuronal", 
              "axons?", "synapses?", "synaptic"), 
  ganglion_w = c("ganglion", "ganglia"), 
  # Another term to capture with perl = TRUE
  autonomic_ganglion_w = c("autonomic ganglion", "autonomic ganglia", "i?ags?"), 
  dorsal_root_ganglion_w = c("drgs?", "dorsalrootganglion", "i?sgs?"), 
  # abbreviation
  neurovascular_w = c("neurovascular", "neurovasculatures?"), 
  neurovascular_unit_w = c("neurovascularunit", "nvus?"), 
  # abbreviation
  neuromuscular_w = c("neuromuscular"), 
  neuromuscular_junction_w = c("neuromuscularjunction"), 
  
  
  ### Spine
  spine_w = c("spines?", "spinal", "intervertebral dis[ck]s?"), 
  spinal_cord_w = c("spinalcord"), 
  
  ### other neural
  enteric_nervous_system_w = c("enteric nervous systems?", "enteric neurons?", "ens"), 
  
  ### ear
  cochlea_w = c("cochlea[rs]?", "hair[- ]cells?"), 
  umbo_w = c("umbos?", "umbones"), 
  
  ### ocular 
  optic_cup_w = c("opticcup"),   
  retina_w = c("retina[els]?"), 
  blood_retinal_barrier_w = c("bloodretinalbarrier", "o?brb"), 
  lacrimal_gland_w = c("lacrimalgland", "lacrimal", "lg"), 
  # abbreviation
  meibomian_gland_w = c("meibomian glands?", "meibocytes?", "mgs?"), 
  # abbreviation
  cornea_w = c("cornea[ls]?", "minicornea[ls]?"), 
  corneal_limbus_w = c("limbal", "limbus", "limbi", "limbuses"), 
  corneal_barrier_w = c("cornealbarrier", "microcornealbarrier"), 
  choroid_w = c("choroids?", "choroidal"), 
  # Make it "FALSE" when choroid plexus is TRUE.
  
  
  ### mouth
  tooth_w = c("tooth", "teeth", "dental"), 
  dental_follicle_w = c("dental follicles?", "dfc"), 
  # abbreviation
  dental_papilla_w = c("dental papillae?", "apical papillae?", "dpmc"), 
  # abbreviation, "dpmc" being dental papillae mesenchymal cell
  enamel_organ_w = c("enamel"), 
  periodontium_w = c("periodontium", "periodontia", "periodontal"),   
  dental_pulp_w = c("dentalpulps?", "dpsc"), 
  # abbreviation
  tongue_w = c("tongues?", "lingual"), 
  taste_bud_w = c("tastebud"), 
  circumvallate_papilla_w = c("circumvallatepapilla"), 
  salivary_gland_w = c("salivarygland", "sgs?"), 
  # abbreviation
  parotid_gland_w = c("parotidgland"), 
  submandibular_gland_w = c("submandibulargland"), 
  gingival_crevice_w = c("gingival crevices?", "gingival sulcus"), 
  
  ### lymphatic
  lymphoid_w = c("lymphoids?"), 
  bone_marrow_w = c("h?bm", "bonemarrow"), 
  # abbreviation
  thymus_w = c("thymus", "thymuses", "thymic?"), 
  spleen_w = c("spleens?"), 
  germinal_center_w = c("germinal centers?", "gc"), 
  # abbreviation
  tonsil_w = c("tonsils?", "tonsillar"), 
  lymph_node_w = c("lymphnode", "ln"), 
  # abbreviation
  sinus_w = c("sinus", "sinuses", "sinusoid"), 
  # Only capture when it occurs with lymphnode.
  lymphatic_vessel_w = c("lymphaticvessel", "lv"), 
  # abbreviation
  
  ### endocrine
  adrenal_w = c("adrenals?"), 
  neuroendocrine_w = c("neuroendocrine"), 
  thyroid_w = c("thyroids?", "tfc"), 
  # abbreviation, tfc being thyroid follicular cell
  parathyroid_w = c("parathyroids?"), 
  thyroid_gland_w = c("thyroidgland"), 
  
  ### breast
  mammary_gland_w = c("mammarygland"), 
  breast_duct_w = c("breastduct"), 
  
  ### cardiovascular
  heart_w = c("hearts?", "cardiac"), 
  myocardium_w = c("myocardium", "myocardial?", "cardiomyocytes?"), 
  valve_w = c("valves?", "valvular"), 
  vascular_w = c("vascular", "vascularnetwork", "vasculature"), 
  blood_w = c("bloods?", "ha?ematopoietic"), 
  blood_vessel_w = c("bloodvessel"), 
  artery_w = c("artery", "arteries", "arterial"), 
  aorta_w = c("aorta[ls]?"), 
  # make it FALSE if TRUE for "aorta-gonad-mesonephros"
  microvascular_w = c("microvascularnetwork", "microvascular", "microvasculatures?", "microvessels?", "angiogenes[ei]s", "capillary", "capillaries"), 
  perivascular_niche_w = c("perivascularniche"), 
  
  ### respiratory
  mucociliary_w = c("mucociliary"), 
  airway_w = c("airways?"), 
  pharynx_w = c("pharynx", "pharynxes", "pharynges", "pharynge?al"), 
  nasopharynx_w = c("nasopharynx", "nasopharynxes", "nasopharynges", "nasopharynge?al"), 
  oropharynx_w = c("oropharynx", "oropharynxes", "oropharynges", "oropharynge?al"), 
  hypopharynx_w = c("hypopharynx", "hypopharynxes", "hypopharynges", "hypopharynge?al"), 
  larynx_w = c("larynx", "larynxes", "larynges", "larynge?al"), 
  trachea_w = c("trachea[els]?", "windpipes?"), 
  tracheosphere_w = c("tracheospheres?"), 
  lung_w = c("lungs?", "pulmonary", "bronchioalveolar"), 
  alveolus_w = c("alveolus", "alveoli", "alveolar", "alveolospheres?", "pneumocytes?"), 
  bronchus_w = c("bronchus", "bronchi", "bronchial"), 
  bronchiole_w = c("bronchioles?", "bronchiolar"), 
  blood_air_barrier = c("bloodairbarrier", "abb"), 
  pleura_w = c("pleura[el]?"), 
  
  ### gastrointestinal
  esophagus_w = c("o?esophagus", "o?esophagi", "o?esophage?al", "o?esophagical"), 
  gastroesophageal_junction_w = c("gastroesophage?al junctions?", "gej"), 
  stomach_w = c("stomachs?", "gastric", "gins"), 
  gastric_antrum_w = c("antrum", "antral?"), 
  gastric_fundus_w = c("fundus", "fundic?"), 
  gastric_corpus_w = c("corpus", "corporal?"), 
  # The above three are only considered when gastric/stomach appears somewhere in the texts.
  gastric_gland_w = c("gastricgland"), 
  abomasum_w = c("abomasums?", "abomasal?"), 
  intestine_w = c("intestines?", "intestinal", "bowels?", "guts?", "enteral", "enteric", "enterocytes?"), 
  small_intestine_w = c("smallintestines?", "h?si"), 
  # abbreviation
  duodenum_w = c("duodenum", "duodenal?"), 
  ileum_w = c("ileum", "ileal?"), 
  jejunum_w = c("jejunum", "jejunal?"), 
  large_intestine_w = c("largeintestines?", "colorectal?", "colorectum"), 
  caecum_w = c("ca?ecums?", "ca?ecal?"), 
  colon_w = c("colons?", "colonic"), 
  appendix_w = c("appendix", "appendixes", "appendices", "appendiceal"), 
  rectum_w = c("rectums?", "rectal?"), 
  
  ### biliary
  pancreas_w = c("pancreas", "pancreases", "pancreatic"), 
  pancreatic_duct_w = c("pancreaticduct"), 
  islet_w = c("islets?"), 
  ### "hepatocellular" is currently not included in "organ_types.R"
  liver_w = c("livers?", "hepatic", "lsc", "hepatocytes?", "h?ihep", "hepatostellate", "hepatic progenitor cells?", "hpcs?", 
              "hepatocellular"), 
  # abbreviation
  lobule_w = c("lobules?", "lobular", "vlsll", "very large-scale liver-lobules?"), 
  # Only capture when liver or hepatic appears.
  sinusoid_w = c("sinusoids?", "sinusoidal"), 
  # Only capture when liver or hepatic appears.
  biliary_w = c("biliary", "hepatobiliary", "biliarytract"), 
  bile_duct_w = c("bileduct", "cholangiocytes?"), 
  ihbd_w = c("intrahepatic bileduct", "ihbd", "intrahepatic ducts?", "ihd"), 
  # abbreviation
  ehbd_w = c("extrahepatic bileduct", "ehbd", "extrahepatic ducts?", "ehd"), 
  # abbreviation
  gallbladder_w = c("gallbladders?", "gb"), 
  # abbreviation
  
  ### reproductive
  female_reproductive_w = c("reproductivetract"), 
  ovary_w = c("ovary", "ovaries", "ovarian"), 
  oviduct_w = c("oviducts?", "oviductal", "salpinx", "salpinges", "salpinxes", "ftec"), 
  uterus_w = c("uterus", "uteri", "uteruses", "uterine", "wombs?"), 
  endometrium_w = c("endometrium", "endometrial?", "adenomyosis"), 
  endometrial_gland_w = c("endometrialgland"), 
  decidua_w = c("decidua[el]?"), 
  cervix_w = c("cervix", "cervices", "cervical", "ectocervix", "ectocervices", "ectocervical", "endocervix", "endocervices", "endocervical"), 
  # ectocervix and endocervix are not distinguished at the moment.
  vagina_w = c("vagina[ls]?"), 
  testes_w = c("test[ei]s", "testicles?", "testicular"), 
  epididymis_w = c("epididymis", "epididymides", "epididymal"), 
  prostate_w = c("prostates?", "prostatic"), 
  
  ### urinary system
  kidney_w = c("kidneys?", "renal"), 
  nephron_w = c("nephrons?", "nephronal"), 
  renal_tubule_w = c("renaltubule"), 
  collecting_duct_w = c("collectingduct", "cd"), 
  # abbreviation
  proximal_tubule_w = c("proximaltubule"), 
  distal_tubule_w = c("distaltubule"), 
  glomerulus_w = c("glomerulus", "glomerular", "glomeruli"), 
  # capture when either kidney, renal, or nephron occurs.
  bladder_w = c("bladders?", "vesical"), 
  urethra_w = c("urethra[els]?"), 
  
  ### dermal
  hair_follicle_w = c("hair follicles?", "hfscs?", "hf"), 
  # abbreviation  
  dermal_papilla_w = c("dermal papillae?", "dp"), 
  # abbreviation
  sebaceous_gland_w = c("sebaceousgland", "sg", "sebaceous"), 
  # abbreviation
  sweat_gland_w = c("sweatgland", "swg"), 
  # abbreviation
  epidermis_w = c("epidermi[cs]", "epidermi[ds]es", "keratinocytes?"), 
  # another term to capture with perl
  
  ### musculoskeletal
  osteochondral_w = c("osteochondral"), 
  bone_w = c("bones?", "osteogenic", "osteo", "osteogenes[ei]s", "osteoclastogenes[ei]s"), 
  cartilage_w = c("cartilages?", "cartilaginous", "chondrogenic", "chondral", "chondrocytes?"), 
  joint_w = c("joints?"), 
  synovial_joint_w = c("synovialjoint", "synovial"), 
  synovium_w = c("synovium", "synovial membranes?", "synovial stratum", "synovial strata", "stratum synoviales?"), 
  anterior_cruciate_ligament_w = c("anterior cruciate ligaments?", "acls?"), 
  artificial_joint_w = c("periprosthetic joints?", "artificial joints?", "prosthes[ei]s", "pji"), 
  muscle_w = c("muscles?", "myotubes?", "muscular", "smcs?"), 
  tendon_w = c("tendons?", "tendinous"), 
  anulus_fibrosus_w = c("ann?ulus fibrosus", "ann?ulus"), 
  
  ### adipose
  white_adipose_tissue_w = c("wats?", "white adipose tissues?"), 
  # abbreviation
  omentum_w = c("omentums?", "omental?"), 
  
  ### Major organs
  neural_w = c("central nervous systems?", "cnss?", "peripheral nervous systems?", "pnss?", "neural"), 
  otic_w = c("ears?", "otic"), 
  ocular_w = c("eyes?", "ocular", "multiocular"), 
  nasal_w = c("noses?", "olfactory", "nasal"), 
  oral_w = c("mouths?", "oral"), 
  lymphatic_w = c("lymphatic", "lymphs?", "lympho"),
  endocrine_w = c("endocrine"), 
  mammary_w = c("breasts?", "mammary", "mammaries", "masc"), 
  # abbreviation
  cardiovascular_w = c("cardiovascular"), 
  respiratory_w = c("respiratory"), 
  gastrointestinal_w = c("gastrointestinal", "gastrointestines?", "git?"), 
  # abbreviation
  hpb_w = c("hepatic, pancreatic,? and biliary", "hpb"), 
  # abbreviation
  reproductive_w = c("reproductive", "gonads?", "gonadal"), 
  # make it FALSE if TRUE for "aorta-gonad-mesonephros"
  urinary_w = c("urinary", "urines?", "usc", "urothelial?", "urothelium"), 
  # abbreviation
  dermal_w = c("skins?", "dermal", "dermis", "cutaneous"), 
  musculoskeletal_w = c("musculoskeletal"), 
  adipose_w = c("adiposes?", "adipocytes?"), 
  venom_gland_w = c("venomgland"), 
  
  ### Tumor
  mpd_w = c("mammary paget’s diseases?"), 
  # abbreviation, mammary
  raird_w = c("refractory diseases?"), 
  # abbreviation, "radioactive iodine (RAI), -refractory disease", cancer that stopped responding to treatment
  cccc_w = c("cervical clear cell carcinomas?"), 
  # cervix, carcinoma
  hcc_w = c("hepatocellular carcinomas?", "hcc","fibrolamellar carcinomas?", "flc"), 
  # liver, carcinoma
  ipmn_w = c("intraductal papillary mucinous neoplasms?"), 
  # pancreatic duct, neoplasm
  lusc_w = c("lung squamous cell carcinomas?"), 
  # lung, carcinoma, epithelium
  oscc_w = c("oral squamous cell carcinomas?"), 
  # mouth, carcinoma
  tcc_w = c("transitional cell carcinomas?"), 
  # urinary system, carcinoma
  tsa_w = c("traditional serrated adenomas") 
  # large intestine, adenoma
) %>% 
  lapply(., function(x) paste0("\\b", x, "\\b", collapse = "|"))




### Making a nested list of words excluding abbreviations.
full_terms <- list(
  ### organs
  cortex_w = c("brains?", "cerebrums?", "cerebral?", "telencephalons?", "telencephalic", "endbrains?"), 
  basal_ganglion_w = c("basalganglion"), 
  hypothalamus_w = c("hypothalamus", "hypothalamic?"), 
  ventral_midbrain_w = c("ventral midbrains?"), 
  bbb_w = c("bloodbrainbarrier"), 
  choroid_plexus_w = c("choroidplexus", "choroidplexuses", "cerebrospinal fluids?"), 
  autonomic_ganglion_w = c("autonomic ganglion", "autonomic ganglia"), 
  dorsal_root_ganglion_w = c("dorsalrootganglion"), 
  neurovascular_unit_w = c("neurovascularunit"), 
  gliovascular_unit_w = c("gliovascular units?"), 
  enteric_nervous_system_w = c("enteric nervous systems?", "enteric neurons?"), 
  blood_retinal_barrier_w = c("bloodretinalbarrier"), 
  lacrimal_gland_w = c("lacrimalgland", "lacrimal"), 
  meibomian_gland_w = c("meibomian glands?", "meibocytes?"), 
  dental_follicle_w = c("dental follicles?"), 
  dental_papilla_w = c("dental papillae?", "apical papillae?"), 
  dental_pulp_w = c("dentalpulps?"), 
  salivary_gland_w = c("salivarygland"), 
  submandibular_gland_w = c("submandibulargland"), 
  bone_marrow_w = c("bonemarrow"),   
  germinal_center_w = c("germinal centers?"), 
  lymph_node_w = c("lymphnode"), 
  sinus_w = c("lymphatic", "lymphs?", "lympho", "lymphoids?", "lymphnode"), 
  lymphatic_vessel_w = c("lymphaticvessel"), 
  thyroid_w = c("thyroids?"), 
  valve_w = c("hearts?", "cardiac"), 
  blood_air_barrier = c("bloodairbarrier"), 
  gastroesophageal_junction_w = c("gastroesophageal junctions?"), 
  stomach_w = c("stomachs?", "gastric"), 
  gastric_antrum_w = c("stomachs?", "gastric"), 
  gastric_fundus_w = c("stomachs?", "gastric"), 
  gastric_corpus_w = c("stomachs?", "gastric"), 
  small_intestine_w = c("smallintestines?"), 
  liver_w = c("livers?", "hepatic", "hepatocytes?", "hepatostellate", "hepatic progenitor cells?", "hepatocellular"), 
  lobule_w = c("livers?", "hepatic"), 
  sinusoid_w = c("livers?", "hepatic"), 
  ihbd_w = c("intrahepatic bileduct", "intrahepatic ducts?"), 
  ehbd_w = c("extrahepatic bileduct", "extrahepatic ducts?"), 
  gallbladder_w = c("gallbladders?"), 
  oviduct_w = c("oviducts?", "oviductal", "salpinx", "salpinges", "salpinxes"), 
  collecting_duct_w = c("collectingduct"), 
  glomerulus_w = c("kidneys?", "renal", "nephrons?", "nephronal"), 
  dermal_papilla_w = c("dermal papillae?"), 
  sebaceous_gland_w = c("sebaceousgland", "sebaceous"), 
  sweat_gland_w = c("sweatgland"), 
  hair_follicle_w = c("hair follicles?", "hfscs?"), 
  anterior_cruciate_ligament_w = c("anterior cruciate ligaments?"), 
  artificial_joint_w = c("periprosthetic joints?", "artificial joints?", "prosthes[ei]s"), 
  muscle_w = c("muscles?", "myotubes?", "muscular"), 
  anulus_fibrosus_w = c("ann?ulus fibrosus"), 
  white_adipose_tissue_w = c("white adipose tissues?"), 
  
  ### Major organs
  neural_w = c("central nervous systems?", "peripheral nervous systems?", "neural"), 
  brain_w = c("brains?", "cyborg"), 
  mammary_w = c("breasts?", "mammary", "mammaries"), 
  gastrointestinal_w = c("gastrointestinal", "gastrointestines?"), 
  hpb_w = c("hepatic, pancreatic,? and biliary"), 
  urinary_w = c("urinary", "urines?", "urothelial"), 
 
  ### Tumor
  mpd_w = c("mammary paget’s diseases?"), 
  raird_w = c("refractory diseases?"), 
  cccc_w = c("cervical clear cell carcinomas?"), 
  hcc_w = c("hepatocellular carcinomas?", "fibrolamellar carcinomas?"), 
  ipmn_w = c("intraductal papillary mucinous neoplasms?"), 
  lusc_w = c("lung squamous cell carcinomas?"), 
  oscc_w = c("oral squamous cell carcinomas?"), 
  tcc_w = c("transitional cell carcinomas?"), 
  tsa_w = c("traditional serrated adenomas")
)  %>% 
  lapply(., function(x) paste0("\\b", x, "\\b", collapse = "|"))





##########
###
### 5. Identifying organ types of researched tumors.
###
### The code is based on the code in non-tumor model analysis with small changes.
###
##########

### Capturing full terms (excluding abbreviations) in the text fields.
tumor_full_TF <- as.data.frame(sapply(full_terms, function(x) grepl(x, pw_metastasis_corpus$text_all_lower)))

### Capturing all terms in the selected phrases for researched tumor types.
tumor_all_TF <- as.data.frame(sapply(all_terms, function(x) grepl(x, pw_metastasis_corpus$pw_sentence))) %>% 
  ### Word groups that need perl = TRUE are captured below.
  mutate(ganglion_w = grepl(paste0("\\b", p_ganglion_w, "\\b", collapse = "|"), pw_metastasis_corpus$pw_sentence, perl = TRUE)) %>% 
  mutate(epidermis_w = grepl(paste0("\\b", p_epidermis_w, "\\b", collapse = "|"), pw_metastasis_corpus$pw_sentence, perl = TRUE)) %>% 
  mutate(hair_follicle_w = 
           ifelse(grepl(paste0("\\b", p_hair_w, "\\b", collapse = "|"), pw_metastasis_corpus$pw_sentence, perl = TRUE), TRUE, hair_follicle_w))

### Capturing all terms in the selected phrases for metastasis.
metastasis_all_TF <- as.data.frame(sapply(all_terms, function(x) grepl(x, pw_metastasis_corpus$metastasis_text_all))) %>% 
  mutate(ganglion_w = grepl(paste0("\\b", p_ganglion_w, "\\b", collapse = "|"), pw_metastasis_corpus$metastasis_text_all, perl = TRUE)) %>% 
  mutate(epidermis_w = grepl(paste0("\\b", p_epidermis_w, "\\b", collapse = "|"), pw_metastasis_corpus$metastasis_text_all, perl = TRUE)) %>% 
  mutate(hair_follicle_w = 
           ifelse(grepl(paste0("\\b", p_hair_w, "\\b", collapse = "|"), pw_metastasis_corpus$metastasis_text_all, perl = TRUE), TRUE, 
                  hair_follicle_w))


### Making a custom function to adjust classification.
### The custom function is mostly identical to the one use in the previous analysis, except that some lines are removed. 
### Column index numbers are unchanged.
fn_tumor_adjustments <- function(TF_all, TF_full = tumor_full_TF) {
  ### From the logical data frame of all terms, only selecting columns that exist in the data frame of full terms.
  ### This is to make the two data frames in the same dimensions.
  TF_selected <- TF_all %>% 
    select(colnames(TF_full))
  ### Multiplying the logical values of the data frames of all terms and full terms.
  ### In this way, logical values in the all-term data frame are changed to FALSE if the corresponding full terms did not appear in the 
  ### combined text field.
  TF_adjusted <- TF_selected * TF_full
  ### Replacing the part of the logical data frame with the adjusted data frame.
  TF_modified <- TF_all %>% 
    select(!colnames(TF_full)) %>% 
    cbind(., (as.data.frame(lapply(TF_adjusted, as.logical)))) %>% 
    ### Reordering the columns
    select(colnames(TF_all)) %>% 
    ### Making adjustments in the data frame.
    ### Note that, unlike the code in the final_analysis/R/organ_types.R, major_organ categories are not changed to TRUE 
    ### even when lower-level categories are TRUE.
    mutate(mammary_w = ifelse(mpd_w == TRUE,TRUE, mammary_w)) %>% 
    mutate(lung_w = ifelse(lusc_w == TRUE, TRUE, lung_w)) %>% 
    mutate(large_intestine_w = ifelse(tsa_w == TRUE, TRUE, large_intestine_w)) %>% 
    mutate(pancreatic_duct_w = ifelse(ipmn_w == TRUE, TRUE, pancreatic_duct_w)) %>% 
    mutate(liver_w = ifelse(hcc_w == TRUE, TRUE, liver_w)) %>% 
    mutate(cervix_w = ifelse(cccc_w == TRUE, TRUE, cervix_w)) %>%  
    mutate(urinary_w = ifelse(tcc_w == TRUE, TRUE, urinary_w)) %>%   
    
    mutate(brain_w = ifelse(rowSums(.[2:17]) > 0, FALSE, brain_w)) %>% 
    mutate(forebrain_w = ifelse(rowSums(.[3:12]) > 0, FALSE, forebrain_w)) %>% 
    mutate(cerebrum_w = ifelse(rowSums(.[4:8]) > 0, FALSE, cerebrum_w)) %>% 
    mutate(cortex_w = ifelse(corticostriatal_network_w == TRUE, FALSE, cortex_w)) %>% 
    mutate(basal_ganglion_w = ifelse(striatum_w == TRUE, FALSE, basal_ganglion_w)) %>%     
    mutate(diencephalon_w = ifelse(rowSums(.[10:12]) > 0, FALSE, diencephalon_w)) %>% 
    mutate(hypothalamus_w = ifelse(pituitary_w == TRUE, FALSE, hypothalamus_w)) %>% 
    mutate(brainstem_w = ifelse(rowSums(.[14:17]) > 0, FALSE, brainstem_w)) %>% 
    mutate(hindbrain_w = ifelse(cerebellum_w == TRUE, FALSE, hindbrain_w)) %>% 
    mutate(glymphatic_w = ifelse(gliovascular_unit_w == TRUE, FALSE, glymphatic_w)) %>% 
    mutate(nerve_w = ifelse(rowSums(.[25:31]) > 0, FALSE, nerve_w)) %>%   
    mutate(ganglion_w = ifelse(dorsal_root_ganglion_w == TRUE, FALSE, ganglion_w)) %>%   
    mutate(neurovascular_w = ifelse(neurovascular_unit_w == TRUE, FALSE, neurovascular_w)) %>% 
    mutate(neuromuscular_w = ifelse(neuromuscular_junction_w == TRUE, FALSE, neuromuscular_w)) %>%  
    mutate(spine_w = ifelse(spinal_cord_w == TRUE, FALSE, spine_w)) %>%    
    mutate(retina_w = ifelse(blood_retinal_barrier_w == TRUE, FALSE, retina_w)) %>% 
    mutate(cornea_w = ifelse(corneal_limbus_w == TRUE | corneal_barrier_w == TRUE, FALSE, cornea_w)) %>% 
    mutate(tooth_w = ifelse(rowSums(.[47:51]) > 0, FALSE, tooth_w)) %>% 
    mutate(tongue_w = ifelse(taste_bud_w == TRUE | circumvallate_papilla_w == TRUE, FALSE, tongue_w)) %>% 
    mutate(salivary_gland_w = ifelse(parotid_gland_w == TRUE | submandibular_gland_w == TRUE, FALSE, salivary_gland_w)) %>% 
    mutate(lymphoid_w = ifelse(rowSums(.[60:66]) > 0, FALSE, lymphoid_w)) %>% 
    mutate(lymph_node_w = ifelse(sinus_w == TRUE, FALSE, lymph_node_w)) %>% 
    mutate(thyroid_w = ifelse(parathyroid_w == TRUE | thyroid_gland_w == TRUE, FALSE, thyroid_w)) %>% 
    mutate(heart_w = ifelse(myocardium_w == TRUE | valve_w == TRUE, FALSE, heart_w)) %>% 
    mutate(vascular_w = ifelse(rowSums(.[79:84]) > 0, FALSE, vascular_w)) %>% 
    mutate(blood_vessel_w = ifelse(rowSums(.[81:84]) > 0, FALSE, blood_vessel_w)) %>% 
    mutate(artery_w = ifelse(aorta_w == TRUE, FALSE, artery_w)) %>%      
    mutate(airway_w = ifelse(rowSums(.[87:93]) > 0, FALSE, airway_w)) %>% 
    mutate(pharynx_w = ifelse(rowSums(.[88:90]) > 0, FALSE, pharynx_w)) %>% 
    mutate(trachea_w = ifelse(tracheosphere_w == TRUE, FALSE, trachea_w)) %>% 
    mutate(lung_w = ifelse(rowSums(.[95:98]) > 0, FALSE, lung_w)) %>% 
    mutate(stomach_w = ifelse(rowSums(.[103:107]) > 0, FALSE, stomach_w)) %>% 
    mutate(gastric_corpus_w = ifelse(gastric_gland_w == TRUE, FALSE, gastric_corpus_w)) %>%     
    mutate(intestine_w = ifelse(rowSums(.[109:117]) > 0, FALSE, intestine_w)) %>% 
    mutate(small_intestine_w = ifelse(rowSums(.[110:112]) > 0, FALSE, small_intestine_w)) %>% 
    mutate(large_intestine_w = ifelse(rowSums(.[114:117]) > 0, FALSE, large_intestine_w)) %>% 
    mutate(pancreas_w = ifelse(pancreatic_duct_w == TRUE | islet_w == TRUE, FALSE, pancreas_w)) %>% 
    mutate(liver_w = ifelse(lobule_w == TRUE | sinusoid_w == TRUE, FALSE, liver_w)) %>% 
    mutate(lobule_w = ifelse(sinusoid_w == TRUE, FALSE, lobule_w)) %>%     
    mutate(biliary_w = ifelse(rowSums(.[125:128]) > 0, FALSE, biliary_w)) %>% 
    mutate(bile_duct_w = ifelse(ihbd_w == TRUE | ehbd_w == TRUE, FALSE, bile_duct_w)) %>% 
    mutate(female_reproductive_w = ifelse(rowSums(.[130:137]) > 0, FALSE, female_reproductive_w)) %>%   
    mutate(uterus_w = ifelse(rowSums(.[133:135]) > 0, FALSE, uterus_w)) %>% 
    mutate(endometrium_w = ifelse(endometrial_gland_w == TRUE | decidua_w == TRUE, FALSE, endometrium_w)) %>% 
    mutate(kidney_w = ifelse(rowSums(.[142:147]) > 0, FALSE, kidney_w)) %>% 
    mutate(nephron_w = ifelse(rowSums(.[143:147]) > 0, FALSE, nephron_w)) %>% 
    mutate(renal_tubule_w = ifelse(rowSums(.[144:146]) > 0, FALSE, renal_tubule_w)) %>%   
    mutate(hair_follicle_w = ifelse(dermal_papilla_w == TRUE | sebaceous_gland_w == TRUE, FALSE, hair_follicle_w)) %>% 
    mutate(osteochondral_w = ifelse(bone_w == TRUE | cartilage_w == TRUE, FALSE, osteochondral_w)) %>% 
    mutate(joint_w = ifelse(rowSums(.[159:162]) > 0, FALSE, joint_w)) %>% 
    mutate(synovial_joint_w = ifelse(synovium_w == TRUE, FALSE, synovial_joint_w)) %>%  
  return(TF_modified)
}

### Applying the custom function to the all-term data frame.
tumor_TF_modified <- fn_tumor_adjustments(tumor_all_TF) %>% 
  ### Removing the unnecessary columns (those of abbreviations)
  select(any_of(colnames(pw_metastasis_corpus))) %>% 
  mutate(ID = pw_metastasis_corpus$ID)

### Applying the custom function to the metastasis data frame.
metastasis_TF_modified <- fn_tumor_adjustments(metastasis_all_TF) %>% 
  select(any_of(colnames(pw_metastasis_corpus))) %>% 
  mutate(ID = pw_metastasis_corpus$ID)

### Making a custom function to convert the data frames to a long format.
fn_category_longer <- function(modified_TF) {
  tumor_longer <- modified_TF %>% 
    pivot_longer(., ends_with("_w"), names_to = "tumor_category") %>% 
    filter(value == TRUE) %>% 
    select(ID, tumor_category) %>% 
    ### Adjusting writing styles
    mutate(tumor_category = gsub("_w$", "", tumor_category)) %>% 
    mutate(tumor_category = gsub("_", " ", tumor_category)) %>% 
    mutate(tumor_category = gsub("bbb", "blood-brain barrier", tumor_category)) %>% 
    mutate(tumor_category = gsub("blood retinal barrier", "blood-retinal barrier", tumor_category)) %>% 
    mutate(tumor_category = gsub("ehbd", "extrahepatic bile duct", tumor_category)) %>% 
    mutate(tumor_category = gsub("ihbd", "intrahepatic bile duct", tumor_category)) 
}

### Applying the custom function to the all-term and metastasis data frame.
researched_longer <- fn_category_longer(tumor_TF_modified)

metastasis_longer <- fn_category_longer(metastasis_TF_modified)

###  Adding additional researched tumor categories based on tumor-related terms that correspond to specific organs.
###
### From the corpus data frame, only selecting "ID" and "organ_from_tumor" columns.
researched_combined_longer <- pw_metastasis_corpus %>% 
  select(ID, organ_from_tumor) %>% 
  ### If publications have multiple organ categories in the organ_from_tumor, they are separated into multiple columns.
  ### For this purpose, the max number of organ categories per publication is calculated.
  mutate(n_organ_from_tumor = ifelse(is.na(organ_from_tumor) | organ_from_tumor == "", 0, str_count(organ_from_tumor, ";") + 1)) %>% 
  ### Separating organ categories into multiple columns.
  separate(organ_from_tumor, paste0("organ_from_tumor_", c(1:max(.$n_organ_from_tumor))), sep = ";", fill = "right") %>% 
  ### Converting to a longer format.
  pivot_longer(starts_with("organ_from_tumor_"), values_to = "tumor_category") %>% 
  filter(!is.na(tumor_category), 
         !tumor_category == "") %>% 
  select(ID, tumor_category) %>% 
  ### Combining with the previous data frame of the researched organ classification.
  rbind(., researched_longer) %>% 
  arrange(ID) %>% 
  distinct()

### In the above data frames, a publication may be classified according to different category levels of the same organ 
### (e.g., colon and large intestine).
### These duplicate categories are deduplicated.
### For this, a custom function is made which shows all higher level categories of the determined organs.
### If a paper is classified according to an organ type and one of its higher level organ categories, the higher level 
### category is removed.
fn_deduplicate_tumor_category <- function(tumor_longer) {
  tumor_all_categories <- tumor_longer %>% 
    left_join(., edge_all[, 1:2], by = c("tumor_category" = "to")) %>% 
    rename(from_TR = from) %>% 
    left_join(., edge_all[, 1:2], by = c("from_TR" = "to")) %>% 
    rename(from_TR2 = from) %>% 
    left_join(., edge_all[, 1:2], by = c("from_TR2" = "to")) %>% 
    rename(from_TR3 = from) %>% 
    left_join(., edge_all[, 1:2], by = c("from_TR3" = "to")) %>% 
    rename(from_TR4 = from) %>% 
    left_join(., edge_all[, 1:2], by = c("from_TR4" = "to")) %>% 
    rename(from_TR5 = from) 
  unconverted_rows <- tumor_all_categories %>% 
    filter(!is.na(from_TR5) & !from_TR5 == "body")
  if(nrow(unconverted_rows) > 0) warning("some categories have not been fully converted")
  tumor_deduplicated <- tumor_all_categories %>% 
    group_by(ID) %>% 
    filter(!tumor_category %in% c(from_TR, from_TR2, from_TR3, from_TR4, from_TR5)) %>% 
    ungroup() %>% 
    select(!c(from_TR, from_TR2, from_TR3, from_TR4, from_TR5))
  return(tumor_deduplicated)
}

### Applying the custom function to researched tumor and metastasis data frames.
researched_deduplicated <- fn_deduplicate_tumor_category(researched_combined_longer) %>% 
  rename(researched_tumor = tumor_category) %>% 
  group_by(ID) %>% 
  mutate(neuroendocrine_tumor = ifelse("neuroendocrine" %in% researched_tumor, TRUE, FALSE), 
         n_ID = n()) %>% 
  ungroup() %>% 
  filter(!(n_ID > 1 & researched_tumor == "neuroendocrine")) %>% 
  select(!n_ID)

metastasis_deduplicated <- fn_deduplicate_tumor_category(metastasis_longer) %>% 
  rename(metastasis_organ = tumor_category) %>% 
  filter(!metastasis_organ == "neuroendocrine")











##########
###
### 6. Identifying tumor models of organs.
###
##########

### Below, organ models that match organ types of researched tumors are determined and considered as tumor models.

colnames(pw_metastasis_corpus)

### Converting the original corpus into a long format to show all the organ categories (which were determined in the previous analysis) 
### that publications have in a single column.
### Note that both minor and major organ categories are included, as these two are not separated for researched tumors and metastasis.
organ_type_all <- pw_metastasis_corpus %>% 
  ### Converting to a long format based on all "minor_organ_" and "major_organ_" columns.
  pivot_longer(contains("or_organ_"), values_to = "all_organ", values_drop_na = TRUE) %>% 
  select(ID, all_organ) %>% 
  ### Adding a column showing the 1st-level organ categories of corresponding organs.
  left_join(., edge_all[, 2:3], by = c("all_organ" = "to")) %>% 
  ### If a publication has both a lower level and a corresponding 1st-level organ categories, the 1st-level category is removed 
  ### to avoid duplication.
  filter(!(duplicated(.[c(1, 3)]) & all_organ == major_organ)) %>% 
  select(!major_organ)

### Combining data frames of researched tumors and organ types.
### Then, organ types that match researched tumor types are determined.
### For this purpose, organ categories are converted to tumor types groups, so that, for example, 
### ("colon", "rectum", "large intestine") are all grouped as "large intestine" and are accordingly matched.
tumor_NT_organs <- full_join(organ_type_all, researched_deduplicated, by = "ID") %>% 
  arrange(ID) %>% 
  ### Adding a column to show a tumor type group of the organ category.
  left_join(., edge_all[, c(2, 4)], by = c("all_organ" = "to")) %>% 
  rename(all_organ_converted = tumor_group) %>% 
  ### Adding a column to show a tumor type group of the researched tumor.
  left_join(., edge_all[, c(2, 4)], by = c("researched_tumor" = "to")) %>% 
  rename(researched_tumor_converted = tumor_group) %>% 
  group_by(ID) %>% 
  ### Determining matching and unmatching organ types.
  ### "intestine" is also matched to "large intestine".
  ### Matching organ types are assigned to the "tumor_organ" column, whereas unmatching organ types are assigned to the "NT_organ" column.
  mutate(tumor_organ = 
           ifelse(is.na(all_organ), NA, 
                  ifelse(all_organ_converted == "intestine" & "large intestine" %in% researched_tumor_converted, "large intestine", 
                         ifelse(all_organ_converted %in% researched_tumor_converted, all_organ, NA)))) %>% 
  mutate(NT_organ = 
           ifelse(is.na(all_organ), NA, 
                  ifelse(all_organ_converted == "intestine" & "large intestine" %in% researched_tumor_converted, NA, 
                         ifelse(!all_organ_converted %in% researched_tumor_converted, all_organ, NA)))) %>% 
  ### counting the number of researched tumor types in each publication.
  mutate(n_researched_tumor = length(unique(researched_tumor_converted))) %>% 
  mutate(n_tumor_organ = sum(!is.na(tumor_organ))) %>% 
  ### If tumor_organ was not determined above, and if the number of researched tumor of the publication is 1, the tumor_organ category is 
  ### assumed to be that of researched_tumor.
  mutate(tumor_organ = ifelse(!is.na(tumor_organ), tumor_organ, 
                              ifelse(n_tumor_organ == 0 & n_researched_tumor == 1, researched_tumor, NA))) %>% 
  select(!c(n_tumor_organ, n_researched_tumor)) %>% 
  ungroup()

### Saving.
### This data frame is used for a quality control.
save(tumor_NT_organs, file = paste0(root_path, "R_temps/tumor_NT_organs"))




### Checking publications on rare researched tumor types.
###
### Identifying tumor organ categories with less than six publications.
rare_researched_tumor <- tumor_NT_organs %>% 
  left_join(., pw_metastasis_corpus %>% select(ID, corpus_F), by = "ID") %>% 
  group_by(tumor_organ, corpus_F) %>% 
  mutate(count = n()) %>% 
  filter(count < 6)

### Selecting publications that involve one of the rare researched tumor types.
rare_researched_tumor_papers <- tumor_NT_organs %>% 
  filter(ID %in% rare_researched_tumor$ID) %>% 
  left_join(., pw_metastasis_corpus %>% select(ID, text_all), by = "ID") %>% 
  select(ID, text_all, researched_tumor, tumor_organ, NT_organ)  %>% 
  mutate(across(c("researched_tumor", "tumor_organ", "NT_organ"), ~ replace_na(., "")))

### Saving as a csv file.
write.csv(rare_researched_tumor_papers, 
          file = paste0(root_path, "csv/temps/rare_researched_tumor_papers.csv"), 
          row.names = FALSE)

### The above csv file was checked and saved as csv/rare_researched_tumor_papers_F.csv.
### This file also includes papers previously checked, and is longer than the rare_researched_tumor_papers.
rare_researched_tumor_papers_F <- read.csv(paste0(root_path, "csv/rare_researched_tumor_papers_F.csv")) %>% 
  mutate(across(c("researched_tumor", "tumor_organ", "NT_organ"), ~ ifelse(. == "", NA, .)))

rare_researched_tumor_papers_F %>% count(tumor_organ_adjusted, sort = TRUE)

tumor_NT_organs_adjusted <- tumor_NT_organs %>% 
  filter(ID %in% rare_researched_tumor_papers_F$ID) %>% 
  left_join(., rare_researched_tumor_papers_F %>% select(!text_all)) %>% 
  mutate(tumor_organ = ifelse(tumor_organ_adjusted == "ok", tumor_organ, tumor_organ_adjusted)) %>% 
  select(!tumor_organ_adjusted) %>% 
  rbind(., tumor_NT_organs %>% filter(!ID %in% rare_researched_tumor_papers_F$ID)) %>% 
  arrange(ID) %>% 
  distinct()

save(tumor_NT_organs_adjusted, file = paste0(root_path, "R_temps/tumor_NT_organs_adjusted"))


### Making a custom function to pivot organ categories wider.
### This is to convert the data frame into one-row-one-document format.
### The input data frame is expected to have the publication ID in the first column, organ type categories in the second column, 
### and the corresponding major organ categories.
fn_separate_categories <- function(organ_category) {
  category_type = colnames(organ_category[, 2])
  organs_separated <- organ_category %>% 
    filter(!is.na(eval(parse(text = category_type)))) %>% 
    group_by(ID) %>% 
    mutate(n_major_organ = length(unique(major_organ))) %>% 
    ungroup() %>% 
    mutate(major_organ = ifelse(n_major_organ > 1, "multiple organs", major_organ)) %>% 
    select(!n_major_organ) %>% 
    distinct() %>% 
    group_by(ID) %>% 
    mutate(n_organ = n(), 
           all = paste0(eval(parse(text = category_type)), collapse = ";")) %>% 
    ungroup() %>% 
    select(!all_of(category_type)) %>% 
    distinct() %>% 
    separate(all, paste0(category_type, "_", c(1:max(.$n_organ))), sep = ";", remove = FALSE, fill = "right") %>% 
    select(!n_organ)
}


### Applying the custom function for the tumor_organ category.
tumor_organ_separated <- tumor_NT_organs_adjusted %>% 
  ### Selecting the ID and the tumor_organ category columns.
  select(ID, tumor_organ) %>% 
  ### Adding a column of 1st-level (i.e., major) organ categories.
  left_join(., edge_all[, 2:3], by = c("tumor_organ" = "to")) %>% 
  ### Applying the custom function.
  fn_separate_categories(.) %>% 
  ### Renaming the column names.
  rename(tumor_organ_all = all, 
         tumor_organ_major = major_organ)


### Applying the custom function for the metastasis_organ category.
metastasis_organs <- metastasis_deduplicated %>% 
  ### First, among the organ categories identified as metastatic organs, those matching the tumor_organs categories are removed as 
  ### they represent origins of metastasis rather than metastatic organs.
  ### For this, tumor_organ categories are added to the data frame for metastasis.
  left_join(., tumor_NT_organs_adjusted %>% 
              select(ID, tumor_organ) %>% 
              filter(!is.na(tumor_organ)) %>% 
              distinct(), 
            by = "ID") %>% 
  mutate(tumor_organ = replace_na(tumor_organ, "unidentified")) %>% 
  ### Metastatic organs matching tumor organs are removed.
  filter(!metastasis_organ == tumor_organ) %>% 
  ### To also remove matching organs that are shown at different levels of the hierarchical classification, organ categories are converted to 
  ### tumor_group categories that are umbrella categories of tumor groups.
  left_join(., edge_all[, c(2, 4)], by = c("metastasis_organ" = "to")) %>% 
  rename(metastasis_major = tumor_group) %>% 
  left_join(., edge_all[, c(2, 4)], by = c("tumor_organ" = "to")) %>% 
  rename(tumor_major = tumor_group) %>% 
  group_by(ID) %>% 
  ### Removing metastatic organ categories that belongs to the same tumor groups as tumor organs.
  filter(!metastasis_major %in% tumor_major) %>% 
  filter(!(metastasis_major %in% c("large intestine", "intestine", "gastrointestinal") & 
             tumor_major %in% c("large intestine", "intestine", "gastrointestinal"))) %>% 
  ungroup()


### Converting the metastasis data frame to the one-row-one-publication format.
metastasis_organ_separated <- metastasis_organs %>% 
  select(ID, metastasis_organ) %>% 
  left_join(., edge_all[, 2:3], by = c("metastasis_organ" = "to")) %>% 
  fn_separate_categories(.) %>% 
  rename(metastasis_organ_all = all, 
         metastasis_organ_major = major_organ)

### Converting the researched_tumor categories to the one-row-one-publication format.
researched_tumor_separated <- tumor_NT_organs_adjusted %>% 
  select(ID, researched_tumor) %>% 
  left_join(., edge_all[, 2:3], by = c("researched_tumor" = "to")) %>% 
  fn_separate_categories(.) %>% 
  rename(researched_tumor_all = all, 
         researched_tumor_major = major_organ)

### Converting the NT_organ categories to the one-row-one-publication format.
NT_organ_separated <- tumor_NT_organs_adjusted %>% 
  select(ID, NT_organ) %>% 
  left_join(., edge_all[, 2:3], by = c("NT_organ" = "to")) %>% 
  fn_separate_categories(.) %>% 
  rename(NT_organ_all = all, 
         NT_organ_major = major_organ)




##########
###
### 7. Making the tumor organ classification.
###
##########

colnames(pw_metastasis_corpus)

### Combining all data frames of organ categories
organ_types_CF <- organ_types_F %>% 
  select(!corpus_F)

pre_tumor_type <- pw_metastasis_corpus %>% 
  select(!any_of(colnames(organ_types_CF[, -1:-30]))) %>% 
  left_join(., researched_tumor_separated, by = "ID") %>% 
  left_join(., tumor_organ_separated, by = "ID") %>% 
  left_join(., NT_organ_separated, by = "ID") %>% 
  left_join(., metastasis_organ_separated, by = "ID") %>% 
  ### Making a column summarizing researched tumor categories.
  mutate(researched_tumor_type = 
           ifelse(is.na(researched_tumor_all), NA,
                  ifelse(!grepl(";", researched_tumor_all), researched_tumor_all, researched_tumor_major))) %>% 
  ### Making a column summarizing tumor organ categories.
  mutate(tumor_organ_type = 
           ifelse(is.na(tumor_organ_all), NA,
                  ifelse(!grepl(";", tumor_organ_all), tumor_organ_all, tumor_organ_major))) %>% 
  ### Making a column summarizing NT organ categories
  mutate(NT_organ_type = 
           ifelse(is.na(NT_organ_all), NA,
                  ifelse(!grepl(";", NT_organ_all), NT_organ_all, NT_organ_major))) %>% 
  ### Making a column summarizing matastatic organ categories.
  mutate(metastasis_organ_type = 
           ifelse(is.na(metastasis_organ_all), NA,
                  ifelse(!grepl(";", metastasis_organ_all), metastasis_organ_all, metastasis_organ_major))) %>% 
  mutate(across(c("researched_tumor_type", "researched_tumor_major", "tumor_organ_type", "tumor_organ_major", 
                  "NT_organ_type", "NT_organ_major", "metastasis_organ_type", "metastasis_organ_major"), 
                ~ replace_na(., "unidentified"))) 




### May save the result.
save(pre_tumor_type, 
     file = paste0(root_path, "R_temps/pre_tumor_type"))


### Assigning hierarchy to the organ classification.
### For this purpose, "unspecified" subcategories are introduced under 1st-level organ categories, as well as some 2nd-level categories.
pre_tumor_type_unspecifying <- pre_tumor_type %>% 
  mutate(tumor_organ_type_unspecified = 
           ifelse(is.na(tumor_organ_type), NA, 
                  ifelse(tumor_organ_type %in% c("multiple organs", "nasal", "venom gland", "unidentified"), tumor_organ_type, 
                         ifelse(tumor_organ_type %in% c(unique(edge_all$major_organ), "brain", "nerve", "spine", "intestine"), 
                                paste0("unspecified ", tumor_organ_type), 
                                tumor_organ_type)))) 

### Selecting columns from the edge_list.
edge_list <- edge_all %>% 
  select(from, to)

### Adding hierarchy to the organ classification
tumor_types_F <- pre_tumor_type_unspecifying %>% 
  mutate(from_OR = tumor_organ_type_unspecified) %>% 
  left_join(., edge_list, by = c("from_OR" = "to")) %>% 
  rename(from_OR4 = from) %>%   
  left_join(., edge_list, by = c("from_OR4" = "to")) %>% 
  rename(from_OR3 = from) %>%   
  left_join(., edge_list, by = c("from_OR3" = "to")) %>% 
  rename(from_OR2 = from) %>% 
  left_join(., edge_list, by = c("from_OR2" = "to")) %>% 
  rename(from_OR1 = from) %>% 
  left_join(., edge_list, by = c("from_OR1" = "to")) %>% 
  rename(from_OR0 = from)

### Checking that the latest column (from_OR1) only contains the zero-level category of "body"
tumor_types_F %>% count(from_OR0)



### Saving the result.
save(tumor_types_F, file = paste0(root_path, "R_results/tumor_types_F"))

### Removing copyright-protected text fields.
tumor_types_P <- tumor_types_F %>% 
  select(!c(7:25, 28:30, 32:34, 37:40))

save(tumor_types_P, file = paste0(root_path, "R_results/tumor_types_P"))


colnames(tumor_types_F)

tumor_types_F %>% 
  count(tumor_organ_type, sort = TRUE)



##########
###
### Writing supplementary csv file showing organ types of publications.
###
##########

organ_models_tumor <- tumor_types_F %>% 
  rename(organ_type = tumor_organ_type, 
         major_organ = tumor_organ_major) %>% 
  filter(type == "Research article", 
         corpus_F == "tumor_organoid", 
         !major_organ == "unidentified") %>% 
  select(author, year, title, doi, major_organ, organ_type) %>% 
  arrange(major_organ, organ_type, year, author)

write.csv(organ_models_tumor, 
          file = paste0(root_path, "results/csv/table_S1_organ_models_tumor.csv"), 
          row.names = FALSE)


organ_models_ToC <- tumor_types_F %>% 
  rename(organ_type = tumor_organ_type, 
         major_organ = tumor_organ_major) %>% 
  filter(type == "Research article", 
         corpus_F == "ToC", 
         !major_organ == "unidentified") %>% 
  select(author, year, title, doi, major_organ, organ_type) %>% 
  arrange(major_organ, organ_type, year, author)

write.csv(organ_models_ToC, 
          file = paste0(root_path, "results/csv/table_S2_organ_models_ToC.csv"), 
          row.names = FALSE)
