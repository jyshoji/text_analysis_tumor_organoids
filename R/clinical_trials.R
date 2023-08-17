### The code here is to analyze clinical trials that use tumor organoids/ToC.

### Loading a package and the data.
library(tidyverse)
library(tidytext)

### Setting the path to the root folder.
root_path <- "~/Research_data/Hybrida/tumor_analysis/"
### Change the above according to your root folder location.
### If you use Windows, change all occurrences of "/" in file paths to "\\".

### Loading files for converting organ type categories.
edge_all <- read.csv(paste0(root_path, "csv/edge_all.csv")) 
tumor_term_nns <- read.csv(paste0(root_path, "csv/tumor_terms.csv")) %>% 
  filter(!include == "ns")

### Loading the csv file of clinical trials using tumor + organoids.
CT_organoid <- read.csv(paste0(root_path, "clinical_trials/SearchResults_organoid.csv")) %>% 
  select(!Rank)

### Loading the csv file of clinical trials using tumor + OoC.
CT_OoC <- read.csv(paste0(root_path, "clinical_trials/SearchResults_OoC.csv")) %>% 
  select(!Rank)

### Combining the corpora of clinical trials using organoids and OoC.
CT_data <- rbind(CT_organoid, CT_OoC) %>% 
  ### removing the duplicate documents
  filter(!duplicated(.[, "NCT.Number"])) %>% 
  ### Making the ID column to give unique identification numbers to each document.
  ### Making new columns that show if the documents are found in the organoid or OoC corpora.
  mutate(ID = c(1:nrow(.)), 
         organoid = ifelse(NCT.Number %in% CT_organoid$NCT.Number, TRUE, FALSE), 
         OoC = ifelse(NCT.Number %in% CT_OoC$NCT.Number, TRUE, FALSE)) %>% 
  ### Making the new column "year" showing the year of the start dates.
  mutate(year = str_extract(Start.Date, "[0-9]{4}$")) %>% 
  ### Removing withdrawn clinical trials.
  filter(!Status == "Withdrawn")

### In the above data frame, subcategories and their values of study design and interventions are stored in the same column.
### These are separated across columns below.
CT_design_intervention_wider <- CT_data %>% 
  unnest_tokens(phrase, Study.Designs, token = stringr::str_split, pattern = "\\|") %>% 
  separate(phrase, c("study_design_sub", "study_design_value"), sep = ": ") %>% 
  unnest_tokens(phrase, Interventions, token = stringr::str_split, pattern = "\\|") %>% 
  separate(phrase, c("intervention_sub", "intervention_value"), sep = ": ", fill = "right") %>% 
  unnest_tokens(intervention_value_separated, intervention_value, token = stringr::str_split, pattern = " and | or | \\+ |/") 

### Restyling the Condition column.
CT_condition_modified <- CT_design_intervention_wider %>% 
  ### Multiple values in the Condition columns are separated across rows.
  unnest_tokens(condition_simplified, Conditions, token = stringr::str_split, pattern = "\\|| and ") %>% 
  ### Frequently occurring tumor-related words are changed to "tumor".
  mutate(condition_simplified = gsub("tumou?rs?|cancers?|neoplasms?|\\badenocarcinomas?|\\bcarcinomas?", "tumor", condition_simplified)) %>% 
  ### Two-words organ names are force-united into single words.
  mutate(condition_simplified = gsub("pancreatic[- ]ductal|pancreatic[- ]ducts?", "pancreaticduct", condition_simplified)) %>% 
  mutate(condition_simplified = gsub(
    "liver[- ]ductal|liver[- ]ducts?|bile[- ]ductal|bile[- ]ductular|bile[- ]ducts?|hepatobiliary[- ]ductal|hepatobiliary[- ]ducts?", 
    "bileduct", condition_simplified)) %>%   
  mutate(condition_simplified = gsub("breast[- ]ductal|breast[- ]ducts?", "breastduct", condition_simplified)) %>%  
  mutate(condition_simplified = gsub("biliary[- ]tracts?", "biliarytract", condition_simplified)) %>%  
  mutate(condition_simplified = gsub("fallopian[- ]tubes?|uterine[- ]tubes?", "oviduct", condition_simplified)) %>%  
  mutate(condition_simplified = gsub("gall[- ]bladder", "gallbladder", condition_simplified)) %>% 
  ### conditions that do not include tumor-related terms are changed to blank and ignored.
  mutate(condition_simplified = ifelse(grepl("tumor|omas?\\b|dysplasia|polyp", condition_simplified), condition_simplified, "")) %>% 
  ### The term "metastasis" and the preceding word are removed to avoid capturing metastatic organ name
  mutate(condition_simplified = gsub("[a-z]+ metastas[ei]s", "", condition_simplified))


### Making a nested list of all terms (both full terms and abbreviations) of organ names.
### This is the same as the one used in ./R/tumor_types.R.
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

### Capturing terms of the above word list in the condition_simplified column.
tumor_all_TF <- as.data.frame(sapply(all_terms, function(x) grepl(x, CT_condition_modified$condition_simplified)))

### Transforming the above data frame.
CT_condition_captured <- CT_condition_modified %>% 
  ### If a paper is TRUE for more than two organs, it is classified as "multi".
  ### If a paper is not TRUE for any of the organ names, it is classified as blank "".
  ### If a paper is TRUE for just one organ, it is classified according to the organ.
  mutate(tumor_term_captured = ifelse(rowSums(tumor_all_TF) > 1, "multi", 
                                      ifelse(rowSums(tumor_all_TF) == 0, "", colnames(tumor_all_TF)[max.col(tumor_all_TF)]))) %>% 
  ### Removing underlines and prefix from the organ category column.
  mutate(tumor_term_captured = gsub("_w$", "", tumor_term_captured)) %>% 
  mutate(tumor_term_captured = gsub("_", " ", tumor_term_captured)) %>% 
  ### If a tumor-related term that specify organ (e.g., glioma for brain) appears, then the paper is classified according to the organ name.
  mutate(oma_term = str_extract(condition_simplified, "[a-z]+omas?")) %>% 
  left_join(., tumor_term_nns, by = c("oma_term" = "tumor")) %>% 
  mutate(tumor_term_captured = ifelse(!tumor_term_captured == "", tumor_term_captured, 
                                      ifelse(!is.na(include), include, "")))

### Making a data frame to manually check conditions which have not been categorized with organ names.
unclassified_conditions <- CT_condition_captured %>% 
  select(condition_simplified, tumor_term_captured) %>% 
  distinct() %>% 
  filter(!tumor_term_captured %in% edge_all$to)

### Writing a csv file.
write.csv(unclassified_conditions, file = paste0(root_path, "csv/temps/unclassified_conditions.csv"), row.names = FALSE)

### The above file was manually checked, and loaded below.
unclassified_conditions_F <- read.csv(paste0(root_path, "csv/unclassified_conditions_F.csv")) %>% 
  rename(tumor_term_captured2 = tumor_term_captured)

### Making a summary data frame, by incorporating the manually adjusted classification and determining the final classification.
clinical_trials_P <- left_join(CT_condition_captured, unclassified_conditions_F, by = "condition_simplified") %>% 
  mutate(tumor_term_captured = ifelse(tumor_term_captured %in% edge_all$to, tumor_term_captured, tumor_term_captured2)) %>% 
  left_join(., edge_all[, c(2, 4)], by = c("tumor_term_captured" = "to")) %>% 
  mutate(tumor_group = ifelse(!is.na(tumor_group), tumor_group, 
                               ifelse(!tumor_term_captured == "", "other", NA))) %>% 
  select(!c(oma_term, include, tumor_term_captured2))

save(clinical_trials_P, file = paste0(root_path, "R_results/clinical_trials_P"))










### Drawing bar chart of study designs.
study_design_bar <- clinical_trials_P %>% 
  select(ID, study_design_sub, study_design_value, year) %>% 
  filter(study_design_sub == "primary purpose") %>% 
  distinct() %>% 
  ggplot(aes(x = year, fill = factor(study_design_value, levels = c("basic science", "prevention", "diagnostic", "treatment", "other")))) + 
  geom_bar() + 
  scale_x_discrete(limits = factor(c(2014:2023)), breaks = c(2014, 2016, 2018, 2020, 2022), name = "year") + 
  scale_fill_discrete(name = "primary purpose") + 
  labs(title = "Primary pupose in clinical trials") + 
  ylab("the number of clinical trials") + 
  theme(text = element_text(size = 8), 
        #axis.text.x.bottom = element_text(size = 5), 
        #legend.title = element_text(size = 5), 
        legend.key.size = unit(2.5, "mm")) 

ggsave(study_design_bar, 
       file = paste0(root_path, "results/clinical_trials/study_design_bar.pdf"), 
       width = 100, height = 90, units = "mm")


### Also drawing bar chart of study design (primary purpose), but plotting all document.
primary_purpose_bar <- clinical_trials_P %>% 
  select(ID, year, study_design_sub, study_design_value) %>% 
  distinct() %>% 
  mutate(study_design_sub = gsub(" ", "_", study_design_sub)) %>% 
  pivot_wider(names_from = study_design_sub, values_from = study_design_value) %>% 
  ggplot(aes(x = year, fill = factor(primary_purpose, levels = c("basic science", "prevention", "diagnostic", "treatment", "other", NA)))) + 
  geom_bar() + 
  scale_x_discrete(limits = factor(c(2008:2023)), breaks = c(2008, 2010, 2012, 2014, 2016, 2018, 2020, 2022), name = "year") + 
  scale_fill_discrete(name = "primary purpose") + 
  labs(title = "Clinical trials using tumor organoids/ToC") + 
  ylab("the number of clinical trials") + 
  theme(text = element_text(size = 7), 
        plot.title = element_text(size = 8, face = 2), 
        #axis.text.x.bottom = element_text(size = 5), 
        #legend.title = element_text(size = 5), 
        legend.key.size = unit(2, "mm")) 

ggsave(primary_purpose_bar, 
       file = paste0(root_path, "results/clinical_trials/primary_purpose_bar.pdf"), 
       width = 85, height = 60, units = "mm")


### Drawing bar chart of study type
study_type_bar <- clinical_trials_P %>% 
  select(ID, year, Study.Type) %>% 
  distinct() %>% 
  ggplot(., aes(x = year, fill = Study.Type)) + 
  geom_bar() + 
  scale_x_discrete(limits = factor(c(2008:2023)), breaks = c(2008, 2010, 2012, 2014, 2016, 2018, 2020, 2022), name = "year") + 
  scale_fill_discrete(name = "Study types") + 
  labs(title = "Study types in clinical trials") + 
  ylab("the number of clinical trials") + 
  theme(text = element_text(size = 7), 
        #axis.text.x.bottom = element_text(size = 5), 
        #legend.title = element_text(size = 5), 
        legend.key.size = unit(2.5, "mm")) 

ggsave(study_type_bar, 
       file = paste0(root_path, "results/clinical_trials/study_type_bar.pdf"), 
       width = 85, height = 60, units = "mm")


### Drawing commonly tested drugs.
common_drugs <- clinical_trials_P %>% 
  filter(intervention_sub == "drug") %>% 
  select(ID, intervention_value_separated) %>% 
  distinct() %>% 
  count(intervention_value_separated, sort = TRUE) %>% 
  filter(n > 1)

common_drugs_bar <- clinical_trials_P %>% 
  filter(intervention_sub == "drug") %>% 
  select(ID, intervention_value_separated) %>% 
  distinct() %>% 
  filter(!is.na(intervention_value_separated)) %>% 
  filter(intervention_value_separated %in% common_drugs$intervention_value_separated) %>% 
  ggplot(aes(x = fct_rev(intervention_value_separated))) + 
  geom_bar() + 
  labs(title = "Commonly tested drugs in clincial trials") + 
  xlab("commonly tested drugs") + 
  ylab("the number of clinical trials") + 
  coord_flip() + 
  theme(text = element_text(size = 7)) 

ggsave(common_drugs_bar, 
       file = paste0(root_path, "results/clinical_trials/common_drugs_bar.pdf"), 
       width = 85, height = 90, units = "mm")
