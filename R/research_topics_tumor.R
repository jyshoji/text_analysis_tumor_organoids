### This R script file is for identifying miscellaneous research topics in tumor organoid and 
### ToC publications, including research interests, studied disease, and used techniques.
###
### The file uses "./R_results/tumor_types_F" as an input.
### The code here is based on the one used for organoids/OoC analysis, with some topics added/omitted.
###
### The file includes following steps: 
### 1. Identifying tumor subgroups in publications.
### 2. Preparing lists of research topics to capture
### 3. Capturing research topics.
### 4. Making a summary data frame of research topics and other research themes.
###
### The outcome of the codes are saved as:
### "./R_results/research_topics_F"
### which were used in TF_tile_graph.R to draw tile graphs.



### Loading a package
library(tidyverse)

### Setting the path to the root folder.
root_path <- "~/Research_data/Hybrida/tumor_analysis/"
### Change the above according to your root folder location.
### If you use Windows, change all occurrences of "/" in file paths to "\\".

### Loading the input file.
load(paste0(root_path, "R_results/tumor_types_F"))




##########
###
### 1. Identifying tumor subgroups in publications.
###
##########

### Making a nested list of tumor subgroup terms.
other_tumor_terms <- list(
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
  nonmuscle_invasive_bladder_cancer = c("nonmuscle[- ]invasive bladder cancers?")
)  %>% 
  lapply(., function(x) paste0("\\b", x, "\\b", collapse = "|"))


### Identifying tumor subgroups, and changing the values from logical to character.
### The resulting data frame has publications in rows, and tumor subgroups in columns.
additional_tumors_longer <- as.data.frame(sapply(other_tumor_terms, function(x) grepl(x, tumor_types_F$text_all_lower))) %>% 
  mutate(ID = tumor_types_F$ID) %>% 
  pivot_longer(c(!ID)) %>% 
  filter(value == TRUE) %>% 
  mutate(name = gsub("_", " ", name)) %>% 
  group_by(ID) %>% 
  mutate(n_other_tumor = n()) %>% 
  mutate(other_tumors_all = paste0(name, collapse = ";")) %>% 
  ungroup() %>% 
  select(!c(name, value)) %>% 
  distinct() %>% 
  separate(other_tumors_all, paste0("researched_tumor_other_", c(1:max(.$n_other_tumor))), sep = ";", fill = "right") %>% 
  select(!n_other_tumor)

### Adding the data frame to the original data frame.
additional_tumor_types <- left_join(tumor_types_F, additional_tumors_longer, by = "ID")





##########
###
### 2. Preparing lists of research topics to capture
###
########## 

### Loading a csv file of terms that are to be considered for organ development when appearing before "development".
pre_dev_inc <- read.csv(paste0(root_path, "csv/pre_dev_F.csv")) %>% 
  filter(include == "y")

### Loading a csv file of terms that are to be considered as tumor.
tumor_inc <- read.csv(paste0(root_path, "csv/tumor_detected_F.csv")) %>% 
  filter(include == "y")

### Selecting tumor-related terms ending with "oma" or "omas" from the above terms.
oma_captured <- tumor_inc %>% 
  filter(grepl("omas?\\b", tumor)) %>% 
  mutate(tumor = gsub("omas\\b", "oma", tumor)) %>% 
  arrange(.$tumor) 

### Making a vector containing terms that are to be considered as multi-component organoids 
### when appearing between multi/multiple and "organoid".
multi_w = c("lineage", "tissue", "layered", "layer", "region", "system", "organ", 
            "lobed", "cell type", "compartment")



### Making a nested list of research topic terms.
research_topics_w <- list(
  ### Diseases
  ### Note that disease terms were captured to be consistent with our previous analysis on non-tumor models.
  ### These terms were in the end not included in the figures as they were not very relevant.
  ### Instead, terms below "Research interests" were used for the figures.
  
  ### Viral disease
  coronavirus = c("coronavirus", "coronaviruses", "coronaviral", "sars[- ]cov[- ]2", "covid[- ]19", 
                  "severe acute respiratory syndromes?", "porcine epidemic diarrhea virus", 
                  "porcine epidemic diarrhea viruses", "porcine deltacoronavirus", "porcine deltacoronaviruses"), 
  astrovirus = c("astrovirus", "astroviruses", "astroviral"), 
  enterovirus = c("enterovirus", "enteroviruses", "enteroviral"), 
  flavivirus = c("flavivirus", "flaviviruses", "flaviviral"),  
  norovirus = c("norovirus", "noroviruses", "noroviral"), 
  lentivirus = c("lentivirus", "lentiviruses", "lentiviral"),  
  rotavirus = c("rotavirus", "rotaviruses", "rotaviral"), 
  Zika_virus = c("zika", "zikv"), 
  dengue = c("dengue"), 
  influenza = c("influenza"), 
  HIV = c("hivs?"), 
  HPV = c("human papillomavirus", "hpvs?"), 
  
  ## neural diseases
  epilepsy = c("epilepsy", "epilepsies", "epileptic"), 
  guillain_barre_syndrome = c("guillain[- ]barre? syndromes?"), 
  hypertonia = c("hypertonia"), 
  leigh_syndrome = c("leigh syndromes?", "leigh diseases?", "subacute necrotizing encephalomyelopathy"), 
  traumatic_brain_injury = c("traumatic brain injury", "traumatic brain injuries", "intracranial injury", "intracranial injuries"), 
  
  ## neurodegenerative disorder
  neurodegenerative_disorder = c("neurodegenerative diseases?", "neurodegenerative disorders?", "dementia"), 
  alzheimers_disease = c("alzheimer'?s diseases?"), 
  huntingtons_disease = c("huntington'?s diseases?"), 
  parkinsons_disease = c("parkinson'?s diseases?"), 
  motor_neuron_disease = c("motor neurone? diseases?", "amyotrophic lateral sclerosis", "amyotrophic lateral sclerotic", 
                           "lou gehrig'?s? diseases?"), 
  
  ## mental_disorder
  mental_disorder = c("neuropsychiatric disorders?", "psychiatric disorders?", "mental disorders?", "neuropsychiatric diseases?", 
                      "mental health disorders?", "mental illness", "mental diseases?", "psychological disorders?"), 
  attentione_deficit = c("attention deficits?"), 
  autism_spectrum = c("autism spectrum"), 
  bipolar_disorders = c("bipolar disorders?", "bipolar diseases?"), 
  schizophrenia = c("schizophrenia"), 
  
  ## eye disease
  amaurosis = c("amaurosis"), 
  glaucoma = c("glaucoma"), 
  retinal_degenerative_disease = c("retinal degenerative diseases?", "retinal degenerations?"), 
  macular_degeneration = c("macular degenerations?"), 
  retinal_dystrophy = c("retinal dystrophy", "retinal dystrophies"), 
  retinitis_pigmentosa = c("retinitis pigmentosa"), 
  stargardt_disease = c("stargardt diseases?"), 
  
  ## respiratory disease 
  acute_respiratory_distress_syndrome = c("acute respiratory distress syndromes?"), 
  chronic_obstructive_pulmonary_disease = c("chronic obstructive pulmonary diseases?", "chronic obstructive lung diseases?", 
                                            "chronic obstructive airway diseases?"), 
  pulmonary_hypertension = c("pulmonary hypertensions?", "pulmonary[- ]arterial hypertensions?"), 
  emphysema = c("emphysema"), 
  asthma = c("asthma"), 
  
  ## cardiovascular disease
  aneurysm = c("aneurysms?", "microaneurysms?"), 
  arrhythmia = c("arrhythmias?", "dysrhythmias?"), 
  atherosclerosis = c("atherosclerosis", "atherosclerotic"), 
  endocarditis = c("endocarditis"), 
  hemolytic_uremic_syndrome = c("ha?emolytic[- ]uremic syndromes?"), 
  hypoplastic_left_heart_syndrome = c("hypoplastic left heart", "cyanotic heart diseases?"), 
  sickle_cell_disease = c("sickle cell diseases?", "sickle cell disorders?", "sickle cell anaemias?"), 
  thrombosis = c("thrombos[ei]s"), 
  heart_failure = c("heart failure", "cardiac failure"), 
  
  ## gastrointestinal disease
  gastroesophageal_reflux_disease = c("gastro-?o?esophageal reflux diseases?"), 
  gastroenteritis = c("gastroenteriti[cs]"), 
  gastritis = c("gastriti[cs]"), 
  enterocolitis = c("enterocoliti[cs]"), 
  enteritis = c("enteriti[cs]"), 
  colitis = c("coliti[cs]"), 
  inflammatory_bowel_disease = c("inflammatory bowel diseases?"), 
  ulcerative_colitis = c("ulcerative coliti[cs]"), 
  crohns_disease = c("crohn'?s diseases?"), 
  coeliac_disease = c("co?eliac diseases?"), 
  microvillous_inclusion_disease = c("microvillo?us inclusion diseases?"), 
  irritable_bowel_syndrome = c("irritable bowel syndromes?"), 
  short_bowel_syndrome = c("short bowel syndromes?"), 
  diarrhea = c("diarrho?eal?"), 
  barretts_esophagus = c("barrett'?s esophagus"), 
  cholestasis = c("cholestasis"), 
  
  ## hpb disease
  diabetes = c("diabetes", "diabetic"), 
  cirrhosis = c("cirrhosis", "end[- ]stage liver diseases?", "chronic liver failures?"), 
  hepatotoxicity = c("drug[- ]induced liver injury", "drug[- ]induced liver injuries", "hepatotoxicity", "drug[- ]induced liver diseases?"), 
  fatty_liver_disease = c("fatty liver diseases?", "steatohepatitis", "hepatosteatosis", "hepatic steatosis"), 
  hepatitis = c("hepatitis"), 
  acute_liver_failure = c("acute liver failures?"), 
  cholangitis = c("cholangitis"), 
  
  ## kidney disease
  chronic_kidney_disease = c("chronic kidney diseases?", "chronic renal diseases?"), 
  end_stage_renal_disease = c("end[- ]stage kidney diseases?", "end[- ]stage renal diseases?", "kidney failures?", "renal failures?"), 
  nephrotoxicity = c("nephrotoxicity", "drug[- ]induced kidney injury", "drug[- ]induced kidney injuries", 
                     "drug[- ]induced renal injury", "drug[- ]induced renal injuries"), 
  polycystic_kidney_disease = c("polycystic kidney diseases?"), 
  acute_kidney_injury = c("acute kidney injury", "acute kidney injuries", "acute renal failure"), 
  
  ## Musculoskeletal diseases
  arthritis = c("[a-z]*arthrit[ei]s"), 
  osteoarthritis = c("osteoarthrit[ei]s"),   
  muscular_dystrophy = c("muscular dystrophy", "muscular dystrophies"), 
  
  ## fibrosis
  fibrosis = c("fibrosis", "fibrotic"), 
  cystic_fibrosis = c("cystic fibrosis"), 
  idiopathic_pulmonary_fibrosis = c("idiopathic pulmonary fibrosis"), 
  
  ## genetic disorder
  alagille_syndrome = c("alagille syndromes?", "alagille[- ]watson syndromes?", "hepatic ductular hypoplasia"), 
  alport_syndrome = c("alport syndromes?"), 
  ciliopathy = c("ciliopathy"), 
  Friedreichs_ataxia = c("friedreich's ataxia"), 
  fragile_X_syndrome = c("fragile x syndromes?"), 
  hermansky_pudlak_syndrome = c("hermansky[- ]pudlak syndromes?"), 
  hirschsprung_disease = c("hirschsprung'?s? diseases?"), 
  lynch_syndrome = c("lynch syndromes?", "hereditary nonpolyposis colorectal cancers?"), 
  lysosomal_storage_disease = c("lysosomal storage diseases?"), 
  microcephaly = c("microcephaly"), 
  niemann_pick_disease = c("niemann[- ]pick diseases?"), 
  Nijmegen_breakage_syndrome = c("nijmegen breakage syndromes?"), 
  progeria = c("progeria"), 
  down_syndrome = c("down'?s? syndromes?"),   
  rett_syndrome = c("rett syndromes?"), 
  wilson_disease = c("wilson'?s? diseases?"), 
  
  ## other disease
  atresia = c("atresia"), 
  callus = c("callus", "calluses"), 
  dystrophy = c("dystrophy", "dystrophies"), 
  endometriosis = c("endometriosis"), 
  graft_versus_host_disease = c("graft[- ]versus[- ]host diseases?"), 
  helminthiasis = c("helminthias[ei]s", "helminthosis", "helminth infections?", "worm infections?"), 
  ischemia = c("ischa?emi[ac]"),   
  reperfusion_injury = c("reperfusion injury", "reperfusion injuries", "reoxygenation injury", "reoxygenation injuries"), 
  injury = c("injury", "injuries"), 
  radiation_injury = c("radiation injury", "radiation injuries", "radiation[- ]induced [a-z]* ?injury", "radiation[- ]induced [a-z]* ?injuries", 
                       "radiation syndromes?", "radiation[- ]induced [a-z]* ?syndromes?"), 
  refractory_disease = c("refractory diseases?"), 
  obesity = c("obesity"), 
  polyposis = c("polyposis"), 
  pre_eclampsia = c("pre[- ]?eclampsia"), 
  prion = c("prions?"),   
  psoriasis = c("psoriasis"), 
  sclerosis = c("[a-z]*sclerosis", "[a-z]*sclerotic"), 
  thyroid_associated_orbitopathy = c("thyroid[- ]associated orbitopathy"), 
  ulcer = c("ulcers?"), 
  sepsis = c("sepsis"), 
  
  ### Microbiology
  host_microbe_interactions = c("host[- ]pathogen interactions?", "host[- ]microbe interactions?", "host[- ]microbiome interactions?", 
                                "host[- ]parasite interactions?", "gut[- ]microb[a-z]*", "intestinal[- ]microb[a-z]*", "microbiota", 
                                "microbiomes?", "dysbiosis"), 
  probiotics = c("probiotics?"),   
  Chlamydia = c("chlamydias?"),  
  Citrobacter = c("citrobacter"), 
  Escherichia_coli = c("escherichia coli", "e\\. coli"), 
  lactobacillus = c("lactobacillus", "lactobacilli"), 
  Helicobacter_pylori = c("helicobacter pylori", "h\\. pylori"), 
  Clostridioides_difficile = c("clostridioides difficile", "c\\. difficile"), 
  Salmonella = c("salmonella"), 
  Staphylococcus = c("staphylococcus"), 
  tuberculosis = c("tuberculosis", "phthisis"), 
  Cryptosporidium = c("cryptosporidium"), 
  malaria = c("malaria"), 
  nematode = c("nematodes?"), 
  Toxoplasma = c("toxoplasma"), 
  
  ### Research interests
  organ_development = c("organogenes[ei]s", "embryogenes[ei]s", "neurogenes[ei]s", "morphogenes[ei]s", "nephrogenesis", 
                        "corticogenesis", "vasculogenesis", "neurodevelopments?", 
                        paste0(pre_dev_inc$pre_dev, "[- ]developments?", collapse = "|")),
  tumor = tumor_inc$tumor, 
  disease = c("diseases?", "syndromes?", "disorders?", "infections?", "injury", "injuries", "dystrophy", "dystrophies"),
  electrophysiology = c("electro[- ]?physiology", "electro[- ]?physiological"), 
  mechanobiology = c("mechano[- ]?biology", "mechano[- ]?transductions?", "mechano[- ]?sensations?", "mechano[- ]?sensing"), 
  high_throughput_screening = c("high[- ]throughput [a-z]* ?screenings?", "high[- ]throughput [a-z]* ?screens?", 
                                "high[- ]throughput [a-z]* ?platforms?"), 
  ethics = c("ethics?", "ethical", "neuroethics?", "neuroethical", "bioethics?", "bioethical", "metabioethics?", "metabioethical"), 
  
  ### pharmacology
  drug_development = c("drug developments?", "drug screenings?", "drug discovery", "drug discoveries", 
                       "developments? of [a-z]*[- ]?[a-z]* ?drugs?", "discovery of [a-z]*[- ]?[a-z]* ?drugs?", 
                       "discoveries of [a-z]*[- ]?[a-z]* ?drugs?", "screenings? of [a-z]*[- ]?[a-z]* ?drugs?"), 
  drug_testing = c("drug testing", "testing of [a-z]*[- ]?[a-z]* ?drugs?", "drug responses?"), 
  drug_delivery = c("drug delivery", "drug deliveries", "delivery of [a-z]*[- ]?[a-z]* ?drugs?", "deliveries of [a-z]*[- ]?[a-z]* ?drugs?"), 
  drug_resistance = c("drug resistances?"), 
  pharmacokinetics = c("pharmacokinetics?", "pharmacodynamics?", "adme", "drug metabolisms?", "drug absorptions?"), 
  pharmacogenomics = c("pharmacogenomics?", "pharmacogenetics?"), 
  toxicology = c("[a-z]*toxicology", "[a-z]*toxicological"), 
  
  ## Clinical interests
  clinical_study = c("clinical [a-z]* ?trials?", "clinical [a-z]* ?study", "clinical [a-z]* ?studies"), 
  preclinical_model = c("pre[- ]?clinical [a-z]* ?models?", "pre[- ]?clinical [a-z]* ?study", 
                        "pre[- ]?clinical [a-z]* ?studies", "disease models?", "disease modell?ing", 
                        "tumou?r models?", "tumou?r modell?ing", "cancer models?", "cancer modell?ing"), 
  precision_medicine = c("precision [a-z]* ?medicines?", "personali[sz]ed [a-z]* ?medicines?", 
                         "precision [a-z]* ?therap[a-z]+", "personali[sz]ed [a-z]* ?therap[a-z]+", 
                         "precision [a-z]* ?treatments?", "personali[sz]ed [a-z]* ?treatments?", 
                         "precision oncology", "personali[sz]ed oncology"), 
  homeostasis = c("homeostas[ei]s"), 
  regeneration = c("regenerations?", "regenerative medicines?", "regenerative therap[a-z]+", "tissue renewals?", 
                   "tissue repairs?", "cell renewals?"), 
  transplantation = c("transplantations?"), 
  
  ### therapy types
  drug_therapy = c("drug therap[a-z]+", "drug treatments?"), 
  chemotherapy = c("chemo[- ]?therap[a-z]+"),  
  chemoresistance = c("chemo[- ]?resistan[a-z]+", "chemo[- ]?therap[a-z]+ resistan[a-z]+"), 
  targeted_therapy = c("targeted therap[a-z]+", "targeted treatments?", "antibody[- ]drug conjugates?"), 
  hormone_therapy = c("hormone therap[a-z]+", "hormonal therap[a-z]+"), 
  gene_therapy = c("gene therap[a-z]+"), 
  immunotherapy = c("immuno[- ]?therap[a-z]+", "immuno[- ]oncology", "immune checkpoint"), 
  immune_checkpoint_blockade = c("immune[- ]checkpoint"), 
  oncolytic_virus = c("oncolytic [a-z]*virus", "oncolytic [a-z]*viruses", "oncolytic [a-z]*virotherapy"), 
  adoptive_cell_therapy = c("adoptive [a-z]* ?cell [a-z]* ?therap[a-z]+", "adoptive [a-z]* ?cellular [a-z]* ?therap[a-z]+"), 
  monoclonal_antibody = c("monoclonal antibody", "monoclonal antibodies", "nivolumab", "ipilimumab", "pembrolizumab", 
                          "toripalimab", "cetuximab", "infliximab", "cibisatamab", "trastuzumab", "pertuzumab", "bevacizumab", 
                          "dinutuximab", "atezolizumab", "daratumumab", "rituximab", "vantictumab", "tocilizumab", "panitumumab", 
                          "cirmtuzumab", "rosmantuzumab", "avelumab", "durvalumab", "ramucirumab"), 
  radiotherapy = c("radio[- ]?therap[a-z]+", "radiation[- ]?therap[a-z]+"), 
  radioresistance = c("radio[- ]?resistan[a-z]+", "radiation [a-z]* ?resistan[a-z]+", "radio[- ]?therap[a-z]+ resistan[a-z]+"), 
  neoadjuvant_therapy = c("neo[- ]?adjuvant [a-z /]*therap[a-z]+", "neo[- ]?adjuvant [a-z /]*treatments?", 
                          "neo[- ]?adjuvant chemoradiations?", "neo[- ]?adjuvant[- ]treated"), 
  replacement_therapy = c("replacement therap[a-z]+"),  
  photodynamic_therapy = c("photodynamic therap[a-z]+"), 
  photothermal_therapy = c("photothermal therap[a-z]+"), 
  combination_therapy = c("combination therap[a-z]+"), 
  systemic_therapy = c("systemic therap[a-z]+"), 
  
  ### model systems
  microfluidics = c("microfluidics?"), 
  organoid = c("organoids?", "enteroids?", "gastruloids?", "colonoids?", "blastoids?", "tumou?roids?", 
               "assembloids?", "embryoids?", "cerebroids?", "cardioids?"), 
  organ_on_chip = c("onchip", "oocs?", "micro[- ]?physiological systems?", 
                    "micro[- ]?physiology systems?", "micro[- ]?physiologic systems?"), 
  patient_derived = c("patient[- ]derived"), 
  patient_derived_organoid = c("patient[- ]derived [a-z]*[- ]?[a-z]*[- ]?organoids?"), 
  patient_derived_tumor_organoid = c("patient[- ]derived [a-z]*[- ]?tumor organoids?", 
                                     "patient[- ]derived [a-z]*[- ]?cancer organoids?"),  
  xenograft = c("xenografts?", "xenografted"), 
  patient_derived_xenograft = c("patient[- ]derived [a-z]*[- ]?[a-z]*[- ]?xenografts?"),  
  grafted_organoid = c("grafted [a-z]*[- ]?organoids?", "xenografted [a-z]*[- ]?organoids?", 
                       "transplanted [a-z]*[- ]?organoids?"), 
  gemm = c("genetically engineered mouse models?"), 
  biobanking = c("biobanks?", "biobanking"), 
  assembloids = c("assembloids?"), 

  ## tumor words
  cancer = c("cancers?", "cancerous"), 
  neoplasm = c("neoplastic", "neoplasms?"), 
  leukemia = c("leuka?emias?"), 
  
  ## tumor physiology
  tumorigenesis = c("tumou?rigenes[ei]s", "carcinogenes[ei]s"), 
  cancer_stem_cell = c("cancer stem cells?","tumou?r stem cells?", "cancer stem[- ]like cells?", "tumor[- ]initiating cells?"), 
  microsatellite_instability = c("microsatellite instability"), 
  tumor_heterogeneity = c("tumou?r heterogeneity", "cancer heterogeneity", "tumou?ral heterogeneity", 
                          "intra[- ]?tumou?r heterogeneity", "intra[- ]?cancer heterogeneity", "intra[- ]?tumou?ral heterogeneity", 
                          "inter[- ]?tumou?r heterogeneity", "inter[- ]?cancer heterogeneity", "inter[- ]?tumou?ral heterogeneity"), 
  intra_tumor_heterogeneity = c("intra[- ]?tumou?r heterogeneity", "intra[- ]?cancer heterogeneity", "intra[- ]?tumou?ral heterogeneity"), 

  ## tumor microenvironment
  tumor_microenvironment = c("tumou?r [a-z]* ?micro-?environments?", "cancer [a-z]* ?micro-?environments?"), 
  angiogenesis = c("angiogenes[ei]s"), 
  lymphangiogenesis = c("lymphangiogenes[ei]s"), 
  tumor_vessel = c("tumorvessel", "tumor vasculatures?"), 
  cancer_associated_fibroblast = c("cancer[- ]associated fibroblasts?"), 
  hypoxia = c("hypoxia"), 
  tumor_stroma_interactions = c("tumor[- ]stroma interactions?"), 
  
  ## Immune cells
  immune_cells = c("immune cells?", "dendritic cells?", "eosinophils?", "lymphocytes?", "macrophages?", "mast cells?", 
                   "myeloid[- ]derived suppressor cells?", "natural killer", "neutrophils?", "t[- ]cells?"), 
  immune_cells_key = c("immune cells?"), 
  lymphatic = c("lymphoid", "lymph[- ]vessels?", "lymphatic[- ]vessels?", "lymphatic[- ]vasculatures?"), 
  primary_lymphoid_organs = c("thymus", "thymuses", "thymic?", "bone marrows?"), 
  secondary_lymphoid_organs = c("spleens?", "germinal centers?", "tonsils?", "tonsillar", "lymph[- ]nodes?", 
                                "lymph[- ]glands?", "lymph[- ]glandular"), 
  resident_immune_cells = c("resident immune cells?", "kupffer cells?", "micro[- ]?glia"), 
  dendritic_cell = c("dendritic cells?"), 
  eosinophil = c("eosinophils?"), 
  lymphocyte = c("lymphocytes?", "natural killer", "t[- ]cells?"), 
  NK_cell = c("natural killer"), 
  B_cell = c("b[- ]cells?"), 
  T_cell = c("t[- ]cells?"),  
  tumor_infiltrating_lymphocytes = c("tumor[- ]infiltrating lymphocytes?"),   
  macrophage = c("macrophages?", "kupffer cells?", "micro[- ]?glia"), 
  tumor_associated_macrophage = c("tumor[- ]associated macrophages?"), 
  mast_cell = c("mast cells?"), 
  MDSC = c("myeloid[- ]derived suppressor cells?"), 
  neutrophil = c("neutrophils?"), 

  ### metastasis
  metastasis = c("metastas[ei]s", "metastatic", "metasta[sz]ized?"), 
  circulating_tumor_cell = c("circulating tumou?r cells?", "circulating cancer cells?"), 
  emt = c("epithelial[- ]mesenchymal transitions?"), 
  intra_extravasation = c("inravasations?", "extravasations?", "intravasate[ds]?", "extravasate[ds]?"), 
  invasion = c("invasions?"), 
  met = c("mesenchymal[- ]epithelial transitions?"), 
  
  ## cellular processes
  apoptosis = c("apoptosis", "apoptotic"), 
  cell_cycle_arrest = c("cell[- ]cycle arrests?"), 
  extracellular_vesicles = c("exosomes?", "extracellular vesicles?"), 
  ferroptosis = c("ferroptosis", "ferroptotic"), 
  necroptosis = c("necroptosis", "necroptotic"), 
  autophagy = c("autophagy", "autophagic"), 
  ER_stress = c("endoplasmic reticulum stress", "er stress", "unfolded protein responses?"), 
  reactive_oxigen_species = c("reactive oxygen species", "ros", "oxidative stress"), 
  redox = c("redox"), 

  
  ## physiology
  barrier_function = c("barrier functions?"), 
  epithelial_barrier = c("epithelial barriers?"), 
  stem_cell_niche = c("stem[- ]cell niches?"),   
  chemotaxis = c("chemotaxis"), 
  crypt_villus_axis = c("crypt[- ]villus ax[ei]s"), 
  gut_brain_axis = c("gut[- ]brain[- ]ax[ei]s"), 
  gut_liver_axis = c("gut[- ]liver[- ]ax[ei]s"), 
  innate_immune_response = c("innate immune responses?", "innate immune systems?"), 
  quiescence = c("quiescence"), 
  senescence = c("senescence", "age?ing"), 
  
  ## organoid-generating techniques
  ThreeD_printing = c("3d)?[- ]printings?", "three[- ]dimensional[- ]printings?", "3d)?[- ]printed", "three[- ]dimensional[- ]printed"), 
  ThreeD_bioprinting = c("3d)?[- ]bioprintings?", "three[- ]dimensional[- ]bioprintings?", "3d)?[- ]bioprinted", 
                         "three[- ]dimensional[- ]bioprinted", "3d)? cell[- ]printings?", "three[- ]dimensional cell[- ]printings?", 
                         "3d)? cell[- ]printed", "three[- ]dimensional cell[- ]printed", "bioinks?"), 
  self_organization = c("self[- ]organi[sz]ed", "self[- ]organization"), 
  vascularization = c("vasculari[sz]ations?", "vasculari[sz]ed"), 
  multi_structure_organoid = c(paste0("multi-?", multi_w, "[- ]?[a-z]*[- ]organoids?"), 
                               paste0("multiple ", multi_w, "[- ]?[a-z]*[- ]organoids?")), 
  gene_editing = c("genome[- ]editing", "crispr", "gene[- ]editing"), 
  tissue_engineering = c("tissue[- ]engineering", "tissue[- ]engineered"), 
  cardiac_tissue_engineering = c("cardiac tissue[- ]engineering"), 
  engineered_heart_tissue = c("engineered heart tissues?"), 
  microelectrode_array = c("micro[- ]?electrode arrays?"), 
  
  ## Analytical techniques
  genomics = c("genomics?", "genome sequencing"), 
  epigenetics = c("epigenetics?", "epigenomics?", "epigenomes?", "dna[- ]methylations?"), 
  exome_sequencing = c("exome sequencing"), 
  transcriptomics = c("transcriptomics?", "transcriptomes?"), 
  sc_RNA_seq = c("single[- ]cell m?rna sequencing", "single[- ]cell m?rna seq", 
                 "single[- ]cell transcriptomics?", "single[- ]cell transcriptomes?", "scrna seq"),   
  proteomics = c("proteomics?", "proteomes?", "phosphoproteomics?", "phosphoproteomes?"), 
  metabolomics = c("metabolomics?", "metabolomes?"), 
  next_generation_sequencing = c("next[- ]generation sequencing", "ngs"), 
  immunohistochemistry = c("immuno[- ]?histochemistry", "immuno[- ]?histochemical", "immuno[- ]?fluorescence", "immuno[- ]?fluorescent", 
                           "immuno[- ]?stainings?", "immuno[- ]cytochemistry", "immuno[- ]cytochemical"), 
  cell_lineage = c("lineage tracing", "cell lineages?"), 
  whole_mount_techniques = c("whole[- ]mount"), 
  TEER = c("transendothelial electrical resistance", "transepithelial electrical resistance", "teer"), 
  acoustofluidics = c("acoustofluidics?"), 
  optogenetics = c("optogenetics?"), 
  high_content_analysis = c("high[- ]content screenings?", "high[- ]content analys[ei]s", "high[- ]content imagings?", "cellomics?"), 
  machine_learning = c("machine[- ]learning", "deep[- ]learning"), 
  cancer_genome_atlas = c("cancer genome atlas"), 
  gene_set_enrichment_analysis = c("gene set enrichment analys[ei]s"), 
  
  ## Imaging techniques
  expansion_microscopy = c("expansion microscopy"), 
  intravital_microscopy = c("intravital microscopy"), 
  optical_coherence_tomography = c("optical coherence tomography"), 
  optical_metabolic_imaging = c("optical metabolic imagings?"), 
  quantitative_phase_imaging = c("quantitative[- ]phase[- ]imaging", "quantitative[- ]phase[- ]contrast[- ]microscopy", "quantitative[- ]phase[- ]microscopy"), 
  Raman_spectroscopy = c("raman"), 
  
  ### tumor types
  minimal_residual_disease = c("minimal residual diseases?"),
  signet_ring_cell_carcinoma = c("signet[- ]ring cell carcinomas?"), 
  small_cell_carcinoma = c("small[- ]cell carcinomas?"), 
  squamous_cell_carcinoma = c("squamous cell carcinomas?"), 
  
  ### monoclonal antibodies
  atezolizumab = c("atezolizumab"), 
  avelumab = c("avelumab"), 
  bevacizumab = c("bevacizumab"), 
  cetuximab = c("cetuximab"), 
  cibisatamab = c("cibisatamab"), 
  cirmtuzumab = c("cirmtuzumab"), 
  daratumumab = c("daratumumab"), 
  dinutuximab = c("dinutuximab"), 
  durvalumab = c("durvalumab"), 
  infliximab = c("infliximab"), 
  ipilimumab = c("ipilimumab"), 
  nivolumab = c("nivolumab"), 
  panitumumab = c("panitumumab"), 
  pembrolizumab = c("pembrolizumab"), 
  pertuzumab = c("pertuzumab"), 
  ramucirumab = c("ramucirumab"), 
  rituximab = c("rituximab"), 
  rosmantuzumab = c("rosmantuzumab"), 
  tocilizumab = c("tocilizumab"), 
  toripalimab = c("toripalimab"), 
  trastuzumab = c("trastuzumab"), 
  vantictumab = c("vantictumab")
  ) %>% 
  lapply(., function(x) paste0("\\b", x, "\\b", collapse = "|"))


### Making another nested list for terms that need case-sensitive searches.
CS_topics <- list(
  mass_spectrometry = c("[Mm]ass[- ][Ss]pectrometry", "MS", "MALDI-MSI"), 
  MS_imaging = c("[Mm]ass[- ][Ss]pectrometric[- ][Ii]magings?", "[Mm]ass[- ][Ss]pectrometry[- ][Ii]magings?", 
                 "MS[- ][Ii]magings?", "MALDI-MSI"), 
  atomic_force_microscopy = c("[Aa]tomic [Ff]orce [Mm]icroscopy", "AFM"), 
  fluorescence_lifetime_imaging_microscopy = c("[Ff]luorescence[- ][Ll]ifetime [Ii]maging [Mm]icroscopy", "FLIM"), 
  light_sheet_microscopy = c("[Ll]ight[- ][Ss]heet [Mm]icroscopy", "[Ll]ight[- ][Ss]heet [Ff]luorescence [Mm]icroscopy", "LSFM"), 
  ### "MRI" is not captured at the moment as its use is mostly not directly on organoids/organ-on-a-chip.
  #MRI = c("[Mm]agnetic [Rr]esonance [Ii]maging", "MRI"), 
  transmission_electron_microscopy = c("[Tt]ransmission [Ee]lectron [Mm]icroscopy", "TEM"), 
  scanning_electron_microscopy = c("[Ss]canning [Ee]lectron [Mm]icroscopy", "SEM"), 
  FACS = c("[Ff]luorescence-[Aa]ctivated [Cc]ell [Ss]orting", "[Ff]luorescent-[Aa]ctivated [Cc]ell [Ss]orting, FACS")
) %>% 
  lapply(., function(x) paste0("\\b", x, "\\b", collapse = "|"))











##########
###
### 3. Capturing research topics.
###
##########

### Capturing research topics.
pre_TF_purpose <- as.data.frame(sapply(research_topics_w, function(x) grepl(x, tolower(additional_tumor_types$text_all_lower)))) %>% 
  ### "immune cells" is also captured in "key" sentences only, to see if it can mostly capture immune cells as components of organoids.
  mutate(immune_cells_key = grepl("immune cells?", tolower(additional_tumor_types$text_all_lower))) %>% 
  rename_with(., ~ gsub("^", "TF_", .))

### Capturing research topics that needs case-sensitive searches.
CS_TF <- as.data.frame(sapply(CS_topics, function(x) grepl(x, additional_tumor_types$text_all_lower))) %>% 
  rename_with(., ~ gsub("^", "TF_", .))






##########
###
### 4. Making a summary data frame of research topics and other research themes.
###
##########

### Combining data frames of the corpus, research topics, cell sources, organisms
TF_purpose <- additional_tumor_types %>% 
  cbind(., pre_TF_purpose) %>% 
  cbind(., CS_TF)

colnames(TF_purpose)

### Adjusting the logical columns.
### Basically, an upper category is changed to TRUE if any of the lower categories are TRUE.
research_topics_T <- TF_purpose %>% 
  mutate(TF_disease = ifelse(rowSums(.[, 89:206]) > 0, TRUE, TF_disease)) %>% 
  mutate(TF_host_microbe_interactions = as.logical(rowSums(.[, 207:221]))) %>% 
  mutate(TF_neurodegenerative_disorder = as.logical(rowSums(.[, 106:110]))) %>% 
  mutate(TF_mental_disorder = as.logical(rowSums(.[, 111:115]))) %>% 
  mutate(TF_retinal_dystrophy = ifelse(TF_retinitis_pigmentosa == TRUE, TRUE, TF_retinal_dystrophy)) %>% 
  mutate(TF_retinal_degenerative_disease = ifelse(TF_retinal_dystrophy == TRUE, TRUE, TF_retinal_degenerative_disease)) %>% 
  mutate(TF_inflammatory_bowel_disease = 
           ifelse(TF_ulcerative_colitis == TRUE | TF_crohns_disease == TRUE, TRUE, TF_inflammatory_bowel_disease)) %>% 
  mutate(TF_colitis = ifelse(TF_inflammatory_bowel_disease == TRUE, TRUE, TF_colitis)) %>% 
  mutate(TF_enterocolitis = ifelse(TF_enteritis == TRUE | TF_colitis == TRUE, TRUE, TF_enterocolitis), 
         TF_gastroenteritis = ifelse(TF_gastritis == TRUE | TF_enteritis == TRUE, TRUE, TF_gastroenteritis)) 

  

### Saving the result
save(research_topics_T, file = paste0(root_path, "R_results/research_topics_T"))

### Removing copyright-protected text fields.
research_topics_PT <- research_topics_T %>% 
  select(!c(7:25, 28:30, 32:34, 37:40))

save(research_topics_PT, file = paste0(root_path, "R_results/research_topics_PT"))


