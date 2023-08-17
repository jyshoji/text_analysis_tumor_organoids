# Summary

This folder contains .csv files that were generated by/for R for the meta-analysis of organoids/organ-on-a-chip publications.

# Files and folders

## temps/
The folder was used by R to store csv files which can be manually checked for the quality-control purpose.

## PubMed_Timeline_Results_by_Year.csv

The number of research articles in the tumor research area between 2011 and 2022 on PubMed. The file was used in *./R/corpus_tumor.R* in order to compare the growth of publication counts in tumor organoid research and tumor research as a whole. The identical data was used for our previous analysis on non-tumor models. The publication search was performed with the search condition shown below. The result was saved by clicking "download csv" on top of the histogram at the top-left corner of the PubMed page.

### Search condition

1. "tumor" or "cancer" as Text Word
2. "English" as language
3. "2011 - " as publication date range.
4. "journal article" but not "comment", "review", "retracted publication", or "retraction of publication" as document types.

### Search strings

Search: ((((tumor[Text Word] OR cancer[Text Word]) AND ("english"[Language])) AND (("2011/01/01"[Date - Publication] : "3000"[Date - Publication]))) AND ("journal article"[Publication Type])) NOT ("retracted publication"[Publication Type] OR "retraction of publication"[Publication Type] OR "review"[Publication Type] OR "comment"[Publication Type])

## README_csv.md

This file.

## ToC_edge_lvl3_nudge_F.csv

Used in *./R/fig_organ_classifications_tumor.R* to manually adjust positions of text labels in a graph. Columns contain organ names, x-axis position adjustments, and y-axis position adjustments.

## all_countries_F.csv

A list of countries that were detected in the address field of the corpus, along with the populations. The population data was obtained from wikipedia (https://en.wikipedia.org/wiki/List_of_countries_and_dependencies_by_population). The file was used to generate world research maps on *./R/fig_global_trends_tumor.R*.

## category_names_T.csv

A conversion table for research topics, used for conversions among column names, research topics (in writing styles to be used as figure labels), and topic groups. It also contains occurrences of each research topic, filters showing which reserach topics to plot depending on figure type, and numbers indicating which subsets of the figures the research topics belong to, which may be used when a large multiple-page figure is generated. The file was used in *./R/fig_model_comparison_tumor.R*.

## edge_all.csv

An edge list for hierarchical organ classification. The first column (*from*) represents a higher category, with a lower category in the second column (*to*). For example, a branch of the hierarchical classification *gastrointestinal* - *intestine* - *colon* is expressed as *gastrointestinal* - *intestine* in one row, and *intestine* - *colon* in another. The file also includes *major_organ* and *tumor_group* columns, which show the highest organ category (the 1st-level organ category mostly matching the organ system level category), and the tumor classification category used in tumor organoid analysis, respectively, of the corresponding organ category listed in the *to* column. The file was used for hierarchical organ classification in *./R/tumor_types.R* and for circular packing graphs and network graphs in *./R/fig_organ_classifications_tumor.R*, among others.

## pre_dev_F.csv

A list of terms that occurred before "development", along with the column *include* to show whether the word is considered for "organ_development". Used in *./R/research_topics_tumor.R*.

## pre_words_F.csv

A list of terms that occurred before "organoid" or "onchip", along with the column *include* which shows whether the term should be considered as organ name equivalents. Values in the *include* column are either "y", "n", or "c", which respectively means "include as an organ name", "not to include", or "to have a look at the word before" which in some cased led to detection of two-words organ names. Used in *./R/tumor_types.R* to capture organ types of models used in research.

## rare_researched_tumor_papers_F.csv
A subcorpus of publications that study infrequently researched tumor models. The file is based on *./csv/temps/rare_researched_tumor_papers.csv*, which was manually checked, adjusted, and saved as this file. Used in *./R/tumor_types.R* to manually correct the organ type classification. 

## tumor_conversion.csv
A conversion table to assign tumor subgroups to tumor groups. The "to" column shows tumor subgroups, "major_organ" and "tumor_group" shows the organ-system-level categories and tumor groups, respectively, that the tumor subgroups belong to. "tumor_sub" shows proper writing styles of the subgroups which was used for figure labels. The file was used in *./R/fig_model_comparison_tumor.R*.

## tumor_detected_F.csv

A list of potential oncological terms, with the column *include* that shows whether or not to include the term for consideration as "tumor" for research topics. Used in *./R/research_topics_tumor.R*. 

## tumor_edge_lvl3_nudge_F.csv
Similar to *ToC_edge_lvl3_nudge_F.csv*, but used for tumor organoid models.

## tumor_terms.csv
A table of all identified oncological terms. The column "tumor" shows oncological terms, and "include" shows names of the organs that the oncological terms correspond to. The value in "include" is "ns" where the oncological terms are not organ-specific. Used in *./R/tumor_types.R* to identify tumor organ models based on oncological terms. 

## unclassified_conditions_F.csv
Showing oncological terms in clinical trials that the algorithm failed to pick up. The file is based on *./csv/temps/unclassified_conditions.csv*, which was manually checked and saved as this file. Used in *./R/clinical_trials.R* to manually adjust organ classifications. 