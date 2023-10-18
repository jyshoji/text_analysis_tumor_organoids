# Summary

This folder contains R script files which are to be opened and run on RStudio.

# Session info.
> sessionInfo()

R version 4.0.5 (2021-03-31)
Platform: x86_64-apple-darwin17.0 (64-bit)
Running under: macOS 12.6.8

Matrix products: default
LAPACK: /Library/Frameworks/R.framework/Versions/4.0/Resources/lib/libRlapack.dylib

locale:
[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

attached base packages:
[1] stats     graphics 
[3] grDevices utils    
[5] datasets  methods  
[7] base  

# Files

## README_R.md

This file.

## accuracy_tests_tumor.R

Containing code used for determining accuracy of computational classifications as compared to manual classifications.

## capturing_topics_tumor.R

Code used for capturing research topics. Key research topics were identified by this code, and were captured in the *./R/research_topics_tumor.R*. 

## clinical_trials.R
Code used for identifying tumor organ models in clinical trials. The end result was saved as *./R_results/clinical_trials_P* and used in *./R/fig_organ_types_tumor.R*.

## corpus_tumor.R

Code used for comparing increases of academic publications in tumor organoid research and tumor research as a whole. 

## fig_global_trends_tumor.R

Code used for drawing graphs of global research trends. The code uses *./R_results/tumor_types_P*, and *./R_results/countries_PT* as inputs, and includes following steps.
1. Identifying well-researched major organ types in research articles of each corpus.
2. Calculating the number and trends of publications.
3. Making a table showing countries' contributions
4. Drawing pie charts showing researched organ types in top research countries.
5. Drawing world maps showing fractional contribution counts of countries.

The plotted graphs were saved at *./results/global_trends/*

## fig_model_comparison_tumor.R
Code used for drawing graphs of research topics in tumor organoid and ToC research. The code uses *./R_results/research_topics_PT* as an input, and includes following steps.
1. Loading and modifying required data
2. Plotting graphs showing trends of research topic occurrences
3. Comparison of research topics between tumor organoids and ToC.
4. Comparing frequency of research topic occurrences among tumor groups.

The plotted graphs were saved at *./results/model_comparison/*.

## fig_organ_classifications_tumor.R
Code used for drawing graphs of organ classifications. The code uses *./R_results/tumor_types_P* as an input, and includes following steps.
1. Preparing for circular tree maps and network graphs.
2. Plotting Circular packing graphs
3. Plotting network graph of hierarchical organ classification

The plotted graphs were saved at *./results/organ_classifications/*.

## fig_organ_types_tumor.R

Code used for drawing a graph showing comparison of tumor organoid models, ToC models, models in clinical trials, global tumor incidence and mortality. The code uses *./R_results/tumor_types_P*, and *./R_results/cliical_trials_P* as inputs.

The plotted graphs were saved at *./results/organ_types/*.

## investigating.R

Code and instructions on how to identify academic publications on specific topics using *./R_results/research_topics_PT*. Not needed for the analysis, but instead shows how to use the generated data. 

## metastasis_links.R
Code for identifying combinations of primary and metastatic cancers that are modelled as tumor organoids/ToC.

## research_topics_tumor.R
Code used for identifying research topics. The code uses *./R_results/tumor_types_F* as an input, and includes following steps.
1. Identifying tumor subgroups in publications.
2. Preparing lists of research topics to capture
3. Capturing research topics.
4. Making a summary data frame of research topics and other research themes.

The result of the analysis was saved as: *./R_results/research_topics_T*, and *./R_results/research_topics_PT*.

## tumor_types.R

Code used for identifying organs/substructures modelled as tumor organoids/ToC. The code uses a file generated in our previous analysis on non-tumor organoids/OoC, which is not stored at the repository due to the copyright issue. See the *./README_reproduction.md* for more details. This R script file includes following steps.
1. Uniting two-word organ names.
2. Extracting key phrases where organ names will be identified in order to determine researched tumor types.
3. Extracting key phrases for metastasis
4. Listing words to capture.
5. Identifying organ types of researched tumors.
6. Identifying tumor models of organs.
7. Making the tumor organ classification.

The result of the code was saved as *./R_results/tumor_types_F*, and *./R_results/tumor_types_P*.



