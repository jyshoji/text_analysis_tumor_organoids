# Summary

This folder contains subfolders to save graphical figures or .csv files generated through the analysis. 

# Folders and files within

## clinical_trials

Containing a figure showing the number of clinical trials per year.

## csv

Containing tables in the .csv formats sumamrizing numerical data.

### countries_contributions.csv

Showing the number and proportions of research articles by countries. Generated in *./R/fig_global_trends_tumor.R*.

### counts_per_capita.csv

Showing the number of research articles by countries per million people. Generated in *./R/fig_global_trends_tumor.R*.

### tumor_comparison.csv

Showing the numbers and percentages of research articles on different tumor models, along with global incidence and mortality of corresponding tumors in percentage. Generated in *./R/corpus_tumor.R*.

## global_trends

Contains figures showing global research trends. Generated in *./R/fig_global_trends_tumor.R*.

### pie_charts/

A subfolder containing pie charts that show proportions of research articles on tumor models in top research countries. Drawn separately for each of the tumor organoid and ToC corpora.

### world_maps/

A subfolder containing world maps that show adjusted fractional contributions of each country. Drawn separately for each corpus, with or without legends.

## model_comparison

Figures for comparing organoid and OoC model systems. Generated in *./R/fig_model_comparison_tumor.R*.

### topic_freq_TO.pdf, topic_freq_ToC.pdf

Showing frequency of research topic occurrence (high in red and low in blue) in tumor model groups in tumor organoid and ToC research. 

### topic_model_point.pdf

Showing model system preference (tumor organoids vs. ToC) in research topics. 

### topic_trends_bubble_TO.pdf, topic_trends_bubble_ToC.pdf

Showing research topic trends in tumor model groups in tumor orgaoid and ToC research.

## organ_classifications

Circular packing graphs and dendrograms of tumor model classifications. Generated in *./R/fig_organ_classifications_tumor.R*.

### ToC_bubble.pdf.png
### tumor_bubble.pdf

Circular packing graphs showing two-level organ classifications.

### ToC_network.png
### tumor_network.png

Dedrograms showing hierarchical organ classifications. 

## organ_types

Showing comparison among tumor models in tumor organoid research, ToC research, clinical trials, global incidence and mortality. Generated in *./R/corpus_tumor.R*. 

## README_results.md

This file.

