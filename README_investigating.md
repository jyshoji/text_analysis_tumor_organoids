# Summary

This file contains basic information on how to investigate the R data files. It is recommended to use *./R_results/research_topics_P* to investigate the data as this R data file contains most of the important classifications that were assigned through the analysis. This file has a tabular structure, with each row containing one publication and columns showing various classifications. The file can be used to find publications that meet certain criteria (e.g., finding publications using cerebral models in human, published after 2021), by subsetting the data by column values. The section below shows what each column of the data represents. 

In order to investigate the data, you will want to download *./R_results/research_topics_PT* and *./R_results/countries_PT* as data files and *./R/investigating.R* for code and instructions. To download them, click the names of corresponding files at the repository, and click "download" on the right side of the browser window. 

For more instructions and examples of code, see *./R/investigating.R*, which can be opened on RStudio, as well as on text editors.

# Column names

Below shows column names of the *./R_results/research_topics_PT* and what these columns represent. Some columns are probably not useful for investigating the data. For example, columns 10 - 45 (from **tumor_term_combined** to **metastasis_organ_8**)
 show preliminary classifications that were used for final classifications that are shown in the columns 46 - 49 (**researched_tumor_type** to **metastasis_organ_type**). Columns 60 - 192 (**TF_coronavirus** to **TF_Toxoplasma**) show diseases, which are not very relevant to tumor models but we identified nevertheless to be consistent with our previous analysis on non-tumor models.

## ID
Unique identification numbers assigned in the analysis to each of the documents.

## author
List of authors of the publication.

## title
Title of the publication.

## type
Article type, either "Review", "Research article", or "Preprint".

## year
Year of publication.

## doi
DOI

## corpus
Either "organoid" or "OoC", showing if the copy of the document is on organoid or OoC technology.

## phase
Either "early" or "later", showing if the publication is from the early (2011 - 2019) or later (2020 - ) period.

## corpus_F
Either "tumor_organoid" or "ToC", showing if the copy of the document is from the tumor organoid corpus or ToC corpus.

## tumor_term_combined
Containing all identified oncological terms, which are separated by ";".

## organ_from_tumor
Containing names of organs that were identified based on the above "tumor_term_combined."

## researched_tumor_*
These columns show tumor types that are mentioned in the document. Where * is "major", tumor types are shown as organ-system-level categories. Where * is "all", all tumor types are shown, separated by ";". Other columns contain one tumor type per column.

## tumor_organ_* 
These columns show the tumorous organ models (tumor organoid models, if the **corpus_F** column of the document has the value "tumor_organoid"; ToC models when the value is "ToC") that were computationally determined. The column suffix ("major", "all", "1", etc) means the same as above. 

## NT_organ_*
These columns show the non-tumorous organ models.

## metastasis_organ_*
These columns show the metastatic sites (if any) that were computationally determined. However, this computational classification has the accuracy rate of ~60% which was not satisfactory high, and thus the data was not used in the paper. 

## researched_tumor_type 
This column shows the summary classification of tumor types that are mentioned in the document.

## tumor_organ_type
This column shows the summary classification of tumorous organ models. 

## NT_organ_type
This column shows the summary classification of non-tumorous organ models. 

## metastasis_organ_type
This column shows the summary classification of metastatic sites. 

## from_OR*
Used to introduce a hierarchy in the classified organ model categories.

## researched_tumor_other_*
Showing tumor subtypes that are mentioned in the document. 


## TF_ columns (from the column 60 TF_coronavirus to 192 TF_Toxoplasma)
The values are either TRUE or FALSE. These columns show disease names and related terms that were identified to be consistent with our previous analysis on non-tumor organoid and OoC models. These terms are not very relevant to tumors, and were not included in our tumor organoid paper. 

## TF_columns (from the column 193 TF_organ_development to 369 TF_FACS)
Either TRUE or FALSE, showing whether the publication mentions the corresponding terms. From these columns, we chose the most important terms to plot in figures. 

