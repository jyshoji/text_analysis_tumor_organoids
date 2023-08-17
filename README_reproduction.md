# Summary

This file contains instruction on how to reproduce the meta-analysis of tumor oranoid and tumor-on-chip publications. The analysis consists of two parts: classification of academic publications and graphical visualization. The visualization part can be readily reproduced using the files in the repository, using R script files named as *./R/fig_*. See the R script files for more information. However, it is tricky to reproduce the classification part, since we are only allowed to share metadata of open-access publications. Below, we offer a couple of possible approaches for reproducing the analysis (see the "Reproduction" section). In addition, guides are provided on how we manually adjusted the data (see the "manual adjustments of the data"). 


# Reproduction

We consider three potential approaches for reproducing the analysis.

1. Full reproduction, using our corpus

- Please contact the lead contact, Stefan Krauss (s.j.k.krauss@medisin.uio.no), and we will provide you with our corpus.

2. Full reproduction, without using our corpus

- This approach requires readers to obtain publication metadata from EMBASE, PubMed, Scopus, Web of Science and/or bioRxiv to collect metadata of academic publications. 
- Since this analysis follows our previous meta-analysis on (non-tumor) organoids / organ-on-chip (Shoji et al., 2023), one will need to run code in another repository (https://github.com/jyshoji/text_analysis_organoids) to start with. Specifically, *./R/formatting.R*, *./R/organ_types.R*, and *./R/country_F.R* will be used to generate the *./R_results/organ_types_F* and *./R_results/countries_F* files, which are used as input files for this analysis. 
- It can be time-consuming, and may take a day or two just to collect the required metadata.
- This approach will allow almost full reproduction of the analysis. However, due to how literature databases handle publication dates before and after the actual print date, you may have slightly more/less publications compared to our corpus.
- Depending on combinations of databases that publications are retrieved from, the code may need adjustments. 
- You may also need to perform manual adjustments and quality checking of the data as we did. When an R script file includes a manual adjustment step, follow the instruction there.

3. Partial reproduction.

- The repository includes files containing metadata of open-access academic publications along with their organ model and country classifications  (*./R_results/organ_types_CC*, and *./R_results/countries_CC*, respectively). These files contain about one-third of academic publication records that we used for the analysis, and can be used for partial reproduction.
- See the next section **Easy-start guide** for details.

# Easy-start guide

This section provides instruction on an easy start of partial reproduction of the analysis. This approach skips all the manual adjustments and quality checking. If you want to perform manual adjustments as well, seen the next section "Manual adjustments of the data" as well.

1. Make a root folder of the analysis.

- You can make a root folder at your desired location. Once you set the right file path in R script files, the code should work without problems. 
- For example, if you use a mac, you may make a folder "tumor_analysis" in your home directory (i.e., /Users/*username*/tumor_analysis)
- If you use a Windows, you may make a folder "tumor_analysis" folder on the C drive. You will also need to replace all occurrences of "/" in file paths in R scropt files with double backslash.

2. Storing required files in the above root folder.

- Download all the folders and files of the repository, and save them in your root folder.
- The easiest way to download them may be to go to the repository, click the green button "Code" in the upper part of the page, slightly to the right, and choose "Download ZIP". This will download the entire repository. After downloading, unzip it if necessary. You may then move the entire folder to where you want to make the root folder (e.g., ~/), rename it (e.g., tumor_analysis), and use it as the root folder. 
- Then, make *./R_temps/* folder in your root folder. This folder is used to save intermediate R data files which can be used for quality control purposes.
- Note that you do not actually need all the files to run the code. If you want to avoid storing unnecessary files, just keep the files and folders listed below. 
- *./R/* folder and all its contents.
- *./R_results/* folder, as well as *organ_types_CC* and *countries_CC* files therein. Other files are not necessary. 
- *./clinical_trials/* folder and csv files therein.
- *./csv2/* folder and its contents. Delete the *./csv/* folder, and rename the *csv2* folder to *csv*. 
- *./results2/* and all its contents. Delete the *./results/* folder and rename the *results2* folder as *results*.  

3. Installing packages

- If you use RStudio, go to "tools" in the menu bar, choose "install packages". Write down a name of a package in the "packages" box.
- You will need "ggpp", "ggraph", "igraph", "rworldmap", "tidytext", and "tidyverse".
- Alternatively, you can run the following lines on RStudio to install these packages.
install.packages("ggpp")
install.packages("ggraph")
install.packages("igraph")
install.packages("rworldmap")
install.packages("tidytext")
install.packages("tidyverse")

4. Setting file paths in each R script file.

- Follow the instruction in R script files to set the file path, so that R can save at and load from your root folder.
- You will essentially only need to change one line of code per an R script file where the root path is set, except for some of the R script files starting with *fig_* where root path may be set multiple times to facilitate our quality control. 
- For example, if your root folder is *~/tumor_analysis/*, change the line:
- root_path <- "~/Research_data/Hybrida/tumor_analysis/"
- into 
- root_path <- "~/tumor_analysis/"

5. Running the code in R script files.

- You will first need to run the code in *./R/tumor_types.R*, and then *./R/research_topics_tumor.R*. After that, you may run the code in remaining R script files in any order.
- *./R/accuracy_tests_tumor.R*, and *./R/capturing_topics_tumor.R*, and *./R/investigating.R* are for quality cotrol, for identifying research topics to capture, and for investigating the data, respectively, and are not needed for generating graphical figures.
- Once you change the line of code for setting file path (as described in 4. Setting file paths in each R script file), the code should work without further changes except below; 
- In *./R/fig_organ_classifications_tumor.R*, change all occurrences of; 
- **position_adjustment = TRUE** to 
- **position_adjustment = FALSE** 
- and delete the lines; 
- **tumor_nudge <- read.csv(paste0(root_path, "csv/tumor_edge_lvl3_nudge_F.csv"))** 
- **ToC_nudge <- read.csv(paste0(root_path, "csv/ToC_edge_lvl3_nudge_F.csv"))**
- In addition, you may get an errot saying: 
- "Error in `palette()`:! Insufficient values in manual scale. 10 needed but only 9 provided."
- This is because the code chooses the top seven organ systems with the highest publication counts to plot, but the actual number of plotted organ systems can be higher if there is more than one top seventh organ system with the identical number of publications. When this is the case you will have to assign another color for the extra organ system, for example by changing the line: 
- scale_fill_manual(values = c("orange", "red", "yellowgreen", "blue", "magenta", "purple", "pink", "grey75", "grey50"))
- to: 
- scale_fill_manual(values = c("orange", "red", "yellowgreen", "blue", "magenta", "purple", "pink", "cyan", "grey75", "grey50"))


# Manual adjustments of the data

In some places, we saved intermediate data as csv files, manually adjusted them, and used them to run the subsequent code. The manual adjustments were performed to for example identify duplicate academic publications in the corpus, or to correct algorithm-based classifications. We typically saved such intermediate data files (called "output csv files" below) in *./csv/temps/*, and saved the manually adjusted data files (called "input csv files" below) in *./csv/* which was then loaded onto R. 

## How to handle manual adjustment steps

As manual adjustments of the data can be time-consuming, we assume that readers may want to skip these steps. We consider the following three options for how readers may deal with manual adjustments. 

1. Perform manual adjustments

In this case, the code can be run as is. However, readers will have to manually adjust the output csv files and save them as input csv files in */csv/*, typically with "_F" added to the end of the file names. 

2. Skip the manual adjustments

The R script files include comments saying that manual adjustments will be performed, along with quick instruction on how to skip that part. Readers may just follow the instruction. Note that this will require small changes to the code.

3. Use the dummy csv files.

Readers may use dummy csv files as input csv files. These files will not do anything, and the code will continue with no manual adjustments being made. To take this approach, copy the files in the *./csv2/* folder and save them in the *./csv/*, replacing any existing csv files. The advantage of this approach is that the code can be run as is, but the downside is that it still performs some computations prior to manual adjustments, and therefore may take longer than the above approach 2. In addition, you will have to make following changes; 
- In *./R/fig_organ_classifications_tumor.R*, change all occurrences of; 
- **position_adjustment = TRUE** to 
- **position_adjustment = FALSE** 
- and delete the lines; 
- **tumor_nudge <- read.csv(paste0(root_path, "csv/tumor_edge_lvl3_nudge_F.csv"))** 
- **ToC_nudge <- read.csv(paste0(root_path, "csv/ToC_edge_lvl3_nudge_F.csv"))**





