##########
###
### Installing R and RStudio
###
##########

### RStudio provides graphical user interface for coding in R.
###
### You can download R and RStudio from below.
### https://posit.co/download/rstudio-desktop/




##########
###
### Installing a package
###
##########

### After installing R and RStudio, open this file on RStudio.
###
### Then, install the package "tidyverse" by running the line of code below.
install.packages("tidyverse")



##########
###
### Making a root folder for the analysis.
###
##########

### You can make a root folder at your desired location. 
### 
### For example, if you use a mac, you may make a folder "tumor_meta_analysis" in your home directory 
### (i.e., /Users/*username*/tumor_meta_analysis).
### If you use a Windows, you may make a folder "tumor_meta_analysis" folder on the C drive.
###
### Then, make the folders "R_results" and "R" in your root folder. 
### Download and save the ./R_results/research_topics_PT, and ./R_results/countries_PT of the repository 
### in the R_results folder.
### Download and save the ./R/investigating.R in the R folder.



##########
###
### Loading the data file and the package.
###
########## 

### Setting the file path to your root folder.
### If your root folder is ~/tumor_meta_analysis/, run the below.
root_path <- "~/tumor_meta_analysis/"

### If you use Windows, and your root folder is C:\\tumor_meta_analysis\\, run the below instead (after removing ### at the start of the line).
### root_path <- "C:\\tumor_meta_analysis\\"

### Loading the research_topics_PT and countries_PT .
### If you use Windows, replace "/" below with "\\".
load(paste0(root_path, "R_results/research_topics_PT"))
load(paste0(root_path, "R_results/countries_PT"))

### Loading the required package.
library(tidyverse)

### To make things easier, insert the main_country column of the countries_PT into the research_topics_PT, 
### so that you only have to deal with one data object.
all_classifications <- research_topics_PT %>% 
  mutate(main_country = countries_PT$main_country)


##########
###
### Subsetting the data
###
########## 

### The data can be subset by column values.
### To do this, you will first want to known what column values to choose.
### See ./README_investigating.md to see what columns mean.
### You can also check the column names as below.
colnames(all_classifications)

### Showing the values in the column "type".
unique(all_classifications$type)
### This basically means showing unique values (i.e., deduplicated values) of the column "type" of the data object "all_classifications".
### This will give you:
### [1] "Preprint"         "Research article" "Review"     
### which means that the column contains three values, "Preprint", "Research article", and "Review".

### Selecting research articles.
research_articles <- all_classifications %>% 
  filter(type == "Research article")
### The pipe operator %>% basically means "use the former as an input for the latter".
### So, the above lines mean "making a data object "research_articles" by applying the second line of the code to the data object "all_classifications".
### The second line of the code means "filter the data object to select rows where the values in the "type" column is "Research article".
### Note that you need to write the value ("Research article" here) exactly as it appeared following "unique(all_classifications$type)" above.

### Taking a look at the TF_precision_medicine column.
unique(all_classifications$TF_precision_medicine)
### This will give: 
### [1] FALSE  TRUE
### which means the column value is either FALSE or TRUE.

### Collectively, you can choose research articles that mention precision medicine as below.
precision_medicine <- all_classifications %>% 
  filter(type == "Research article") %>% 
  filter(TF_precision_medicine == TRUE)

### Checking the titles of the selected publications.
precision_medicine[, "title"]
### This means "subset the data object precision_medicine by choosing the column "title".
### Similarly, you can see the DOIs of the selected publications.
precision_medicine[, "doi"]
### You can also select rows of the data object, for example to show the first 10 publications.
precision_medicine[1:10, "doi"]

### Overall, through selecting combinations of column values, you can subset the data to find publications of your interest.




##########
###
### Counting the number of publications in a subset of the corpus.
###
##########

### You can for example count the number of papers in the subset of the corpus.
### The below lines show the countries that performed research on precision medicine using neural tumor organoids, 
### along with the number of research articles.
PM_countries <- all_classifications %>% 
  ### Subsetting the corpus
  filter(type == "Research article") %>% 
  filter(TF_precision_medicine == TRUE) %>% 
  filter(corpus_F == "tumor_organoid") %>% 
  filter(tumor_organ_major == "neural") %>% 
  ### Counting the numbers of occurrences of each value in the main_country column.
  count(main_country, sort = TRUE)

### Below lines show the numbers of review publications in each year of gastrointestinal ToC models.
gastrointestinal_ToC_year <- all_classifications %>% 
  filter(type == "Review") %>% 
  filter(corpus_F == "ToC") %>% 
  filter(tumor_organ_major == "gastrointestinal") %>% 
  ### Counting the numbers of occurrences of each value in the year column.
  count(year)





##########
###
### Some more useful tips.
###
##########

### Exclusion
### You can use ! to select a complement.
### For example, below will exclude reviews, instead of selecting them.
t3 <- all_classifications %>% 
  filter(!type == "Review")

### Temporarily disable a line of code.
### 
### If you use #, R does not run the code after the symbol in the same line.
### For example, below, the line for filtering based on the year is ignored. 
t4 <- all_classifications %>% 
  filter(type == "Research article") %>% 
  #filter(phase == "later") %>% 
  filter(corpus_F == "tumor_organoid") %>% 
  filter(tumor_organ_major == "urinary") %>% 
  filter(TF_patient_derived == TRUE)



### Filtering with multiple values
### If you want to filter based on more than one values, for example to select papers on either neural or ocular tumor organoids, 
### below will do it.
t5 <- all_classifications %>% 
  filter(tumor_organ_major %in% c("neural", "ocular"))
### Also, if you want to see papers talking about either immunotherapy or chemotherapy, below will do it.
t6 <- all_classifications %>% 
  filter(TF_immunotherapy == TRUE | TF_chemotherapy == TRUE)
### Above, "|" means "or".


### Selecting papers that mention certain term.
###
### You can for example select papers that mention the term "liver" in the title.
t7 <- all_classifications %>% 
  filter(grepl("\\blivers?\\b", title, ignore.case = TRUE) == TRUE)
### Above, the second line look for the character sting "liver" in the "title" column.
### "ignore.case = TRUE" means case-insensitive search.
### "\\b" is word boundary, and "?" means that the character immediately before is optional.
### So it selects papers that mention either "liver" or "livers", but exclude "delivery" for example.

