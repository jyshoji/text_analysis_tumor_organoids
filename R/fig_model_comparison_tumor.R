### This R script file is to draw graphs showing research topics researched with tumor organoids or ToC.
###
### The script include the following steps.
### 1. Loading and modifying required data
### 2. Plotting graphs showing trends of research topic occurrences
### 3. Comparison of research topics between tumor organoids and ToC.
### 4. Comparing frequency of research topic occurrences among tumor groups.
### 
### The figures were saved at: 
### ./results/model_comparison/






##########
###
### 1. Loading and modifying required data
###
##########

### The graph will show tumor types in X-axis, grouped by 1st-level organ categories, and research topics in Y-axis, 
### grouped by topic categories.
### However, there are too many tumor types to plot in a one-page figure.
### Therefore, first, tumor types to be plotted are selected, and they are appropriately grouped so that they appear 
### in the graph in order.

### Loading a package and required files.
library(tidyverse)

### Setting the path to the root folder.
root_path <- "~/Research_data/Hybrida/tumor_analysis/"
### Change the above according to your root folder location.
### If you use Windows, change all occurrences of "/" in file paths to "\\".

### Loading an R object file of research topics.
load(paste0(root_path, "R_results/research_topics_T"))
### If you want to reproduce the figures, change the above line to the following two lines. 
### load(paste0(root_path, "R_results/research_topics_PT"))
### research_topics_T <- research_topics_PT

### Loading the csv file of research topic categories.
### The file includes research topic writing styles to be used for figure labels, names of research topics group categories, and 
### filter of which research topics to plot.
category_names_T <- read.csv(paste0(root_path, "csv/category_names_T.csv")) %>% 
  mutate(category_renamed = gsub("^dash\\b", "--", category_renamed))

### extracting category names (to be used as figure labels), and converting them to factors to properly order them.
category_level <- factor(category_names_T$category_renamed)

### Extracting category group names and converting them to factors.
category_group_level <- unique(category_names_T$group_name)

### Loading the edge list
### The file shows organ categories ("to") along with their corresponding upper organ categories ("from" column), 
### the 1st-level organ categories ("major_organ), tumor groups ("tumor_group").
### 
### The organ category of a publication will be determined based on the "to" column, and be converted to a tumor type 
### based on the "tumor_group", which will be then grouped according to the "major_organ" column showing the 1st-level organ categories.
edge_all <- read.csv(paste0(root_path, "csv/edge_all.csv"))

### Loading a data frame of additional tumor subgroups, such as triple-negative breast cancer. 
### The data frame contains the names of tumor subgroups as they appear in the text field ("to" column), 
### the 1st-level organ categories they belong to ("major_organ"), the tumor type they belong to ("tumor_group"), 
### and the names of the tumor subgroups as they are used in figure labels ("tumor_sub").
tumor_conversion <- read.csv(paste0(root_path, "csv/tumor_conversion.csv")) %>% 
  mutate(tumor_sub = gsub("^", "- ", tumor_sub))

### Combining the edge list and tumor conversion data frame.
### The resulting data frame will contain all the tumor types to be considered, and will be used as a conversion table 
### between tumor types and the groups they belong to.
tumor_conversion_all <- edge_all %>% 
  ### Removing organ categories without corresponding tumor group.
  filter(!is.na(tumor_group)) %>% 
  ### Removing the column that's not needed.
  select(!from) %>% 
  ### Making a new column to be consistent with the tumor conversion data frame that is to be added.
  mutate(tumor_sub = tumor_group) %>% 
  ### Adding the tumor conversion data frame. 
  rbind(., tumor_conversion) 

colnames(research_topics_T)






##########
###
### 2. Plotting graphs showing trends of research topic occurrences
###
##########

### Making a custom function for calculating trends of publications.
### This custom function is identical to the one used in other R script files in the analysis.
fn_calculate_trends <- function(publication_count_data, output_prefix = "") {
  trend_data <- publication_count_data %>% 
    pivot_wider(names_from = phase, values_from = n) %>% 
    mutate(across(c("early", "later"), ~ replace_na(., 0))) %>% 
    mutate(total = later + early, 
           trend = later / early) %>% 
    rename_with(~ gsub("^", paste0(output_prefix, "_"), .), .cols = c("early", "later", "total", "trend"))
}


### Calculating publication trends of each publication type.
type_trends <- research_topics_T %>% 
  group_by(type, corpus_F) %>% 
  count(phase) %>% 
  ungroup() %>% 
  fn_calculate_trends(., output_prefix = "group")

### Calculating trends of topic occurrence by tumor types, separately for tumor organoids and ToC, and also for each publication type.
corpus_topic_trends <- research_topics_T %>% 
  ### Making new columns to be consistent with a later data frame to be added. 
  mutate(upper_group = "Total", 
         tumor_sub2 = "all tumor") %>% 
  ### All columns of research topics (starting with "TF_") are changed to a longer format.
  pivot_longer(starts_with("TF_"), names_to = "topic") %>% 
  filter(value == TRUE) %>% 
  ### Counting the number of publications mentioning each topic in combination of phase and tumor types.
  group_by(type, corpus_F, phase, upper_group, tumor_sub2) %>% 
  count(topic) %>% 
  ungroup() %>% 
  ### Calculating trends.
  fn_calculate_trends(., output_prefix = "TF") %>% 
  ### Adding publication trends of tumor groups.
  left_join(type_trends, by = c("type", "corpus_F"))



### Identifying tumor types that are researched in each document.
### For this, converting the data frame of research topics to a longer format.
researched_tumor_longer <- research_topics_T %>% 
  ### NA in tumor_organ_1 is changed to "unidentified", so that documents without identified organ types are treated as 
  ### studying unidentified tumor type, rather than being dropped from the corpus.
  mutate(tumor_organ_1 = replace_na(tumor_organ_1, "unidentified")) %>% 
  ### Converting the tumor_organ_1 to _3 and researched_tumor_other_1 to 3 to a longer format.
  ### The former columns contain identified tumor organ types, and the latter columns include identified tumor subgroups.
  pivot_longer(any_of(c(paste0("tumor_organ_", c(1:3)), paste0("researched_tumor_other_", c(1:3)))), 
               values_to = "researched_tumor_total", values_drop_na = TRUE) %>% 
  ### Adding the previously made conversion table.
  left_join(., tumor_conversion_all, by = c("researched_tumor_total" = "to")) %>% 
  select(!name) 

### Important tumor types are identified below, so that they are plotted in the later graphs.
### Counting the number of research articles on each tumor type.
common_tumor_groups <- researched_tumor_longer %>% 
  filter(type == "Research article") %>% 
  select(ID, tumor_sub) %>% 
  distinct() %>% 
  count(tumor_sub, sort = TRUE) %>% 
  ### For the time being, tumor types with more than 5 publications are considered important.
  filter(n > 5) 

### Tumor type categories are adjusted below.
### Basically, important tumor type categories are kept as they are, whereas less important categories are combined together 
### as "other" tumor type.
researched_tumor_adjusted <- researched_tumor_longer %>% 
  ### Making a new column with modified tumor categories
  mutate(tumor_sub2 = ifelse(is.na(tumor_sub), NA, 
                             ### "intestine" and "appendix" are integrated into "gastrointestinal". 
                             ### "gallbladder" is integrated into "biliary".
                             ifelse(tumor_sub %in% c("intestine", "appendix"), "gastrointestinal", 
                                    ifelse(tumor_sub == "gallbladder", "biliary", 
                                           ifelse(tumor_sub %in% c(common_tumor_groups$tumor_sub, "gastrointestinal"), tumor_sub, 
                                                  ### Tumor subgroups which are not among common tumor groups are changed into NA.
                                                  ### Other uncommon tumor types are put together as "other" tumor type.
                                                  ifelse(grepl("^- ", tumor_sub), NA, "other")))))) %>% 
  ### Dropping rows corresponding to uncommon tumor subgroups.
  ### Note that documents on these subgroups are included/counted according to their tumor groups, 
  ### so they would be counted twice without dropping these rows.
  filter(!is.na(tumor_sub2)) %>% 
  ### If the tumor type is "other", then tumor_group and major_organ columns are also changed to "other".
  mutate(tumor_group = ifelse(tumor_sub2 == "other", "other", tumor_group)) %>% 
  mutate(major_organ = ifelse(tumor_sub2 == "other", "other", major_organ)) %>% 
  ### In the later graphs, tumor types will be grouped under the 1st-level organ categories.
  ### However, if there is only one tumor type that belongs to a 1st-level organ category, the tumor type is put under the "other" 
  ### 1st-level category.
  ### For this purpose, the number of tumor types under each 1st-level organ category are counted.  
  group_by(major_organ) %>% 
  mutate(n_group = length(unique(tumor_sub2))) %>% 
  ungroup() %>% 
  ### If there is only one tumor type under a 1st-level organ category, the tumor type is grouped under "other".
  ### If there are more than one tumor type, the tumor type with the same name as the corresponding 1st-level category will be labelled as 
  ### "other" + the name of the 1st-level category. 
  ### For example, "gastrointestinal" tumor type will be shown as "other gastrointestinal".
  ### Mammary category has one tumor group and a few tumor subgroups, and will be labelled without "other".
  mutate(tumor_sub2 = ifelse(tumor_sub2 == "mammary", tumor_sub2, 
                             ifelse(tumor_sub2 == major_organ & n_group > 1, paste0("other ", major_organ), tumor_sub2))) %>% 
  ### Making a column showing the 1st-level organ group of the tumor groups.
  mutate(upper_group = ifelse(n_group > 1, major_organ, "other")) %>% 
  select(!n_group)

### Tumor groups will be appropriately ordered below so that they appear in the graphs in an organized manner.
### For this, from the researched_tumor_adjusted data frame, only selecting tumor group columns, along with upper group columns.
tumor_categories <- researched_tumor_adjusted %>% 
  select(tumor_sub2, upper_group, tumor_group) %>% 
  distinct() %>% 
  arrange(tumor_sub2) 

### The order of tumor groups are changed below.
tumor_order <- tumor_categories %>% 
  filter(!upper_group == "other") %>% 
  filter(!grepl("^other ", tumor_sub2)) %>% 
  filter(!grepl("^- ", tumor_sub2)) %>% 
  rbind(., tumor_categories %>% filter(grepl("^- ", tumor_sub2))) %>% 
  arrange(tumor_group) %>% 
  rbind(., tumor_categories %>% filter(grepl("^other ", tumor_sub2)) %>% arrange(tumor_sub2)) %>% 
  arrange(upper_group) %>% 
  rbind(., tumor_categories %>% filter(upper_group == "other" & !tumor_sub2 %in% c("unidentified", "other"))) %>% 
  rbind(., tumor_categories %>% filter(tumor_sub2 == "other")) %>% 
  rbind(., tumor_categories %>% filter(tumor_sub2 == "unidentified"))

### Now that the tumor categories have been adjusted, publication trends of the tumor types are calculated.
group_trends <- researched_tumor_adjusted %>% 
  select(ID, type, corpus_F, phase, tumor_sub2) %>% 
  distinct() %>% 
  group_by(phase, type, corpus_F) %>% 
  count(tumor_sub2) %>% 
  ungroup() %>% 
  fn_calculate_trends(., output_prefix = "group")

### Calculating publication trends of research topics.
topic_trends <- researched_tumor_adjusted %>% 
  ### All columns of research topics (starting with "TF_") are changed to a longer format.
  pivot_longer(starts_with("TF_"), names_to = "topic") %>% 
  filter(value == TRUE) %>% 
  ### Counting the number of publications mentioning each topic in combination of phase and tumor groups.
  group_by(type, corpus_F, phase, upper_group, tumor_sub2) %>% 
  count(topic) %>% 
  ungroup() %>% 
  ### Calculating trends.
  fn_calculate_trends(., output_prefix = "TF") %>% 
  ### Adding publication trends of tumor types
  left_join(group_trends, by = c("type", "corpus_F", "tumor_sub2")) %>% 
  ### Adding the overall trend calculated previously.
  rbind(., corpus_topic_trends) %>% 
  ### Calculating adjusted trends by dividing the topic trends of each combination by tumor group trends.
  mutate(adjusted_trend = TF_trend / group_trend) %>% 
  mutate(adjusted_trend = ifelse(is.nan(adjusted_trend), 1, adjusted_trend)) %>% 
  mutate(adjusted_trend = ifelse(topic == "TF_tumor", group_trend, adjusted_trend)) %>% 
  ### Changing the trends to "high", "moderate", and "low".
  mutate(adjusted_trend2 = ifelse(adjusted_trend > 1.4, "high", 
                                  ifelse(adjusted_trend < 0.6, "low", "moderate"))) %>% 
  ### Adding the category_names_T data frame.
  ### This is to adjust the writing styles and order the research topics.
  left_join(., category_names_T, by = c("topic" = "category_colname")) %>% 
  ### Changing columns into factors.
  mutate(group_name = factor(group_name, levels = category_group_level)) %>% 
  mutate(category_renamed = factor(category_renamed, levels = category_level)) %>% 
  mutate(upper_group = factor(upper_group, levels = c("Total", unique(tumor_order$upper_group)))) %>% 
  mutate(tumor_sub2 = factor(tumor_sub2, levels = c("all tumor", unique(tumor_order$tumor_sub2)))) %>% 
  ### Selecting only research articles.
  filter(type == "Research article") %>% 
  ### Selecting research topics to plot.
  filter(T_filter == "A")

### To properly style the Y-axis label, the max number of characters in the research topic category is determined.
max_width = max(nchar(as.character(topic_trends$category_renamed)))

### Plotting a bubble plot of tumor organoids.
topic_trends_TO <- topic_trends %>% 
  filter(corpus_F == "tumor_organoid") %>% 
  ggplot(aes(x = tumor_sub2, y = fct_rev(category_renamed))) + 
  geom_point(aes(color = adjusted_trend2, size = sqrt(TF_total)), shape = 16) + 
  scale_color_manual(values = c("high" = "orange", "moderate" = "grey65", "low" = "deepskyblue1"), 
                     breaks = c("high", "moderate", "low"), name = "Publication trends") + 
  #labs(color = "Model preference") + 
  #scale_y_discrete(limits = rev) + 
  scale_x_discrete(position = "top") + 
  scale_y_discrete(labels = function(x) sprintf(paste0("%-", max_width, "s"), x)) + 
  scale_size_continuous(range = c(0.3, 3), breaks = c(sqrt(10), sqrt(100)), labels = c(10, 100), name = "Publication counts") + 
  facet_grid(rows = vars(group_name), cols = vars(upper_group), scales = "free", space = "free", switch = "y") + 
  theme(strip.placement = "outside", 
        text = element_text(size = 8), 
        axis.text.x.top = element_text(size = 8, angle = 90, vjust = 0.3, hjust = 0), 
        axis.text.y = element_text(size = 8, hjust = 0, family = "Courier"), 
        strip.text.x.top = element_text(size = 8, face = 2, angle = 90, vjust = 0.3, hjust = 0), 
        strip.text.y.left = element_text(size = 7, face = 2, angle = 360, vjust = 0.5, hjust = 1), 
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"), 
        legend.key.size = unit(4, "mm"), 
        legend.position = c(- 0.40, 1.15), 
        legend.title = element_text(size = 8, face = 2), 
        legend.text = element_text(size = 8), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title = element_blank(), 
        axis.ticks = element_blank())

ggsave(topic_trends_TO, 
       filename = paste0(root_path, "results/model_comparison/topic_trends_bubble_TO.pdf"),  
       width = 178, height = 270, units = "mm")

### Plotting a bubble plot of ToC.
topic_trends_ToC <- topic_trends %>% 
  filter(corpus_F == "ToC") %>% 
  ggplot(aes(x = tumor_sub2, y = fct_rev(category_renamed))) + 
  geom_point(aes(color = adjusted_trend2, size = sqrt(TF_total)), shape = 16) + 
  scale_color_manual(values = c("high" = "orange", "moderate" = "grey65", "low" = "deepskyblue1"), 
                     breaks = c("high", "moderate", "low"), name = "Publication trends") + 
  #labs(color = "Model preference") + 
  #scale_y_discrete(limits = rev) + 
  scale_x_discrete(position = "top") + 
  scale_y_discrete(labels = function(x) sprintf(paste0("%-", max_width, "s"), x)) + 
  scale_size_continuous(range = c(0.3, 3), breaks = c(sqrt(10), sqrt(100)), labels = c(10, 100), name = "Publication counts") + 
  facet_grid(rows = vars(group_name), cols = vars(upper_group), scales = "free", space = "free", switch = "y") + 
  theme(strip.placement = "outside", 
        text = element_text(size = 8), 
        axis.text.x.top = element_text(size = 8, angle = 90, vjust = 0.3, hjust = 0), 
        axis.text.y = element_text(size = 8, hjust = 0, family = "Courier"), 
        strip.text.x.top = element_text(size = 8, face = 2, angle = 90, vjust = 0.3, hjust = 0), 
        strip.text.y.left = element_text(size = 7, face = 2, angle = 360, vjust = 0.5, hjust = 1), 
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"), 
        legend.key.size = unit(4, "mm"), 
        legend.position = c(- 0.4, 1.2), 
        legend.title = element_text(size = 8, face = 2), 
        legend.text = element_text(size = 8), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title = element_blank(), 
        axis.ticks = element_blank())

ggsave(topic_trends_ToC, 
       filename = paste0(root_path, "results/model_comparison/topic_trends_bubble_ToC.pdf"),  
       width = 178, height = 220, units = "mm")












##########
###
### 3. Comparison of research topics between tumor organoids and ToC.
###
##########

### Drawing a graph to compare research topic preference between tumor organoids and ToC.

### Counting the number of publications in each of tumor organoid and ToC corpora that mention research topics across all tumor types.
corpus_topic_counts <- research_topics_T %>% 
  ### Making new columns to be consistent with a later data frame to be added. 
  mutate(upper_group = "Total", 
         tumor_sub2 = "all tumor") %>% 
  ### All columns of research topics (starting with "TF_") are changed to a longer format.
  pivot_longer(starts_with("TF_"), names_to = "topic") %>% 
  filter(value == TRUE) %>% 
  ### Counting the number of publications mentioning each topic in combination of phase and tumor groups, separately for the two corpora.
  group_by(type, upper_group, tumor_sub2, corpus_F) %>% 
  count(topic) %>% 
  ungroup() %>% 
  pivot_wider(names_from = corpus_F, values_from = n) %>% 
  mutate(across(c(5, 6), ~ replace_na(., 0)))

### Calculating the ratio of the number of publications on tumor organoids and ToCs per article type.
### This will be later used to adjust ratio of publications that mention research topics.
corpus_ratio <- research_topics_T %>% 
  ### Counting the number of publications of each article type in each corpus.
  group_by(type) %>% 
  count(corpus_F) %>% 
  ungroup() %>% 
  rename(corpus_count = n) %>% 
  ### Calculating the ratio of publications as "publication counts on organoids" divided by "publication counts on OoCs".
  pivot_wider(names_from = corpus_F, values_from = corpus_count) %>% 
  mutate(corpus_ratio = tumor_organoid / ToC) %>% 
  rename(tumor_organoid_total = tumor_organoid, 
         ToC_total = ToC)

colnames(researched_tumor_adjusted)

### Calculating the ratio of the number of publications between tumor organoid and ToC publications that mention research topics, 
### separately for tumor types.
topic_counts <- researched_tumor_adjusted %>% 
  ### Converting the research topic columns to a longer format.
  pivot_longer(starts_with("TF_"), names_to = "topic") %>% 
  filter(value == TRUE) %>% 
  ### Counting the number of publications mentioning each topic in tumor groups.
  group_by(type, upper_group, tumor_sub2, corpus_F) %>% 
  count(topic) %>% 
  ungroup() %>% 
  ### Transforming the data frame to calculate ratio of publication counts.
  pivot_wider(names_from = corpus_F, values_from = n) %>% 
  ### Adding the data frame of publication ratio across all tumor types.
  rbind(., corpus_topic_counts %>% select(all_of(colnames(.)))) %>% 
  ### Changing NA values to 0.
  mutate(across(c(5, 6), ~ replace_na(., 0))) %>% 
  ### Adding the corpus ratio.
  left_join(., corpus_ratio, by = "type") %>% 
  ### Calculating the ratio of tumor organoid/ToC publications for research topic.
  ### Also calculating the sum of tumor organoid/ToC publications that mention research topics.
  mutate(topic_ratio = tumor_organoid / ToC, 
         topic_total = tumor_organoid + ToC) %>% 
  ### Adjusting the ratios of publication counts on each topic by the ratio of total tumor organoid/ToC publications.
  mutate(adjusted_topic_ratio = topic_ratio / corpus_ratio) %>% 
  ### Setting upper and lower limits of the adjusted ratio to 10 and 0.1, respectively.
  ### Also converting the ratio to a log10 scale.
  mutate(adjusted_topic_ratio2 = ifelse(adjusted_topic_ratio > 10, 1, 
                                        ifelse(adjusted_topic_ratio < 0.1, - 1, log10(adjusted_topic_ratio)))) %>% 
  ### Adding the columns of research topic category names, as well as a topic filter columns showing which topics to plot.
  left_join(., category_names_T, by = c("topic" = "category_colname")) %>% 
  ### Changing columns into factors.
  mutate(group_name = factor(group_name, levels = category_group_level)) %>% 
  mutate(category_renamed = factor(category_renamed, levels = category_level)) %>% 
  mutate(upper_group = factor(upper_group, levels = c("Total", unique(tumor_order$upper_group)))) %>% 
  mutate(tumor_sub2 = factor(tumor_sub2, levels = c("all tumor", unique(tumor_order$tumor_sub2)))) %>% 
  ### Selecting only research articles.
  filter(type == "Research article") %>% 
  ### Selecting research topics to plot.
  filter(T_filter == "A")

### Plotting
topic_model_point <- topic_counts %>% 
  ggplot(aes(x = tumor_sub2, y = category_renamed)) +
  geom_point(aes(color = adjusted_topic_ratio2, size = sqrt(topic_total)), shape = 16) + 
  scale_color_gradient2(low = "red", mid = "grey80", high = "darkblue", midpoint = 0, na.value = "black", 
                        breaks = c(-1, 0, 1), labels = c("ToC", "", "tumor organoid"), name = "Model system preference") + 
  scale_y_discrete(limits = rev, labels = function(x) sprintf(paste0("%-", max_width, "s"), x)) + 
  scale_x_discrete(position = "top") + 
  scale_size_continuous(range = c(0.3, 3), breaks = c(sqrt(10), sqrt(100)), labels = c(10, 100), name = "Publication counts") + 
  facet_grid(rows = vars(group_name), cols = vars(upper_group), scales = "free", space = "free", switch = "y") + 
  theme(strip.placement = "outside", 
        text = element_text(size = 8), 
        axis.text.x.top = element_text(size = 8, angle = 90, vjust = 0.3, hjust = 0), 
        axis.text.y = element_text(size = 8, hjust = 0, family = "Courier"), 
        strip.text.x.top = element_text(size = 8, face = 2, angle = 90, vjust = 0.3, hjust = 0), 
        strip.text.y.left = element_text(size = 7, face = 2, angle = 360, vjust = 0.5, hjust = 1), 
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"), 
        legend.key.size = unit(4, "mm"), 
        legend.position = c(- 0.40, 1.15), 
        legend.title = element_text(size = 8, face = 2), 
        legend.text = element_text(size = 8), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title = element_blank(), 
        axis.ticks = element_blank())

ggsave(topic_model_point, 
       filename = paste0(root_path, "results/model_comparison/topic_model_point.pdf"),  
       width = 178, height = 270, units = "mm")















##########
###
### 4. Comparing frequency of research topic occurrences among tumor groups.
###
########## 

### Calculating research topic occurrences (i.e., percentage of publications that mention a research topic) 
### per article type - corpus combinations.
global_topic_freq <- research_topics_T %>% 
  group_by(type, corpus_F) %>% 
  mutate(corpus_total = n()) %>% 
  ungroup() %>% 
  ### All columns of research topics (starting with "TF_") are changed to a longer format.
  pivot_longer(starts_with("TF_"), names_to = "topic") %>% 
  filter(value == TRUE) %>% 
  ### Counting the number of publications mentioning each topic in combination of phase and tumor groups.
  group_by(type, corpus_F, corpus_total) %>% 
  count(topic) %>% 
  ungroup() %>% 
  mutate(global_freq = n / corpus_total)

### Calculating research topic occurrences in each tumor group (i.e., article type - corpus - tumor group combinations).
### adjusted_freq columns will show if the topic occurrences are above- or below-average, compared to the global 
### topic occurrences determined above.
group_topic_freq <- researched_tumor_adjusted %>% 
  group_by(type, corpus_F, tumor_sub2) %>% 
  mutate(group_total = n()) %>% 
  ungroup() %>% 
  ### All columns of research topics (starting with "TF_") are changed to a longer format.
  pivot_longer(starts_with("TF_"), names_to = "topic") %>% 
  filter(value == TRUE) %>% 
  ### Counting the number of publications mentioning each topic in combination of phase and tumor groups.
  group_by(type, corpus_F, upper_group, tumor_sub2, group_total) %>% 
  count(topic) %>% 
  ungroup() %>% 
  ### Adding publication trends of tumor groups.
  mutate(group_freq = n / group_total) %>% 
  left_join(., global_topic_freq[, c(1, 2, 4, 6)], by = c("type", "corpus_F", "topic")) %>% 
  ### Calculating adjusted frequency.
  mutate(adjusted_freq = group_freq / global_freq) %>% 
  mutate(adjusted_freq2 = ifelse(adjusted_freq > 1.5, "high", 
                                 ifelse(adjusted_freq < 0.5, "low", "moderate"))) %>% 
  left_join(., category_names_T, by = c("topic" = "category_colname")) %>% 
  ### Changing columns into factors.
  mutate(group_name = factor(group_name, levels = category_group_level)) %>% 
  mutate(category_renamed = factor(category_renamed, levels = category_level)) %>% 
  mutate(upper_group = factor(upper_group, levels = c(unique(tumor_order$upper_group)))) %>% 
  mutate(tumor_sub2 = factor(tumor_sub2, levels = unique(tumor_order$tumor_sub2))) %>% 
  ### Selecting only research articles.
  filter(type == "Research article") %>% 
  ### Selecting research topics to plot.
  filter(T_filter == "A")


### Plotting a bubble plot for tumor organoids.
topic_freq_TO <- group_topic_freq %>% 
  filter(corpus_F == "tumor_organoid") %>% 
  ggplot(aes(x = tumor_sub2, y = fct_rev(category_renamed))) + 
  geom_point(aes(color = adjusted_freq2, size = sqrt(n)), shape = 16) + 
  scale_color_manual(values = c("high" = "red", "moderate" = "grey70", "low" = "blue"), 
                     breaks = c("high", "moderate", "low"), name = "Topic occurrence") + 
  scale_x_discrete(position = "top") + 
  scale_y_discrete(labels = function(x) sprintf(paste0("%-", max_width, "s"), x)) + 
  scale_size_continuous(range = c(0.3, 3), breaks = c(sqrt(10), sqrt(100)), labels = c(10, 100), name = "Publication counts") + 
  facet_grid(rows = vars(group_name), cols = vars(upper_group), scales = "free", space = "free", switch = "y") + 
  theme(strip.placement = "outside", 
        text = element_text(size = 8), 
        axis.text.x.top = element_text(size = 8, angle = 90, vjust = 0.3, hjust = 0), 
        axis.text.y = element_text(size = 8, hjust = 0, family = "Courier"), 
        strip.text.x.top = element_text(size = 8, face = 2, angle = 90, vjust = 0.3, hjust = 0), 
        strip.text.y.left = element_text(size = 7, face = 2, angle = 360, vjust = 0.5, hjust = 1), 
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"), 
        legend.key.size = unit(4, "mm"), 
        legend.position = c(- 0.40, 1.15), 
        legend.title = element_text(size = 8, face = 2), 
        legend.text = element_text(size = 8), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title = element_blank(), 
        axis.ticks = element_blank())

ggsave(topic_freq_TO, 
       filename = paste0(root_path, "results/model_comparison/topic_freq_TO.pdf"),  
       width = 178, height = 270, units = "mm")


### Plotting a bubble plot for ToC.
topic_freq_ToC <- group_topic_freq %>% 
  filter(corpus_F == "ToC") %>% 
  ggplot(aes(x = tumor_sub2, y = fct_rev(category_renamed))) + 
  geom_point(aes(color = adjusted_freq2, size = sqrt(n)), shape = 16) + 
  scale_color_manual(values = c("high" = "red", "moderate" = "grey70", "low" = "blue"),
                     breaks = c("high", "moderate", "low"), name = "Topic occurrence") + 
  scale_x_discrete(position = "top") + 
  scale_y_discrete(labels = function(x) sprintf(paste0("%-", max_width, "s"), x)) + 
  scale_size_continuous(range = c(0.3, 3), breaks = c(sqrt(10), sqrt(100)), labels = c(10, 100), name = "Publication counts") + 
  facet_grid(rows = vars(group_name), cols = vars(upper_group), scales = "free", space = "free", switch = "y") + 
  theme(strip.placement = "outside", 
        text = element_text(size = 8), 
        axis.text.x.top = element_text(size = 8, angle = 90, vjust = 0.3, hjust = 0), 
        axis.text.y = element_text(size = 8, hjust = 0, family = "Courier"), 
        strip.text.x.top = element_text(size = 8, face = 2, angle = 90, vjust = 0.3, hjust = 0), 
        strip.text.y.left = element_text(size = 7, face = 2, angle = 360, vjust = 0.5, hjust = 1), 
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"), 
        legend.key.size = unit(4, "mm"), 
        legend.position = c(- 0.40, 1.20), 
        legend.title = element_text(size = 8, face = 2), 
        legend.text = element_text(size = 8), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title = element_blank(), 
        axis.ticks = element_blank())

ggsave(topic_freq_ToC, 
       filename = paste0(root_path, "results/model_comparison/topic_freq_ToC.pdf"),  
       width = 170, height = 210, units = "mm")












##########
###
### Writing a supplementary table
###
##########
selected_topics <- read.csv(paste0(root_path, "csv/category_names_T.csv")) %>% 
  mutate(category_renamed = gsub("^dash ", "", category_renamed)) %>% 
  filter(T_filter == "A")

selected_publications <- researched_tumor_adjusted %>% 
  filter(type == "Research article") %>% 
  select(author, year, title, doi, corpus_F, upper_group, tumor_sub2, any_of(selected_topics$category_colname)) %>% 
  select(!TF_tumor) %>% 
  filter(rowSums(across(starts_with("TF_"))) > 0) %>% 
  rename(major_organ = upper_group, 
         tumor_type = tumor_sub2) %>% 
  arrange(year) %>% 
  arrange(factor(tumor_type, levels = unique(tumor_order$tumor_sub2))) %>% 
  arrange(factor(major_organ, levels = unique(tumor_order$upper_group))) %>%   
  arrange(factor(corpus_F, levels = c("tumor_organoid", "ToC"))) %>% 
  mutate(across(starts_with("TF_"), ~ ifelse(. == FALSE, "", "X"))) %>% 
  rename(any_of(setNames(selected_topics$category_colname, selected_topics$category_renamed))) 

write.csv(selected_publications, 
          file = paste0(root_path, "results/csv/table_S3_research_topics.csv"), 
          row.names = FALSE)
