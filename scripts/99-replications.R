#### Preamble ####
# Purpose: Replicate Table 1, Figures 1(a), 1(b) in Moore et al.'s paper titled "COVID-19, School Closures, and
# Outcomes" found at https://www.journals.uchicago.edu/doi/epdf/10.1086/716662. 
# Author: Julia Kim 
# Date: 2 April 2024 
# Contact:juliaym.kim@mail.utoronto.ca
# License: MIT

#### Workspace Setup ####
library(tidyverse)
library(ggplot2)
library(here)

#### TABLE 1 #### 
spending_data <- read.csv(here::here("data/raw_data/spendingdata.csv")) 

# aggregate the spending data by name and sum up the spending
summarised_spending_data <- aggregate(spending ~ name, data = spending_data, FUN = sum)

# arrange the data in descending order of total spending and select the first 25 rows
summarised_spending_data <- summarised_spending_data |>
  mutate(spending = round(spending / 1e6),  # divide spending by 1e6 to convert to millions
         percentage = round((spending / sum(spending)) * 100)) |> 
  arrange(desc(spending)) |> 
  slice(1:25)

# convert scientific names to common names 
scientific_common_names <- read.csv(here::here("data/raw_data/scientific_common_names.csv")) 
summarised_spending_data <- left_join(summarised_spending_data, scientific_common_names, 
                                      by = c("name" = "scientific_name")) 
summarised_spending_data <- summarised_spending_data |> 
  select(common_name, everything()) |> # reorder columns so that common_name is first 
  select(-name) # remove the scientific name column 
  
#### FIGURE 1(a) #### 
cleaned_spending_data <- spending_data |>
  group_by(code) |> 
  # for each group based on code, calculate the mean of the spending variable, ignoring NA values 
  dplyr::summarise(mean_annual_spending = mean(spending, na.rm = TRUE), taxon = taxon[1]) |>
  group_by(taxon) |> 
  # fo each group based on taxon, calculate the median of the mean_annual_spending variable,
  # ignoring NA values 
  dplyr::summarise(mean_spending = quantile(mean_annual_spending, 0.5, na.rm = TRUE)) |>
  arrange(desc(mean_spending)) |>  # arrange data by mean_spending in descending order
  mutate(taxon = factor(taxon, levels = taxon)) # reorder factor levels of "taxon"

cleaned_spending_data |> 
  ggplot(aes(x = taxon, y = mean_spending/1e6)) + # rescale y-variable 
  geom_bar(stat = "identity") + 
  theme_minimal() + 
  labs(x = "Taxon", y = "Median Annual Recovery\nSpending per Species\n(Millions of Dollars)") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # rotate x-labels 

#### FIGURE 1(b) #### 
species_data <- read.csv(here::here("data/raw_data/speciesdata.csv"))
cleaned_species_data <- species_data |>
  filter(status %in% c(1, 2, 3, 4, 5, "UNK")) |>
  # relabel status numbers
  mutate(status = case_when(   
    status == 1 ~ "Critically Imperiled",
    status == 2 ~ "Imperiled",
    status == 3 ~ "Vulnerable",
    status == 4 ~ "Apparently Secure",
    status == 5 ~ "Secure",
    status == "UNK" ~ "Unknown",
    TRUE ~ as.character(status)
  )) |> 
  # convert status to a factor with the desired levels 
  mutate(status = factor(status, levels = c("Critically Imperiled", "Imperiled", "Vulnerable", 
                                            "Apparently Secure", "Secure", "Unknown"))) |> 
  # convert taxon to a factor with levels arranged in the same order as in Figure 1(a) 
  add_count(taxon) |> # add column that counts number of occurrences of each taxon 
  group_by(taxon, status) |>  # group by unique taxon-status groups  
  # count number of occurrences of each taxon-status group, and total number of rows in each group 
  dplyr::summarise(count = n(), total = n[1]) |> 
  mutate(proportion = count/total) 

# assign colour to each status, as used by NatureServe  
# https://www.natureserve.org/conservation-status-assessment 
status_colours <- c(
  "Critically Imperiled" = "#DB0908",   # Red
  "Imperiled" = "#F17619",               # Orange
  "Vulnerable" = "#FDD131",               # Yellow
  "Apparently Secure" = "#27BBEF",        # Light blue
  "Secure" = "#194B89",                   # Blue
  "Unknown" = "grey"                    # Grey
)

# plot data 
cleaned_species_data |> 
  ggplot(aes(x = taxon, y = proportion, fill = status)) + 
  geom_bar(stat = "identity", position = "fill") + 
  scale_fill_manual(values = status_colours) + # apply custom colors
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + # rotate x-labels 
  labs(x="Taxon", y="Proportion", fill="Status") + 
  geom_text(aes(x = taxon, y = 1.05, label = total), size=3)  # add counts for each taxon above the stacked chart

#### FIGURE 2 #### 
# set ngrams with suspicious ratio, i.e., for which the ratio of ngram_common to ngram_science is 
# greater than 10 to "NA" 
species_data$ngram_common[which(abs(species_data$ngram_common / species_data$ngram_science) > 10)] = NA
