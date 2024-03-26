#### Preamble ####
# Purpose: Explore and understand the spending model generated in 04-model.R by 
# creating data visualisations. Note visualisations of the species model are 
# contained in 99-replications.R.  
# Author: Julia Kim 
# Date: 24 March 2024 
# Contact: juliaym.kim@mail.utoronto.ca 
# License: MIT
# Pre-requisites: Run the 04-model.R script to produce the models. 

#### Workspace setup ####
library(tidyverse)
library(readr)
library(ggplot2)

#### Read data and model ####
cleaned_spending_data <- read_csv("data/analysis_data/cleaned_spendingdata.csv")
spending_model <- readRDS("models/species_listing_model.rds")

#### Generate plots ####
### Predicted log(spending) against assessed conservation status, with points shaped 
# according to taxon ### 
taxconsmeans <- cleaned_spending_data |>
  # remove rows where status_species is NA
  filter(!is.na(status_species)) |>
  # filter rows where CommonNameFlag is 0
  filter(CommonNameFlag == 0) |>
  # group by taxon and status
  group_by(taxon, status_species) |>
  # calculate median values and uncertainties for each variable
  summarize(
    ngrams_common = median(ngrams_common, na.rm = TRUE),
    ngrams_science = median(ngrams_science, na.rm = TRUE),
    ngenus = log(median(ngenus, na.rm = TRUE)), 
    priority_threat = median(priority_threat, na.rm = TRUE),
    priority_potential = median(priority_potential, na.rm = TRUE),
    priority_rarity = median(priority_rarity, na.rm = TRUE), 
    region_num = median(region_num, na.rm = TRUE), 
    conflict = median(conflict, na.rm = TRUE),
    spending = median(spending, na.rm = TRUE)
  )

# generate predictions 
taxconsmeans$log_spending_prediction <- predict(species_spending_model, newdata = taxconsmeans)

# plot data
taxconsmeans |> 
  ggplot(aes(x = status_species, y = log_spending_prediction, group = taxon, col = taxon, pch = taxon)) + 
  geom_point(size = 3, stroke = 0.75) + 
  labs(x = "Assessed Conservation Status", y = "Predicted Log(Spending)", fill = "Taxon") + 
  theme_minimal() +
  scale_shape_manual(values = shape_values)  # manually specify shape values