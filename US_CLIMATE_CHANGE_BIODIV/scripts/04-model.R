#### Preamble ####
# Purpose: Make models of the data to generate inferential statistics.
# Author: Julia Kim 
# Date: 23 March 2024 
# Contact: juliaym.kim@mail.utoronto.ca 
# License: MIT
# Pre-requisites: Downloaded the data files through 01-download_data.R and through the instructions in the README

#### Workspace setup ####
library(tidyverse)
library(rstanarm)

#### Read data ####
analysis_species_data <- read_csv("data/analysis_data/cleaned_speciesdata.csv")
analysis_spending_data <- read_csv("data/analysis_data/cleaned_spendingdata.csv")

#### Model data #### 
# set reference level of taxon in to "Mammals"
analysis_species_data$taxon <- factor(analysis_species_data$taxon)
analysis_species_data$taxon <- relevel(analysis_species_data$taxon, ref = "Mammals")

analysis_spending_data$taxon <- factor(analysis_spending_data$taxon)
analysis_spending_data$taxon <- relevel(analysis_spending_data$taxon, ref = "Mammals")

# fit a logistic regression model, with listed as the response variable 
species_listing_model <- glm(listed ~ taxon + status + ngram_common + ngram_science + I(log(ngenus)),  
                     data = analysis_species_data, 
                     family = binomial(link = "logit")
                     )
species_spending_model <- lm(I(log(spending) ~ taxon + status_species + priority_threat + 
                                 priority_rarity + priority_potential + conflict + 
                                 ngrams_common + ngrams_science + region_num + I(log(ngenus))), 
                             data = analysis_spending_data) 

#### Save models ####
saveRDS(
  species_listing_model,
  file = "models/species_listing_model.rds"
)

saveRDS(
  species_spending_model, 
  file = "models/species_spending_model.rds"
)

#### Summarise models #### 
summary(species_listing_model) 
summary(species_spending_model)