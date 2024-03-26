#### Preamble ####
# Purpose: Cleans the raw datasets 
# Author: Julia Kim 
# Date: 23 March 2024 
# Contact: juliaym.kim@mail.utoronto.ca
# License: MIT
# Pre-requisites: run 01-download_data.R, and download the relevant data as outlined in the README. 

#### Workspace setup ####
library(tidyverse)

#### SPECIES DATASET CLEANING ####
raw_species_data <- read_csv("data/raw_data/speciesdata.csv")

cleaned_species_data1 <- 
  raw_species_data |> 
  mutate(
    # remove long tails of common and scientific ngrams by replacing values by NA 
    ngram_common = ifelse(ngram_common > 10, NA, ngram_common),
    ngram_science = ifelse(ngram_science > 10, NA, ngram_science),
    # remove ngrams with suspicious ratio
    ngram_common = ifelse(abs(ngram_common/ngram_science) > 10, NA, ngram_common), 
    # set listing to 0 for species whose NatureServe ranking was determined after delisting or before listing
    listed = ifelse(probs == 1, 0, listed)
  ) |> 
  # use mammals as the dropped category / reference level 
  mutate(taxon = factor(taxon)) |> 
  mutate(taxon = relevel(taxon, ref = "Mammals")) |> 
  # exclude rows where ngram_common_flag is equal to 1
  filter(ngram_common_flag != 1) |> 
  # remove unneeded columns 
  select(-code, -family, -order, -name, -status_global, -evdist, -ge, -edge, -ngram_common_flag)

#### Save data ####
write_csv(cleaned_species_data1, "data/analysis_data/cleaned_speciesdata.csv")

#### SPENDING DATASET CLEANING ####
raw_spending_data <- read_csv("data/raw_data/spendingdata.csv")

cleaned_spending_data1 <- raw_spending_data |>
  # filter so that spending is always positive 
  filter(spending > 0) |> 
  select(-code, -year, -family, -order, -genus, -status, -status_global, -yearssincelisting, 
         -region, -evdist, -ge, -edge, -X, -X.1, -rangeareas, -datelisted, -extinct, 
         -priority_threat, -priority_potential, -priority_rarity)
  
#### Save data ####
write_csv(cleaned_spending_data1, "data/analysis_data/cleaned_spendingdata.csv")