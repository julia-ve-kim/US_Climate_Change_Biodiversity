#### Preamble ####
# Purpose: Test the data sets 
# Author: Julia Kim 
# Date: 23 March 2024 
# Contact: juliaym.kim@mail.utoronto.ca
# License: MIT
# Pre-requisites: Downloaded the data files through 01-download_data.R and through the instructions in the README

#### Workspace setup ####
library(tidyverse)

#### Read data ####
cleaned_species_data <- read_csv(here("data/analysis_data/cleaned_speciesdata.csv"))
cleaned_spending_data <- read_csv(here("data/analysis_data/cleaned_spendingdata.csv"))

#### Testing species data ####
# Check that there are exactly 9 unique taxons to which a species can belong  
length(unique(cleaned_species_data$taxon)) == 9

# Check those taxons are exclusively one of these 9: "Fishes", "Birds", "Mammals", "Amphibians", 
# "Invertebrates", "Plants", "Reptiles", "Fungi", "Protists"
unique(cleaned_species_data$taxon) == c("Fishes", "Birds", "Mammals", "Amphibians", 
                                        "Invertebrates", "Plants", "Reptiles", "Fungi", 
                                        "Protists")

# Check that listed, probs are binary variables taking on 0 or 1 
all(cleaned_species_data$listed %in% c(0, 1)) == TRUE 
all(cleaned_species_data$probs %in% c(0, 1)) == TRUE 

# Check that status is a value of 1, 2, 3, 4, 5, "UNK", "Prob. Extinct", "Extinct"
# or NA 
all(cleaned_species_data$status %in% c(1, 2, 3, 4, 5, "UNK", "Prob. Extinct", "Extinct", NA)) == TRUE 

# Check ngenus only takes NA or positive values 
all(is.na(cleaned_species_data$ngenus) | cleaned_species_data$ngenus > 0) == TRUE 

# Verify that each column is of the appropriate class 
cleaned_species_data$taxon |> class() == "character"
cleaned_species_data$listed |> class() == "numeric"
cleaned_species_data$status |> class() == "character"
cleaned_species_data$ngram_common |> class() == "numeric"
cleaned_species_data$ngram_science |> class() == "numeric"
cleaned_species_data$ngenus |> class() == "numeric"
cleaned_species_data$probs |> class() == "numeric"

#### Cleaned spending data ####
# Check that status_species is a value of 1, 2, 3, 4, 5,  "UNK", "Prob. Extinct", "Extinct"
# or NA 
all(cleaned_spending_data$status_species %in% c(1, 2, 3, 4, 5, "UNK", "Prob. Extinct", "Extinct", NA)) == TRUE 

# Check priority numbers are NA or between 1 and 18 
all(ifelse(is.na(cleaned_spending_data$priority), TRUE, cleaned_spending_data$priority %in% 1:18)) == TRUE 

# Check that conflict, CommonNameFlag is NA or takes on 0 or 1 
all(ifelse(is.na(cleaned_spending_data$conflict), TRUE, cleaned_spending_data$conflict %in% 0:1)) == TRUE
all(ifelse(is.na(cleaned_spending_data$CommonNameFlag), TRUE, cleaned_spending_data$CommonNameFlag %in% 0:1)) == TRUE

# Check ngenus only takes on NA or positive values 
all(is.na(cleaned_spending_data$ngenus) | cleaned_spending_data$ngenus > 0) == TRUE 

# Verify that each column is of the appropriate class
cleaned_spending_data$name |> class() == "character"
cleaned_spending_data$taxon |> class() == "character"
cleaned_spending_data$status_species |> class() == "character"
cleaned_spending_data$ngenus |> class() == "numeric"
cleaned_spending_data$priority |> class() == "numeric"
cleaned_spending_data$CommonNameFlag |> class() == "numeric"
cleaned_spending_data$ngrams_common |> class() == "numeric"
cleaned_spending_data$ngrams_science |> class() == "numeric"