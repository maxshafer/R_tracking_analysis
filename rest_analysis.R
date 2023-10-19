library(lubridate)
library(tidyr)
library(ggplot2)
library(gsheet)
library(tictoc)
library(data.table)
library(dplyr)
library(here)
library(data.table)

source(here("rest_functions.R"))
source(here("scripts", "tracking_analysis_functions.R"))

url <- 'https://docs.google.com/spreadsheets/d/14vd7fH7Ra64nNFi5mq34iEUfIIfE2MGHqLEfxJ8C71c/edit?usp=sharing'
meta_data <- read.csv(text=gsheet2text(url, format='csv'), stringsAsFactors=FALSE)

als.files <- list.files(path = "/Users/AshasMacbookAir/Library/CloudStorage/OneDrive-SharedLibraries-UniversityofToronto/Maxwell Shafer - cichlid_als_files/Altcal/", recursive = TRUE, pattern = "_als.csv")

setwd("/Users/AshasMacbookAir/Library/CloudStorage/OneDrive-SharedLibraries-UniversityofToronto/Maxwell Shafer - cichlid_als_files/Altcal/")
data <- lapply(als.files, function(x) loadALSfiles(path_to_file = x, average_by = "second"))

data <- rbindlist(data)
data <- merge(data, meta_data, by = "sample_id")
#print("all data merged")

#print("starting rest analysis")
restData(data)

