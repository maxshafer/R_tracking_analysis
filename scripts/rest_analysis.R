library(lubridate)
library(tidyr)
library(ggplot2)
library(gsheet)
library(tictoc)
library(data.table)
library(dplyr)
library(here)

args = commandArgs(trailingOnly=TRUE)
if (length(args) != 1) {
  stop("only 1 argument for Rscript command")
}

als_file <- args[1]

source("/home/ayasha/R_tracking_analysis/scripts/rest_functions.R")
source("/home/ayasha/R_tracking_analysis/scripts/tracking_analysis_functions.R")

meta_data <- read.csv('/home/ayasha/projects/def-mshafer/ayasha/meta_data.csv')
PC_data <- read.csv('/home/ayasha/projects/def-mshafer/ayasha/pca_loadings_individuals.csv')
PC1 <- select(PC_data, FishID, pc1)
rename(PC1, "FishID" = "sample_id")


setwd("/home/ayasha/projects/def-mshafer/ayasha/cichlid_als_data/")
als_data <- loadALSfiles(als_file, average_by = "second")
data.combined <- merge(als_data, meta_data, by = "sample_id")
data.combined <- merge(data.combined, PC1, by = "sample_id")

#print("starting rest analysis")
rest_data <- restData(data.combined)

