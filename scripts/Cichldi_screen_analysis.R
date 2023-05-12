library(lubridate)
library(tidyr)
library(ggplot2)
library(gsheet)
library(tictoc)
library(data.table)
library(dplyr)
library(reticulate)
library(patchwork)
library(stringr)
library(ggrepel)

source("/Volumes/BZ/Scientific Data/RG-AS04-Data01/R_tracking_analysis/scripts/tracking_analysis_functions.R")
source("/Volumes/BZ/Scientific Data/RG-AS04-Data01/R_tracking_analysis/scripts/peak_finding_functions.R")


####### IDENTIFY THE FILES FOR IMPORT #######

## Set the directory to the '_analysis2' folder, or wherever all of the als files are located
setwd("/Volumes/BZ/Scientific Data/RG-AS04-Data01/Cichlid_sleep_videos/_analysis2/")

# List the files in the current directory that are als files
# This also finds the original runs for brichardi and crassus which have been transferred
# These need to be analysed differently (different times)
als.files <- list.files(path = ".", recursive = TRUE, pattern = "als.csv")


### Make meta data from file names ###

meta_data <- as.data.frame(str_split_fixed(als.files, "/", 2))
meta_data <- cbind(meta_data, as.data.frame(str_split_fixed(meta_data$V2, "_", 6)))[,1:7]
colnames(meta_data) <- c("species_six", "file_name", "date", "camera", "roi", "species_full", "sex")
meta_data$sample_id <- paste(meta_data$date, meta_data$camera, meta_data$roi, sep = "_")

als.files <- als.files[!(grepl("old", meta_data$file_name))]
meta_data <- meta_data[!(grepl("old", meta_data$file_name)),]

als.files <- als.files[!(grepl("Limnotilapia-dardenni", meta_data$species_six))]
meta_data <- meta_data[!(grepl("Limnotilapia-dardenni", meta_data$species_six)),]

als.files <- als.files[!(grepl("old", meta_data$species_six))]
meta_data <- meta_data[!(grepl("old", meta_data$species_six)),]


# url <- 'https://docs.google.com/spreadsheets/d/1Als2BdcYRUUWP4X5Np7WpfKt2YPMIz_55W3ktx5DOh4/edit?usp=sharing'
# meta_data <- read.csv(text=gsheet2text(url, format='csv', sheetid = '833420087'), stringsAsFactors=FALSE)
# meta_data$sample_id <- paste("FISH", meta_data$Date..started., "_", meta_data$Camera, "_", meta_data$ROI, sep = "")

#######################################################################################################################
####### RUN THE COMMANDS TO IMPORT DATA ###############################################################################
#######################################################################################################################

# This imports a list of files
als.data.list <- lapply(als.files, function(x) loadALSfiles(path_to_file = x, average_by = "minute"))

names(als.data.list) <- als.files

saveRDS(als.data.list, file = "/Volumes/BZ/Scientific Data/RG-AS04-Data01/Cichlid_sleep_videos/all_fish_als_combined.rds")

als.data.list <- readRDS(file = "/Volumes/BZ/Scientific Data/RG-AS04-Data01/Cichlid_sleep_videos/all_fish_als_combined.rds")

als.data.list.2 <- lapply(als.data.list, function(x) summariseALSdata(als_data = x, average_by = "halfhour"))

#######################################################################################################################
####### MAKE PLOTS FOR COMBINED #######################################################################################
#######################################################################################################################


# Reduce the list into one dataframe
week.combined <- Reduce(rbind, als.data.list.2)

## I need to remove certain days/times (where fish jumped)

# Add meta_data
week.combined <- merge(week.combined, meta_data, by = "sample_id")

# averages by groupings
test_2 <- week.combined %>% group_by(species_six, half_hour) %>% mutate(mean_speed_mm = mean(mean_speed_mm), mean_x_nt = mean(mean_x_nt), mean_y_nt = mean(mean_y_nt))

## Then plot
plot <- ggplot(test_2, aes(x = datetime, y = mean_speed_mm, group = species_six, color = species_six)) + geom_rect_shading_zoo() + shade_colours() + geom_point(size = 1) + geom_line() + theme_classic()

plot <- ggplot(test_2[test_2$species_six %in% c("Xenspi", "Gnapfe", "Petpol", "Cyplep", "Loblab", "Neopul-daffodil", "Ophboo"),], aes(x = datetime, y = mean_speed_mm, group = species_six, color = species_six)) + geom_rect_shading_zoo() + shade_colours() + geom_point(size = 1) + geom_line() + theme_classic()

ave.all.7days <- plot + shade_colours() + facet_wrap(~species_six, scales = "free_y")



#######################################################################################################################
#### Find peaks #######################################################################################################
#######################################################################################################################
# Plot all days for an invidual, first set all days to '01' in a new datetime column called 'daytime'
als.data.list.2 <- lapply(als.data.list.2, function(x) {
  x$daytime <- as.POSIXct(x$datetime, format = '%Y-%m-%d %H:%M:%S')
  day(x$daytime) <- 01
  return(x)
})

# This averages by day for a single dataset
# Importantly, ignore the single entry for day 1 when setting 'days_include'. For example, if you want the first 3 days, do 'days_include = c(1,2,3)' NOT 'days_include = c(2,3,4)'
avg.day.list <- lapply(als.data.list.2, function(x) averageDay(als_data = x, units = "halfhour", days_include = "all"))


dist <- 4
prom <- 7

# Find peaks across the week
avg.day.list <- lapply(avg.day.list, function(x) findPeaks(als_data = x, distance = dist, prominence = prom))
# ... and for the average day
als.data.list.2 <- lapply(als.data.list.2, function(x) findPeaks(als_data = x, distance = dist, prominence = prom))

### Return peak_percentages for the week and average day
percentages.day <- lapply(avg.day.list, function(x) {
  if (grepl("FISH20210205", x$sample_id[1]) | grepl("FISH20220216", x$sample_id[1])) {
    y <- returnPeakPercentages(als_data = x, avg_days = TRUE, zoo_times = TRUE)
  } else {
    y <- returnPeakPercentages(als_data = x, avg_days = TRUE, zoo_times = TRUE)
  }
  return(y)
})

percentages <- lapply(als.data.list.2, function(x) {
  if (grepl("FISH20210205", x$sample_id[1]) | grepl("FISH20220216", x$sample_id[1])) {
    y <- returnPeakPercentages(als_data = x, avg_days = FALSE, zoo_times = TRUE)
  } else {
    y <- returnPeakPercentages(als_data = x, avg_days = FALSE, zoo_times = TRUE)
  }
  return(y)
})


### Combine into a dataframe with sample_ids
df <- tibble(dawn_percentages = unlist(lapply(percentages, function(x) x[[1]])), 
             dusk_percentages = unlist(lapply(percentages, function(x) x[[2]])), 
             dawn_avg_day_percentages = unlist(lapply(percentages.day, function(x) x[[1]])), 
             dusk_avg_day_percentages = unlist(lapply(percentages.day, function(x) x[[2]])), 
             sample_id = str_extract(als.files, pattern = "FISH........_c._r."))


df$percentages <- rowMeans(df[,c("dawn_percentages", "dusk_percentages")])
df$percentages_avg_day <- rowMeans(df[,c("dawn_avg_day_percentages", "dusk_avg_day_percentages")])

all_data <- merge(df, meta_data, by = "sample_id")
all_data <- all_data[order(all_data$dawn_percentages),]
all_data$sample_id <- factor(all_data$sample_id, levels = all_data$sample_id)


## Sumarise by species and plot to see and compare with plots
## Seems like it works well, and there is a clear wide distribution with some with high dawn, not dusk and vice versa
## Also many with high or low both (there is clear correlation)

all_data_2 <- all_data %>% group_by(species_six) %>% summarise(avg_dawn_percentages = mean(dawn_percentages), avg_dusk_percentages = mean(dusk_percentages))

## Need to fix these names in my dataset (to match the pheno_data and the genomes)

old_names <- c("Cypfro", "Julmar", "Neopul-daffodil", "Neoven", "Parnig", "Telshe", "Trobem")
new_names <- c("Cphfro", "Julmrk", "Neopul", "NeoveS", "Pcynig", "TelteS", "Tronig")

all_data_2$species_six[match(old_names, all_data_2$species_six)] <- new_names

ggplot(all_data_2, aes(x = avg_dawn_percentages, y = avg_dusk_percentages, colour = species_six, label = species_six)) + geom_point(size = 3) + geom_text_repel() + theme_classic() + theme(legend.position = "none")

## Load in the other phenotyping data and append

pheno_data <- read.csv("/Volumes/BZ/Scientific Data/RG-AS04-Data01/Cichlid-genomes/cichlid_sleep_gwas/pheno_data/combined_cichlid_data_2022-11-17.csv", row.names = "X")

pheno_data$peak_dawn <- all_data_2$avg_dawn_percentages[match(pheno_data$six_letter_name_Ronco, all_data_2$species_six)]
pheno_data$peak_dusk <- all_data_2$avg_dusk_percentages[match(pheno_data$six_letter_name_Ronco, all_data_2$species_six)]

## Save it back out for gwas

write.csv(pheno_data, file = "/Volumes/BZ/Scientific Data/RG-AS04-Data01/Cichlid-genomes/cichlid_sleep_gwas/pheno_data/combined_cichlid_data_2022-11-17_new_peaks.csv")
write.csv(pheno_data[!(pheno_data$six_letter_name_Ronco %in% c("Neobre", "Neomul")),], file = "/Volumes/BZ/Scientific Data/RG-AS04-Data01/Cichlid-genomes/cichlid_sleep_gwas/pheno_data/combined_cichlid_data_2022-11-17_noNeoShell_new_peaks.csv")
write.csv(pheno_data[!(pheno_data$six_letter_name_Ronco %in% c("Neobre", "Neomul", "Lamoce", "NeoveS", "TelteS", "Telvit")),], file = "/Volumes/BZ/Scientific Data/RG-AS04-Data01/Cichlid-genomes/cichlid_sleep_gwas/pheno_data/combined_cichlid_data_2022-11-17_noshell_new_peaks.csv")


