library(lubridate)
library(tidyr)
library(ggplot2)
library(gsheet)
library(tictoc)
library(data.table)
library(dplyr)
library(reticulate)
library(patchwork)
library(here)

source(here("scripts/tracking_analysis_functions.R"))
source(here("scripts/peak_finding_functions.R"))


####### IDENTIFY THE FILES FOR IMPORT #######

## Set the directory to the '_analysis2' folder, or wherever all of the als files are located

# List the files in the current directory that are als files
# This also finds the original runs for brichardi and crassus which have been transferred
# These need to be analysed differently (different times)
als.files <- list.files(path = ".", recursive = TRUE, pattern = "als.csv")


### Load meta data ###

url <- 'https://docs.google.com/spreadsheets/d/1Als2BdcYRUUWP4X5Np7WpfKt2YPMIz_55W3ktx5DOh4/edit?usp=sharing'
meta_data <- read.csv(text=gsheet2text(url, format='csv', sheetid = '833420087'), stringsAsFactors=FALSE)
meta_data$sample_id <- paste("FISH", meta_data$Date..started., "_", meta_data$Camera, "_", meta_data$ROI, sep = "")

#######################################################################################################################
####### RUN THE COMMANDS TO IMPORT DATA ###############################################################################
#######################################################################################################################

# This imports a list of files
als.data.list <- lapply(als.files, function(x) loadALSfiles(path_to_file = x, average_by = "minute"))

names(als.data.list) <- als.files

als.data.list.2 <- lapply(als.data.list, function(x) summariseALSdata(als_data = x, average_by = "halfhour"))

#######################################################################################################################
####### MAKE PLOTS FOR COMBINED #######################################################################################
#######################################################################################################################


# Reduce the list into one dataframe
week.combined <- Reduce(rbind, als.data.list.2)

## I need to remove certain days/times (where fish jumped)

# Add meta_data
week.combined <- merge(week.combined, meta_data, by = "sample_id")


baseline <- week.combined[week.combined$day %in% c(1,2,3,4),]
baseline$daytime <- as.POSIXct(baseline$half_hour, format = '%Y-%m-%d %H:%M:%S')
day(baseline$daytime) <- 1

treatment <- week.combined[week.combined$day %in% c(5,6,7,8),]
treatment$daytime <- as.POSIXct(treatment$half_hour, format = '%Y-%m-%d %H:%M:%S')
day(treatment$daytime) <- 1

# averages by groupings
# test_2 <- week.combined %>% group_by(SPECIES, TREATMENT, TIME, half_hour, day) %>% mutate(sd_speed_mm = sd(mean_speed_mm), mean_speed_mm = mean(mean_speed_mm), mean_x_nt = mean(mean_x_nt), mean_y_nt = mean(mean_y_nt))

# averages by groupings and days

baseline_2 <- baseline %>% group_by(Species, daytime) %>% summarise(sd_speed_mm = sd(mean_speed_mm), mean_speed_mm = mean(mean_speed_mm), mean_x_nt = mean(mean_x_nt), mean_y_nt = mean(mean_y_nt))
treatment_2 <- treatment %>% group_by(Species, daytime) %>% summarise(sd_speed_mm = sd(mean_speed_mm), mean_speed_mm = mean(mean_speed_mm), mean_x_nt = mean(mean_x_nt), mean_y_nt = mean(mean_y_nt))

### Add ribbons for SDs

dts <- data.frame(xstart = c("1970-01-01 03:00:00"), xend = c("1970-01-01 17:00:00"), col = c("dark_dark"))

## With ribbons for SD
# baseline_plot <- ggplot(baseline_2, aes(x = daytime, y = mean_speed_mm, group = interaction(Species), color = Species)) + geom_rect_shading_bz_Ndays(n_days = 1) + geom_ribbon(aes(ymin = mean_speed_mm-sd_speed_mm, ymax = mean_speed_mm+sd_speed_mm, fill = Species), alpha = 0.1) + shade_colours() + geom_point(size = 1) + geom_line(alpha = 0.5) + theme_classic()
# baseline_plot <- baseline_plot + facet_wrap(~Species, ncol = 1, scales = "fixed") + scale_color_manual(values = c("black", "black"))

# treatment_plot <- ggplot(treatment_2, aes(x = daytime, y = mean_speed_mm, group = interaction(Species), color = Species)) + geom_rect_shading_bz_Ndays(n_days = 1, date_time_shade = dts) + geom_ribbon(aes(ymin = mean_speed_mm-sd_speed_mm, ymax = mean_speed_mm+sd_speed_mm, fill = Species), alpha = 0.1) + shade_colours() + geom_point(size = 1) + geom_line(alpha = 0.5) + theme_classic()
# treatment_plot <- treatment_plot + facet_wrap(~Species, ncol = 1, scales = "fixed") + scale_color_manual(values = c("black", "black"))

## Without ribbons (for grant)
baseline_plot <- ggplot(baseline_2, aes(x = daytime, y = mean_speed_mm, group = interaction(Species), color = Species)) + geom_rect_shading_bz_Ndays(n_days = 1) + shade_colours() + geom_line(size = 3) + theme_classic()
baseline_plot <- baseline_plot + facet_wrap(~Species, ncol = 1, scales = "free_y") + scale_color_manual(values = c("red", "black"))

treatment_plot <- ggplot(treatment_2, aes(x = daytime, y = mean_speed_mm, group = interaction(Species), color = Species)) + geom_rect_shading_bz_Ndays(n_days = 1, date_time_shade = dts) + shade_colours() + geom_line(size = 3) + theme_classic()
treatment_plot <- treatment_plot + facet_wrap(~Species, ncol = 1, scales = "free_y") + scale_color_manual(values = c("red", "black"))


baseline_plot + treatment_plot


pdf(here("lightperturb_baseline_behaviour_plots.pdf"), width = 7.5, height = 8.57)
baseline_plot
dev.off()

pdf(here("lightperturb_treatment_behaviour_plots.pdf"), width = 7.5, height = 8.57)
treatment_plot
dev.off()
