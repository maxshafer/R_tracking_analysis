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

source("/Volumes/BZ/Scientific Data/RG-AS04-Data01/R_tracking_analysis/scripts/tracking_analysis_functions.R")
source("/Volumes/BZ/Scientific Data/RG-AS04-Data01/R_tracking_analysis/scripts/peak_finding_functions.R")


####### IDENTIFY THE FILES FOR IMPORT #######

## Set the directory to the '_analysis2' folder, or wherever all of the als files are located
setwd("/Volumes/BZ/Scientific Data/RG-AS04-Data01/Cichlid_sleep_videos/melatonin_exps/")

# List the files in the current directory that are als files
# This also finds the original runs for brichardi and crassus which have been transferred
# These need to be analysed differently (different times)
als.files <- list.files(path = ".", recursive = TRUE, pattern = "als.csv")

# load meta data from google sheet
url <- 'https://docs.google.com/spreadsheets/d/19SWcIXumBUqvhEZzIzzMZKFtCcRxNbxWwmaQlmhOiIg/edit?usp=sharing'
meta_data <- read.csv(text=gsheet2text(url, format='csv', sheetid = 726571573), stringsAsFactors=FALSE)
meta_data$sample_id <- paste("FISH", meta_data$DATE, "_", meta_data$CAMERA, "_", meta_data$ROI, sep = "")


#######################################################################################################################
####### RUN THE COMMANDS TO IMPORT DATA ###############################################################################
####### & REMOVE HALF HOUR DURING ADMINISTRATION ######################################################################
####### & CORRECT DAYLIGHT SAVINGS ####################################################################################
#######################################################################################################################

# This imports a list of files
als.data.list <- lapply(als.files, function(x) loadALSfiles(path_to_file = x, average_by = "minute"))

names(als.data.list) <- als.files

## The computer was on regular time, but the lights and fish were on daylight savings time still, so I need to remove an hour? Or there is a jump

## Melatonin was inject between 9h00 - 9h30  on day 5 and 6 (jan 5th and 6th), therefore tracking doesn't work during this window
## and tracks should be ignored. Maybe do +/- 5-10 mins to be safe

als.data.list.2 <- lapply(als.data.list, function(als) {
  als$mean_speed_mm[als$day %in% c(5,6,7) & als$hour == 9 & als$minute < 30] <- NA
  return(als)
})




als.data.list.2 <- lapply(als.data.list.2, function(x) summariseALSdata(als_data = x, average_by = "halfhour"))

#######################################################################################################################
####### MAKE PLOTS FOR INDIVIDUAL FISH ################################################################################
#######################################################################################################################

# Plot all days for an invidual, first set all days to '01' in a new datetime column called 'daytime'
als.data.list.2 <- lapply(als.data.list.2, function(x) {
  x$daytime <- as.POSIXct(x$datetime, format = '%Y-%m-%d %H:%M:%S')
  day(x$daytime) <- 01
  return(x)
})


week_plots <- lapply(als.data.list.2, function(x) {
  return(ggplot(x, aes(x = datetime, y = mean_speed_mm, group = day, colour = day)) + geom_rect_shading_bz_7days() + shade_colours() + geom_point() + geom_line() + ylim(0,120) + ggtitle(paste(x$sample_id[1])))
})


all_days_plots <- lapply(als.data.list.2, function(x) {
  if (grepl("FISH20210205", x$sample_id[1]) | grepl("FISH20220216", x$sample_id[1])) {
    return(ggplot(x, aes(x = daytime, y = mean_speed_mm, group = day, colour = day)) + geom_rect_shading_zoo() + shade_colours() + geom_point() + geom_line() + ylim(0,120) + ggtitle(paste(x$sample_id[1])))
  } else {
    return(ggplot(x, aes(x = daytime, y = mean_speed_mm, group = day, colour = day)) + geom_rect_shading_bz() + shade_colours() + geom_point() + geom_line() + ylim(0,120) + ggtitle(paste(x$sample_id[1])))
  }
})

names(all_days_plots) <- unlist(lapply(als.data.list.2, function(x) x$sample_id[1]))

# This averages by day for a single dataset
# Importantly, ignore the single entry for day 1 when setting 'days_include'. For example, if you want the first 3 days, do 'days_include = c(1,2,3)' NOT 'days_include = c(2,3,4)'
avg.day.list <- lapply(als.data.list.2, function(x) averageDay(als_data = x, units = "halfhour", days_include = "all"))

avg_day_plots <- lapply(avg.day.list, function(x) {
  if (grepl("FISH20210205", x$sample_id[1]) | grepl("FISH20220216", x$sample_id[1])) {
    return(ggplot(x, aes(x = datetime, y = mean_speed_mm)) + geom_rect_shading_zoo() + shade_colours() + geom_point() + geom_line() + ylim(0,120) + ggtitle(paste(x$sample_id[1])))
  } else {
    return(ggplot(x, aes(x = datetime, y = mean_speed_mm)) + geom_rect_shading_bz() + shade_colours() + geom_point() + geom_line() + ylim(0,120) + ggtitle(paste(x$sample_id[1])))
  }
})

names(avg_day_plots) <- unlist(lapply(als.data.list.2, function(x) x$sample_id[1]))


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
test_2 <- week.combined %>% group_by(SPECIES, TREATMENT, TIME, half_hour, day) %>% mutate(sd_speed_mm = sd(mean_speed_mm), mean_speed_mm = mean(mean_speed_mm), mean_x_nt = mean(mean_x_nt), mean_y_nt = mean(mean_y_nt))

# averages by groupings and days

baseline_2 <- baseline %>% group_by(SPECIES, TREATMENT, TIME, daytime) %>% summarise(sd_speed_mm = sd(mean_speed_mm), mean_speed_mm = mean(mean_speed_mm), mean_x_nt = mean(mean_x_nt), mean_y_nt = mean(mean_y_nt))
treatment_2 <- treatment %>% group_by(SPECIES, TREATMENT, TIME, daytime) %>% summarise(sd_speed_mm = sd(mean_speed_mm), mean_speed_mm = mean(mean_speed_mm), mean_x_nt = mean(mean_x_nt), mean_y_nt = mean(mean_y_nt))

### Add ribbons for SDs

dts <- data.frame(xstart = c("1970-01-01 10:00:00"), xend = c("1970-01-01 10:30:00"), col = c("injection"))


baseline_plot <- ggplot(baseline_2, aes(x = daytime, y = mean_speed_mm, group = interaction(TREATMENT), color = TREATMENT)) + geom_rect_shading_bz_Ndays(n_days = 1) + geom_ribbon(aes(ymin = mean_speed_mm-sd_speed_mm, ymax = mean_speed_mm+sd_speed_mm, fill = TREATMENT), alpha = 0.1) + shade_colours() + geom_point(size = 1) + geom_line(alpha = 0.5) + theme_classic()
baseline_plot <- baseline_plot + facet_wrap(~TIME+SPECIES, ncol = 2, scales = "free_y") + scale_color_manual(values = c("black", "red","black", "red"))

treatment_plot <- ggplot(treatment_2, aes(x = daytime, y = mean_speed_mm, group = interaction(TREATMENT), color = TREATMENT)) + geom_rect_shading_bz_Ndays(n_days = 1, date_time_shade = dts) + geom_ribbon(aes(ymin = mean_speed_mm-sd_speed_mm, ymax = mean_speed_mm+sd_speed_mm, fill = TREATMENT), alpha = 0.1) + shade_colours() + geom_point(size = 1) + geom_line(alpha = 0.5) + theme_classic()
treatment_plot <- treatment_plot + facet_wrap(~TIME+SPECIES, ncol = 2, scales = "free_y") + scale_color_manual(values = c("black", "red","black", "red"))

baseline_plot + treatment_plot








