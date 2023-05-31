library(lubridate)
library(tidyr)
library(ggplot2)
library(gsheet)
library(tictoc)
library(data.table)
library(dplyr)
library(reticulate)
library(patchwork)

source("/Volumes/BZ/Scientific Data/RG-AS04-Data01/R_tracking_analysis/scripts/tracking_analysis_functions.R")
source("/Volumes/BZ/Scientific Data/RG-AS04-Data01/R_tracking_analysis/scripts/peak_finding_functions.R")


####### IDENTIFY THE FILES FOR IMPORT #######

## Set the directory to the '_analysis2' folder, or wherever all of the als files are located
setwd("/Volumes/BZ/Scientific Data/RG-AS04-Data01/LCP/FISH20230407")

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

# averages by groupings
test_2 <- week.combined %>% group_by(Species, half_hour, day) %>% mutate(sd_speed_mm = sd(mean_speed_mm), mean_speed_mm = mean(mean_speed_mm), mean_x_nt = mean(mean_x_nt), mean_y_nt = mean(mean_y_nt))


## Then plot
plot <- ggplot(test_2, aes(x = datetime, y = mean_speed_mm, group = sex, color = sex)) + geom_rect_shading_bz_7days_darkdark() + shade_colours() + geom_point(size = 1) + geom_line() + theme_classic()

ave.all.7days <- plot + shade_colours() + facet_wrap(~sex, ncol = 1, scales = "free_y")



### For laura's shell dwellers

test_2 <- week.combined %>% group_by(sex, half_hour, day) %>% mutate(sd_speed_mm = sd(mean_speed_mm), mean_speed_mm = mean(mean_speed_mm), mean_x_nt = mean(mean_x_nt), mean_y_nt = mean(mean_y_nt))


## Then plot

## Days 4 and 5 have pulses
## I think they need to be shifted a bit, because the way they are averaged and the way these rects are plotted doesn't match?
dts <- data.frame(xstart = c("1970-01-04 03:00:00", "1970-01-04 15:00:00", "1970-01-05 03:00:00", "1970-01-05 15:00:00", "1970-01-07 00:00:00"), xend = c("1970-01-04 04:00:00", "1970-01-04 16:00:00", "1970-01-05 04:00:00", "1970-01-05 16:00:00", "1970-01-08 23:59:00"), col = c("light_pulse", "dark_pulse", "light_pulse", "dark_pulse", "dark_dark"))

plot <- ggplot(test_2, aes(x = datetime, y = mean_speed_mm, color = sex, group = sex)) + geom_rect_shading_bz_Ndays(n_days = 9, date_time_shade = dts) + geom_ribbon(aes(ymin = mean_speed_mm-sd_speed_mm, ymax = mean_speed_mm+sd_speed_mm, color = sex), alpha = 0.1) + shade_colours() + geom_point(size = 1) + geom_line() + theme_classic() + scale_color_manual(values = c("red", "black"))

ave.all.7days <- plot + shade_colours() + facet_wrap(~sex, ncol = 1, scales = "free_y")

pdf("light_pulse_experiments_20230407_sex.pdf", width = 25, height = 7.5)
ave.all.7days
dev.off()

