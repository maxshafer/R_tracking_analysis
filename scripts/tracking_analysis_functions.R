## Functions for analysing data that is output by annic's python fish tracking software
## Maxwell E.R. Shafer 2023
## Last updated 03/02/2023

## This funtion takes a file path of an als file, loads it into memory, and summarises it by second, minute (default behaviour), halfhour, or hour
## Returns the summarised data, labelled with sample_id

loadALSfiles <- function(path_to_file = file_path, average_by = c("second", "minute", "halfhour", "hour"), datetime_origin = "1970-01-01 00:00:00", normalise = "both") {
  require("tictoc")
  require("data.table")
  require("stringr")
  
  if (!(average_by %in% c("second", "minute", "halfhour", "hour"))) {
    stop("'average_by' must be 'second', 'minute', 'halfhour', or 'hour'")
  }
  
  print(paste("processing file", path_to_file, sep = " "))
  
  # Load in als files using fread
  tic("als file loaded")
  table <- fread(path_to_file, showProgress = FALSE)
  toc()
  
  # Add datetime
  tic("coverted timestamp to datetime")
  table$datetime <- as.POSIXct(table$tv_ns/1e9, origin = datetime_origin, tz = "GMT")
  toc()
  
  # Now summarise by datetime
  # The mutating takes the longest, and can be shortened if you don't need that info ()
  tic("summarised data")
  if (average_by == "halfhour") {
    # Takes much longer to do the half_hour, so if you don't want to do it at this point, then the function forgoes it
    output <- table %>% mutate(second = second(datetime), minute = minute(datetime), half_hour = floor_date(datetime, "30 minutes"), hour = hour(datetime), day = day(datetime))
  } else {
    output <- table %>% mutate(second = second(datetime), minute = minute(datetime), hour = hour(datetime), day = day(datetime))
  }
  
  if (average_by == "hour") { summarised <- output %>% group_by(day, hour) }
  if (average_by == "halfhour") { summarised <- output %>% group_by(day, hour, half_hour) }
  if (average_by == "minute") { summarised <- output %>% group_by(day, hour, minute) }
  if (average_by == "second") { summarised <- output %>% group_by(day, hour, minute, second) }
  
  summarised <- summarised %>% summarise(mean_speed_mm = mean(speed_mm), mean_x_nt = mean(x_nt), mean_y_nt = mean(y_nt))
  toc()
  
  ## Now need to convert back to a datetime
  if (average_by == "hour") { summarised$datetime <- paste("1970-01-0", summarised$day, " ", summarised$hour, ":00", ":00", sep = "") }
  if (average_by == "halfhour") { ssummarised$datetime <- summarised$half_hour }
  if (average_by == "minute") { summarised$datetime <- paste("1970-01-0", summarised$day, " ", summarised$hour, ":", summarised$minute, ":00", sep = "") }
  if (average_by == "second") { summarised$datetime <- paste("1970-01-0", summarised$day, " ", summarised$hour, ":", summarised$minute, ":", summarised$second, sep = "") }
  
  summarised$datetime <- as.POSIXct(summarised$datetime, '%Y-%m-%d %H:%M:%S', tz = "GMT")
  
  ## Add sample ID
  summarised$sample_id <- str_extract(path_to_file, pattern = "FISH........_c._r.")
  

  ## Normalize x and y coordinates
  if (normalise %in% c("both", "x")) {
    summarised$mean_x_nt <- summarised$mean_x_nt/max(summarised$mean_x_nt)
    if (normalise == "both") {
      summarised$mean_y_nt <- summarised$mean_y_nt/max(summarised$mean_y_nt)
    }
  }
  
  if (normalise == "y") {
    summarised$mean_y_nt <- summarised$mean_y_nt/max(summarised$mean_y_nt)
  }
  
  # This reverses the order of y_coordiantes (higher numbers in original data are closer to the bottom)
  summarised$mean_y_nt <- (summarised$mean_y_nt - max(summarised$mean_y_nt))*-1
  
  return(summarised)
}


## This function takes als data that is already loaded into memory, and summarises it further
## For example, if you have data averaged by minute, this function can average it by halfhour, or hour
## Still returns all days

summariseALSdata <- function(als_data = als_data, average_by = c("minute", "halfhour", "hour"), datetime_origin = "1970-01-01 00:00:00") {
  
  if (!(average_by %in% c("minute", "halfhour", "hour"))) {
    stop("'average_by' must be 'minute', 'halfhour', or 'hour'")
  }
  
  if (average_by == "halfhour") {
    # Takes much longer to do the half_hour, so if you don't want to do it at this point, then the function forgoes it
    als_data <- als_data %>% mutate(half_hour = floor_date(datetime, "30 minutes"))
  }
  
  if (average_by == "hour") { summarised <- als_data %>% group_by(day, hour) }
  if (average_by == "halfhour") { summarised <- als_data %>% group_by(day, hour, half_hour) }
  if (average_by == "minute") { summarised <- als_data %>% group_by(day, hour, minute) }

  summarised <- summarised %>% summarise(mean_speed_mm = mean(mean_speed_mm), mean_x_nt = mean(mean_x_nt), mean_y_nt = mean(mean_y_nt))

  ## Now need to convert back to a datetime
  if (average_by == "hour") { summarised$datetime <- paste("1970-01-0", summarised$day, " ", summarised$hour, ":00", ":00", sep = "") }
  if (average_by == "halfhour") { summarised$datetime <- summarised$half_hour }
  if (average_by == "minute") { summarised$datetime <- paste("1970-01-0", summarised$day, " ", summarised$hour, ":", summarised$minute, ":00", sep = "") }

  summarised$datetime <- as.POSIXct(summarised$datetime, '%Y-%m-%d %H:%M:%S', tz = "GMT")
  summarised$sample_id <- unique(als_data$sample_id)
  
  return(summarised)
}

## This function averages across days, in units of either seconds, minutes, halfhours, or hours
## Takes as input the als.data
averageDay <- function(als_data = als_data, units = c("second", "minute", "halfhour", "hour"), date_origin = "1970-01-01", days_include = "all") {
  
  if (!(units %in% c("second", "minute", "halfhour", "hour"))) {
    stop("'unit' must be 'second', 'minute', 'halfhour', or 'hour'")
  }
  
  if (units == "halfhour") {
    # Takes much longer to do the half_hour, so if you don't want to do it at this point, then the function forgoes it
    als_data <- als_data %>% mutate(half_hour = floor_date(datetime, "30 minutes"))
    als_data <- als_data %>% mutate(half_hour = minute(half_hour))
  }
  
  if (days_include[1] != "all") {
    if (!(is.numeric(days_include))) {
      stop("'days_include' must be a numeric vector")
    }
    # If there is only one entry for day 1, assume the user hasn't consider it
    if (nrow(als_data[als_data[,"day"] %in% 1]) == 1) {
      days_include <- days_include + 1
    }
    als_data <- als_data[als_data$day %in% days_include,]
  }
  
  if (units == "hour") { summarised <- als_data %>% group_by(hour) }
  if (units == "halfhour") { summarised <- als_data %>% group_by(hour, half_hour) }
  if (units == "minute") { summarised <- als_data %>% group_by(hour, minute) }
  
  summarised <- summarised %>% summarise(mean_speed_mm = mean(mean_speed_mm), mean_x_nt = mean(mean_x_nt), mean_y_nt = mean(mean_y_nt))
  
  ## Now need to convert back to a datetime
  if (units == "hour") { summarised$datetime <- paste(date_origin, " ", summarised$hour, ":00", ":00", sep = "") }
  if (units == "halfhour") { summarised$datetime <- paste(date_origin, " ", summarised$hour, ":", summarised$half_hour, ":00", sep = "") }
  if (units == "minute") { summarised$datetime <- paste(date_origin, " ", summarised$hour, ":", summarised$minute, ":00", sep = "") }
  
  summarised$datetime <- as.POSIXct(summarised$datetime, '%Y-%m-%d %H:%M:%S', tz = "GMT")
  summarised$sample_id <- unique(als_data$sample_id)
  
  return(summarised)
  
}


## These functions add colours based on the time of day to a ggplot

shade_colours <- function(x = x, ...) {
  scale_fill_manual(values = c("night" = "grey", "dawn" = "yellow", "day" = "white", "dusk" = "yellow", "dark_dark" = "grey85"))
}

geom_rect_shading_bz <- function(new_bz_times = TRUE, ...) {
  rects_day_newbz <- data.frame(xstart = c(as.POSIXct("1970-01-01 00:00:00", '%Y-%m-%d %H:%M:%S', tz = "GMT"), 
                                           as.POSIXct("1970-01-01 08:00:00", '%Y-%m-%d %H:%M:%S', tz = "GMT"),
                                           as.POSIXct("1970-01-01 08:30:00", '%Y-%m-%d %H:%M:%S', tz = "GMT"),
                                           as.POSIXct("1970-01-01 21:30:00", '%Y-%m-%d %H:%M:%S', tz = "GMT"),
                                           as.POSIXct("1970-01-01 22:00:00", '%Y-%m-%d %H:%M:%S', tz = "GMT")), 
                                xend = c(as.POSIXct("1970-01-01 07:59:59", '%Y-%m-%d %H:%M:%S', tz = "GMT"), 
                                         as.POSIXct("1970-01-01 08:29:59", '%Y-%m-%d %H:%M:%S', tz = "GMT"),
                                         as.POSIXct("1970-01-01 21:59:00", '%Y-%m-%d %H:%M:%S', tz = "GMT"),
                                         as.POSIXct("1970-01-01 21:59:59", '%Y-%m-%d %H:%M:%S', tz = "GMT"),
                                         as.POSIXct("1970-01-01 23:59:59", '%Y-%m-%d %H:%M:%S', tz = "GMT")), 
                                col = c("night",
                                        "dawn",
                                        "day",
                                        "dusk",
                                        "night"))
  geom_rect(data = rects_day_newbz, aes(ymin=-Inf, ymax=Inf, xmin=xstart, xmax=xend, fill=col), alpha =0.5, inherit.aes = FALSE)
}

geom_rect_shading_zoo <- function(zoological_times = FALSE, ...) {
  rects_day_zoo <- data.frame(xstart = c(as.POSIXct("1970-01-01 00:00:00", '%Y-%m-%d %H:%M:%S', tz = "GMT"), 
                                         as.POSIXct("1970-01-01 07:00:00", '%Y-%m-%d %H:%M:%S', tz = "GMT"),
                                         as.POSIXct("1970-01-01 07:30:00", '%Y-%m-%d %H:%M:%S', tz = "GMT"),
                                         as.POSIXct("1970-01-01 18:30:00", '%Y-%m-%d %H:%M:%S', tz = "GMT"),
                                         as.POSIXct("1970-01-01 19:00:00", '%Y-%m-%d %H:%M:%S', tz = "GMT")), 
                              xend = c(as.POSIXct("1970-01-01 06:59:59", '%Y-%m-%d %H:%M:%S', tz = "GMT"), 
                                       as.POSIXct("1970-01-01 07:29:59", '%Y-%m-%d %H:%M:%S', tz = "GMT"),
                                       as.POSIXct("1970-01-01 18:59:00", '%Y-%m-%d %H:%M:%S', tz = "GMT"),
                                       as.POSIXct("1970-01-01 18:59:59", '%Y-%m-%d %H:%M:%S', tz = "GMT"),
                                       as.POSIXct("1970-01-01 23:59:59", '%Y-%m-%d %H:%M:%S', tz = "GMT")), 
                              col = c("night",
                                      "dawn",
                                      "day",
                                      "dusk",
                                      "night"))
  geom_rect(data = rects_day_zoo, aes(ymin=-Inf, ymax=Inf, xmin=xstart, xmax=xend, fill=col), alpha =0.5, inherit.aes = FALSE, fill = c("grey", "yellow", "white", "yellow", "grey"))
}

geom_rect_shading_bz_7days <- function(new_bz_times = TRUE, ...) {
  rects_day_newbz <- data.frame(xstart = c(as.POSIXct("1970-01-01 00:00:00", '%Y-%m-%d %H:%M:%S', tz = "GMT"), 
                                           as.POSIXct("1970-01-01 08:00:00", '%Y-%m-%d %H:%M:%S', tz = "GMT"),
                                           as.POSIXct("1970-01-01 08:30:00", '%Y-%m-%d %H:%M:%S', tz = "GMT"),
                                           as.POSIXct("1970-01-01 21:30:00", '%Y-%m-%d %H:%M:%S', tz = "GMT"),
                                           as.POSIXct("1970-01-01 22:00:00", '%Y-%m-%d %H:%M:%S', tz = "GMT")), 
                                xend = c(as.POSIXct("1970-01-01 07:59:59", '%Y-%m-%d %H:%M:%S', tz = "GMT"), 
                                         as.POSIXct("1970-01-01 08:29:59", '%Y-%m-%d %H:%M:%S', tz = "GMT"),
                                         as.POSIXct("1970-01-01 21:59:00", '%Y-%m-%d %H:%M:%S', tz = "GMT"),
                                         as.POSIXct("1970-01-01 21:59:59", '%Y-%m-%d %H:%M:%S', tz = "GMT"),
                                         as.POSIXct("1970-01-01 23:59:59", '%Y-%m-%d %H:%M:%S', tz = "GMT")), 
                                col = c("night",
                                        "dawn",
                                        "day",
                                        "dusk",
                                        "night"))
  
  rects <- Reduce(rbind, list(rects_day_newbz, rects_day_newbz, rects_day_newbz, rects_day_newbz, rects_day_newbz, rects_day_newbz, rects_day_newbz, rects_day_newbz))
  rects$xstart <- as.POSIXct(rects$xstart, '%Y-%m-%d %H:%M:%S', tz = "GMT", origin = "1970-01-01 00:00:00")
  rects$xend <- as.POSIXct(rects$xend, '%Y-%m-%d %H:%M:%S', tz = "GMT", origin = "1970-01-01 00:00:00")
  day(rects$xstart) <- c(rep(01, 5), rep(02, 5), rep(03, 5), rep(04, 5), rep(05, 5), rep(06, 5), rep(07, 5), rep(08, 5))
  day(rects$xend) <- c(rep(01, 5), rep(02, 5), rep(03, 5), rep(04, 5), rep(05, 5), rep(06, 5), rep(07, 5), rep(08, 5))
  rects <- rects[5:36,]
  
  geom_rect(data = rects, aes(ymin=-Inf, ymax=Inf, xmin=xstart, xmax=xend, fill=col), alpha =0.5, inherit.aes = FALSE)
}

geom_rect_shading_bz_7days_darkdark <- function(new_bz_times = TRUE, ...) {
  rects_day_newbz <- data.frame(xstart = c(as.POSIXct("1970-01-01 00:00:00", '%Y-%m-%d %H:%M:%S', tz = "GMT"), 
                                           as.POSIXct("1970-01-01 08:00:00", '%Y-%m-%d %H:%M:%S', tz = "GMT"),
                                           as.POSIXct("1970-01-01 08:30:00", '%Y-%m-%d %H:%M:%S', tz = "GMT"),
                                           as.POSIXct("1970-01-01 21:30:00", '%Y-%m-%d %H:%M:%S', tz = "GMT"),
                                           as.POSIXct("1970-01-01 22:00:00", '%Y-%m-%d %H:%M:%S', tz = "GMT")), 
                                xend = c(as.POSIXct("1970-01-01 07:59:59", '%Y-%m-%d %H:%M:%S', tz = "GMT"), 
                                         as.POSIXct("1970-01-01 08:29:59", '%Y-%m-%d %H:%M:%S', tz = "GMT"),
                                         as.POSIXct("1970-01-01 21:59:00", '%Y-%m-%d %H:%M:%S', tz = "GMT"),
                                         as.POSIXct("1970-01-01 21:59:59", '%Y-%m-%d %H:%M:%S', tz = "GMT"),
                                         as.POSIXct("1970-01-01 23:59:59", '%Y-%m-%d %H:%M:%S', tz = "GMT")), 
                                col = c("night",
                                        "dawn",
                                        "day",
                                        "dusk",
                                        "night"))
  
  rects_day_newbz_darkdark <- data.frame(xstart = c(as.POSIXct("1970-01-01 00:00:00", '%Y-%m-%d %H:%M:%S', tz = "GMT"), 
                                           as.POSIXct("1970-01-01 08:00:00", '%Y-%m-%d %H:%M:%S', tz = "GMT"),
                                           as.POSIXct("1970-01-01 08:30:00", '%Y-%m-%d %H:%M:%S', tz = "GMT"),
                                           as.POSIXct("1970-01-01 21:30:00", '%Y-%m-%d %H:%M:%S', tz = "GMT"),
                                           as.POSIXct("1970-01-01 22:00:00", '%Y-%m-%d %H:%M:%S', tz = "GMT")), 
                                xend = c(as.POSIXct("1970-01-01 07:59:59", '%Y-%m-%d %H:%M:%S', tz = "GMT"), 
                                         as.POSIXct("1970-01-01 08:29:59", '%Y-%m-%d %H:%M:%S', tz = "GMT"),
                                         as.POSIXct("1970-01-01 21:59:00", '%Y-%m-%d %H:%M:%S', tz = "GMT"),
                                         as.POSIXct("1970-01-01 21:59:59", '%Y-%m-%d %H:%M:%S', tz = "GMT"),
                                         as.POSIXct("1970-01-01 23:59:59", '%Y-%m-%d %H:%M:%S', tz = "GMT")), 
                                col = c("night",
                                        "dawn",
                                        "dark_dark",
                                        "dusk",
                                        "night"))
  
  rects <- Reduce(rbind, list(rects_day_newbz, rects_day_newbz, rects_day_newbz, rects_day_newbz, rects_day_newbz_darkdark, rects_day_newbz_darkdark, rects_day_newbz_darkdark, rects_day_newbz_darkdark))
  rects$xstart <- as.POSIXct(rects$xstart, '%Y-%m-%d %H:%M:%S', tz = "GMT", origin = "1970-01-01 00:00:00")
  rects$xend <- as.POSIXct(rects$xend, '%Y-%m-%d %H:%M:%S', tz = "GMT", origin = "1970-01-01 00:00:00")
  day(rects$xstart) <- c(rep(01, 5), rep(02, 5), rep(03, 5), rep(04, 5), rep(05, 5), rep(06, 5), rep(07, 5), rep(08, 5))
  day(rects$xend) <- c(rep(01, 5), rep(02, 5), rep(03, 5), rep(04, 5), rep(05, 5), rep(06, 5), rep(07, 5), rep(08, 5))
  rects <- rects[5:36,]
  
  geom_rect(data = rects, aes(ymin=-Inf, ymax=Inf, xmin=xstart, xmax=xend, fill=col), alpha =0.5, inherit.aes = FALSE)
}

geom_rect_shading_zoo_7days <- function(zoological_times = FALSE, ...) {
  rects_day_zoo <- data.frame(xstart = c(as.POSIXct("1970-01-01 00:00:00", '%Y-%m-%d %H:%M:%S', tz = "GMT"), 
                                         as.POSIXct("1970-01-01 07:00:00", '%Y-%m-%d %H:%M:%S', tz = "GMT"),
                                         as.POSIXct("1970-01-01 07:30:00", '%Y-%m-%d %H:%M:%S', tz = "GMT"),
                                         as.POSIXct("1970-01-01 18:30:00", '%Y-%m-%d %H:%M:%S', tz = "GMT"),
                                         as.POSIXct("1970-01-01 19:00:00", '%Y-%m-%d %H:%M:%S', tz = "GMT")), 
                              xend = c(as.POSIXct("1970-01-01 06:59:59", '%Y-%m-%d %H:%M:%S', tz = "GMT"), 
                                       as.POSIXct("1970-01-01 07:29:59", '%Y-%m-%d %H:%M:%S', tz = "GMT"),
                                       as.POSIXct("1970-01-01 18:59:00", '%Y-%m-%d %H:%M:%S', tz = "GMT"),
                                       as.POSIXct("1970-01-01 18:59:59", '%Y-%m-%d %H:%M:%S', tz = "GMT"),
                                       as.POSIXct("1970-01-01 23:59:59", '%Y-%m-%d %H:%M:%S', tz = "GMT")), 
                              col = c("night",
                                      "dawn",
                                      "day",
                                      "dusk",
                                      "night"))
  
  rects <- Reduce(rbind, list(rects_day_zoo, rects_day_zoo, rects_day_zoo, rects_day_zoo, rects_day_zoo, rects_day_zoo, rects_day_zoo, rects_day_zoo))
  rects$xstart <- as.POSIXct(rects$xstart, '%Y-%m-%d %H:%M:%S', tz = "GMT", origin = "1970-01-01 00:00:00")
  rects$xend <- as.POSIXct(rects$xend, '%Y-%m-%d %H:%M:%S', tz = "GMT", origin = "1970-01-01 00:00:00")
  day(rects$xstart) <- c(rep(01, 5), rep(02, 5), rep(03, 5), rep(04, 5), rep(05, 5), rep(06, 5), rep(07, 5), rep(08, 5))
  day(rects$xend) <- c(rep(01, 5), rep(02, 5), rep(03, 5), rep(04, 5), rep(05, 5), rep(06, 5), rep(07, 5), rep(08, 5))
  rects <- rects[5:36,]
  
  geom_rect(data = rects, aes(ymin=-Inf, ymax=Inf, xmin=xstart, xmax=xend, fill=col), alpha =0.5, inherit.aes = FALSE, fill = c("grey", "yellow", "white", "yellow", "grey"))
}


