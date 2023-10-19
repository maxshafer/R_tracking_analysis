
# Inserts rest column based on activity in rolling 60 sec windows (default = < 5% movement above 15 mm/s)
getRest <- function(als_data = als_data, threshold = 15, pct = 0.05) {
  require("runner")
  tic("rest calculated")
  cutoff = pct*60 
  
  als_data <- als_data[!is.na(als_data$mean_speed_mm),]
  als_data <- tail(als_data, -1)
  
  als_data$rest <- runner(als_data$mean_speed_mm, k = 60, f = function(x) sum(x < threshold) > cutoff, lag = -30)
  als_data <- tail(data_subset, -30)
  als_data <- head(data_subset, -30)
  toc()
  
  return (als_data)
}

# Inserts phase (dawn/day/dusk/night) for each observation, day/night times can be adjusted but defaulted to 7am and 7pm
getPhase <- function(als_data = als_data, day = "7:00", night = "19:00") {
  tic("phase determined")
  
  day <- parseTime(day, FALSE)
  night <- parseTime(night, FALSE)
  
  dawn_start <- format(day - hours(1), format="%H:%M")
  dawn_end <- format(day + hours(1), format="%H:%M")
  
  dusk_start <- format(night - hours(1), format="%H:%M")
  dusk_end <- format(night + hours(1), format="%H:%M")
  
  phase <- lapply(als_data$datetime, function(x) 
                  if (between(parseTime(x, TRUE), dawn_start, dawn_end)) { "dawn" }
                  else if (between(parseTime(x, TRUE), dusk_start, dusk_end)) { "dusk" }
                  else if (parseTime(x, TRUE) >= dawn_end & parseTime(x, TRUE) <= dusk_start) { "day" }
                  else { "night" })
  
  als_data$phase <- cbind(als_data, phase)
  toc()
  
  return(als_data)
}

# Turns string into datetime object, keeping only the time if format = TRUE
parseTime <- function(time = character, format = logical) {
  ifelse(format == TRUE, time <- format(as.POSIXct(time, origin = "1970-01-01 00:00:00", format="%H:%M", tz="GMT"), format="%H:%M"), 
         time <- as.POSIXct(time, origin = "1970-01-01 00:00:00", format="%H:%M", tz="GMT"))
  return(time)
}

# Calculates total rest by phase
totalRest <- function(als_data = als_data, units = c("second", "minute", "hour"), 
                      phase = c("day", "night", "dawn", "dusk", "all")) {
  total <- 0
  
  if (phase == "all") { total <- sum(als_data$rest == TRUE) }
  else { total <- sum(als_data$rest == TRUE & als_data$phase == phase) }
  
  if (units == "hour") { total <- total/3600 }
  if (units == "minute") { total <- total/60 }

  return (total)
}

# Creates new dataframe with the bout structure (length, start and end times, and phases)
boutStructure <- function(als_data = als_data) {
  tic("bout structure determined")
  rest_seq <- character(0)
  bout_lengths <- integer(0)
  
  bouts = data.frame(state = rest_seq, 
                     length = bout_lengths)
  
  seq <- rle(als_data$rest)
  rest_seq <- c(rest_seq, seq$values)
  bout_lengths <- c(bout_lengths, seq$lengths)
  
  bouts$species_six[1] <- als_data$species_six[1]
  bouts$sample_id[1] <- als_data$sample_id[1]
  bouts$start[1] <- als_data$datetime[1]
  bouts$start_phase[1] <- als_data$phase[1]

  time_elapsed = 1
  
  # Assign start and end times and phases
  for (i in 1:nrow(bouts)) {
    time_elapsed <- time_elapsed + bouts$length[i]
    bouts$species_six[i] <- als_data$species_six[time_elapsed]
    bouts$sample_id[i] <- als_data$sample_id[time_elapsed]
    
    bouts$end[i] <- als_data$datetime[time_elapsed]
    bouts$end_phase[i] <- als_data$phase[time_elapsed]
    
    if (i + 1 < nrow(bouts)) { 
      bouts$start[i + 1] <- bouts$end[i] 
      bouts$start_phase[i + 1] <- bouts$end_phase[i] 
    }
    
  }
  
  bouts$start <- as.POSIXct(bouts$start, origin = "1970-01-01 00:00:00", tz = "GMT")
  bouts$end <- as.POSIXct(bouts$end, origin = "1970-01-01 00:00:00", tz = "GMT")
  toc()
  
  return(head(bouts, -1))
}

filterBouts <- function(bout_data = bout_data, threshold = 3) {
  # Remove all bouts below threshold
  bouts <- filter(bout_data, bout_data$length > threshold)
  
  state_seq <- bouts$state
  state_length <- (rle.state_seq = rle(state_seq)$lengths)
  
  # Adjust bout lengths after removing short bouts
  for (i in 1:length(state_length)) {
    if (state_length[i] > 1) { 
      bouts$end[i] <- bouts$end[i - 1 + state_length[i]]
      bouts$length[i] <- difftime(bouts$end[i], bouts$start[i], unit = "secs")
      ifelse(state_length[i] == 2, bouts <- bouts[-(i + 1),], bouts <- bouts[-((i + 1):(i - 1 + state_length[i])),]) 
    }
  }
  
  return(head(bouts, -1))
}

# Returns table with daily summaries of total rest, bout frequency and phase ratio 
boutSummary <- function(bout_data = bout_data) {
  tic("bouts summarised")
  
  bout_data <- group_by(bout_data, day(bout_data$start), species_six, sample_id, state, start_phase)
  avg_daily <- summarise(bout_data, total_sec = sum(length), total_hour = total_sec/3600, 
                         freq = length(state), mean_length = mean(length), median_length = median(length), sfi = freq/total_hour,
                         L50 = L50consolidation(length), N50 = N50consolidation(length))
  
  names(avg_daily)[1] <- "day"
  
  toc()
  
  return (avg_daily)
}

weekSummary <- function(avg_daily = avg_daily) {
  tic("week summarised")
  
  avg_daily <- group_by(avg_daily, species_six, sample_id, state, start_phase)
  
  avg_week <- summarise(avg_daily, avg_total = mean(total_hour), avg_freq = mean(freq), 
                        avg_length = mean(mean_length), sfi = mean(sfi),
                        avg_L50 = L50consolidation(mean_length), avg_N50 = N50consolidation(mean_length))
  
  toc()
  return(avg_week)
}

N50consolidation <- function(bout_lengths = bout_lengths) {
  
  bout_lengths <- sort(bout_lengths, TRUE)
  
  bout_sum <- cumsum(bout_lengths)
  bout_ind <- which(bout_sum > sum(bout_lengths)*0.5)
  
  L <- min(bout_ind)
  
  return(L)
}

L50consolidation <- function(bout_lengths = bout_lengths) {
  return(bout_lengths[N50consolidation(bout_lengths)])
}


# Write CSVs for bout data
restData <- function(als_data = als_data, speed_threshold = 15, pct_movement = 0.05, day = "7:00", night = "19:00") {
  require("tictoc")
  
  als_data <- getRest(als_data, threshold = speed_threshold, pct = pct_movement)
  als_data <- getPhase(als_data, day, night)

  rest_data <- vector(mode='list', length=3)
  
  rest_data$bout_data <- boutStructure(als_data)
  rest_data$daily_summary <- boutSummary(rest_data$bout_data)
  rest_data$weekly_summary <- weekSummary(rest_data$daily_summary)
  
  write.csv(rest_data$bout_data, paste("/home/ayasha/projects/def-mshafer/ayasha/cichlid_bout_data/", als_data$sample_id[1], "_bout_structure.csv", sep = ""))
  write.csv(rest_data$daily_summary, paste("/home/ayasha/projects/def-mshafer/ayasha/cichlid_bout_data/", als_data$sample_id[1], "_daily_bout_summary.csv", sep = ""))
  write.csv(rest_data$weekly_summary, paste("/home/ayasha/projects/def-mshafer/ayasha/cichlid_bout_data/", als_data$sample_id[1], "_weekly_bout_summary.csv", sep = ""))
  
  #return(rest_data)
}
