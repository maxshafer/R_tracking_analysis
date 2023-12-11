
# Inserts rest column based on activity in rolling 60 sec windows (default = < 5% movement above 15 mm/s)
getRest <- function(als_data = als_data, threshold = 15, pct = 0.05) {
  require("runner")
  tic("rest calculated")
  
  als_data <- als_data[!is.na(als_data$speed_mm),]
  als_data <- tail(als_data, -1)
  
  
  cutoff = pct*600 
  als_data$rest <- runner(als_data$speed_mm, k = 600, f = function(x) sum(x > threshold) < cutoff, lag = -300)
  
  als_data <- tail(als_data, -300)
  als_data <- head(als_data, -300)
  toc()
  
  return(als_data)
}


# Inserts phase (dawn/day/dusk/night) for each observation, day/night times can be adjusted but defaulted to 7am and 7pm
getPhase <- function(als_data = als_data, day = "7:00:00", night = "19:00:00") {
  tic("phase determined")
  
  day <- as.POSIXct(paste('1970-01-01 ', day), format = "%Y-%m-%d %H:%M:%S", origin = "", tz = "GMT")
  night <- as.POSIXct(paste('1970-01-01 ', night), format = "%Y-%m-%d %H:%M:%S", origin = "", tz = "GMT")
  
  dawn_start <- format(day - hours(1), format="%H:%M:%S")
  dawn_end <- format(day + hours(1), format="%H:%M:%S")
  
  dusk_start <- format(night - hours(1), format="%H:%M:%S")
  dusk_end <- format(night + hours(1), format="%H:%M:%S")
  
  als_data <- mutate(als_data, phase = case_when(between(format(as.POSIXct(datetime), format="%H:%M:%S"), dusk_start, dusk_end) ~ 'dusk',
                                                 between(format(as.POSIXct(datetime), format="%H:%M:%S"), dawn_start, dawn_end) ~ 'dawn',
                                                 between(format(as.POSIXct(datetime), format="%H:%M:%S"), dawn_end, dusk_start) ~ 'day',
                                                 .default = 'night'))
  
  toc()
  
  return(als_data)
}

getDiel <- function(als_data = als_data) {
  day <- als_data[which(als_data$phase == "day"), ]
  day <- mean(day$speed_mm)
  
  night <- als_data[which(als_data$phase == "night"), ]
  night <- mean(night$speed_mm)
  
  als_data <- mutate(als_data, diel = (day - night)/(day + night))
  return(als_data)

}

# Turns string into datetime object, keeping only the time if format = TRUE
parseTime <- function(time = character, format = logical) {
  time <- as.POSIXct(paste('1970-01-01 ', time), format = "%Y-%m-%d %H:%M:%S", origin = "", tz = "GMT")
  
  if (format == TRUE) {
    time <- format(time, format="%H:%M:%S")
  } 
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


findBoutPhase <- function(start_time, start_phase, end_time, end_phase, day = "07:00:00", night = "19:00:00") {
  day <- as.POSIXct(paste(format(start_time, "%Y-%m-%d"), day), format = "%Y-%m-%d %H:%M:%S", origin = "", tz = "GMT")
  night <- as.POSIXct(paste(format(start_time, "%Y-%m-%d"), night), format = "%Y-%m-%d %H:%M:%S", origin = "", tz = "GMT")
  
  start_time <- as.POSIXct(start_time, tz = "GMT")
  end_time <- as.POSIXct(end_time, tz = "GMT")
  
  dawn_start <- day - hours(1)
  dawn_end <- day + hours(1)
  
  dusk_start <- night - hours(1)
  dusk_end <- night + hours(1)
  
  if (start_phase == end_phase) { return(start_phase) }
  else if (start_phase == "dawn" & end_phase == "dusk" | end_phase == "night") { return("day") }
  else if (start_phase == "dusk" & end_phase == "dawn" | end_phase == "day") { return("night") }
  else if (start_phase == "night" & end_phase == "dusk") { return("day") }
  else if (start_phase == "day" & end_phase == "dawn") { return("night") }
  else {
    phase_seq <- c("night", "dawn", "day", "dusk", "night")
    end_times <- c(dawn_start, dawn_end, dusk_start, dusk_end, dawn_start)
    if (day(start_time) < day(end_time)) { }
    
    if (start_phase == "night" & end_phase == "day") { next_phase_time <- day }
    else if (start_phase == "day" & end_phase == "night") { next_phase_time <- night }
    else { next_phase_time <- end_times[which(phase_seq == start_phase)[1]] }
    
    if (day(start_time) < day(end_time)) { day(next_phase_time) <- day(next_phase_time) + 1 }
    phase <- ifelse(abs(as.numeric(difftime(start_time, next_phase_time, units = "secs"))) > 
                    abs(as.numeric(difftime(end_time, next_phase_time, units = "secs"))), 
                    start_phase, end_phase)
  }
  
  return(phase)
}

# Creates new dataframe with the bout structure (length, start and end times, and phases)
boutStructure <- function(als_data = als_data) {
  tic("bout structure determined")
  als_data$rest <- ifelse(als_data$rest == TRUE, "rest", "active")
  
  seq <- rle(als_data$rest)
  bouts = data.frame(state = seq$values,
                     length = seq$lengths)

  bouts$start[1] <- als_data$datetime[1]
  bouts$start_phase <- als_data$phase[1]

  time_elapsed = 0
  
  # Assign start and end times and phases
  for (i in 1:nrow(bouts)) {
    time_elapsed <- time_elapsed + bouts$length[i]
    
    bouts$end[i] <- als_data$datetime[time_elapsed]
    bouts$end_phase[i] <- als_data$phase[time_elapsed]
    
    if (i != nrow(bouts)) {
      bouts$start[i + 1] <- als_data$datetime[time_elapsed + 1]
      bouts$start_phase[i + 1] <- als_data$phase[time_elapsed + 1]
    }
    
    else { 
      last_bout_index <- nrow(als_data) - bouts$length[i] + 1
      bouts$start[i] <- als_data$datetime[last_bout_index]
      bouts$start_phase[i] <- als_data$phase[last_bout_index]
    }
    
  }
  
  bouts$start <- as.POSIXct(bouts$start, origin = "1970-01-01 00:00:00", tz = "GMT")
  bouts$end <- as.POSIXct(bouts$end, origin = "1970-01-01 00:00:00", tz = "GMT")
  
  bouts <- mutate(bouts, length = as.numeric(difftime(bouts$end, bouts$start, unit = "secs")))
  
  bouts$sample_id <- unique(als_data$sample_id)
  bouts$species_six <- unique(als_data$species_six)
  bouts$tribe <- unique(als_data$tribe)
  bouts$diel <- als_data$diel[1]
  
  bouts$overall_phase <- mapply(findBoutPhase, bouts$start, bouts$start_phase, bouts$end, bouts$end_phase)
  
  total <- sum(bouts$length)
  bouts$proportion <- bouts$length/total
  toc()
  
  return(bouts)
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
  
  bout_data <- group_by(bout_data, day(bout_data$start), species_six, sample_id, state, overall_phase)
  avg_daily <- summarise(bout_data, total_sec = sum(length), total_hour = total_sec/3600, 
                         freq = length(state), mean_length = mean(length), median_length = median(length), sfi = freq/total_hour,
                         L50 = L50consolidation(length), N50 = N50consolidation(length), proportion = sum(proportion), diel = mean(diel),
                         most_awakenings = mode(start_phase))
  
  names(avg_daily)[1] <- "day"
  
  toc()
  
  return (avg_daily)
}

weekSummary <- function(avg_daily = avg_daily) {
  tic("week summarised")
  
  avg_daily <- group_by(avg_daily, species_six, sample_id, state, overall_phase)
  
  avg_week <- summarise(avg_daily, avg_total = mean(total_hour), avg_freq = mean(freq), 
                        avg_length = mean(mean_length), sfi = mean(sfi), avg_L50 = mean(L50), avg_N50 = mean(N50), 
                        avg_proportion = mean(proportion), diel = mean(diel), most_awakenings = mode(start_phase))
  
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
restData <- function(als_data = als_data, speed_threshold = 15, pct_movement = 0.05, day = "07:00:00", night = "19:00:00") {
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
  
  return(rest_data)
}

