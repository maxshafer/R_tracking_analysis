### These functions require python on the local machine with scipy
### These are required for finding peaks, but nothing else

## Run Python functions to find peaks
## Remember, python is 0 indexed, so add 1 to the indices

require(reticulate)
scipy.signal <- import("scipy.signal")

## This function finds peaks using scipy.signal$find_peaks
## Returns an als data frame with peaks labelled along with their prominences

findPeaks <- function(als_data = als, distance = 4, prominence = 7) {
  # This uses scipy to find peaks and returns something with a unique structure
  peaks <- scipy.signal$find_peaks(x = als_data$mean_speed_mm, distance = distance, prominence = prominence)

  als_data$peaks <- "no"
  als_data$peaks[peaks[[1]]+1] <- "yes"
  als_data$peak_prominence <- NA
  als_data$peak_prominence[peaks[[1]]+1] <- peaks[[2]]$prominences

  return(als_data)
}


## This function takes als_data (for one individual) that already contains whether each entry is a peak (regular input is half_hour averaged)
## Intervals are kept inside (might slow it down, but keeps environment clear)
returnPeakPercentages <- function(als_data = als_data, zoo_times = FALSE, avg_days = FALSE) {

  dawn_intervals <- list()
  dusk_intervals <- list()

  if(zoo_times) {
    for (i in 02:08) {
      dawn_intervals[[i]] <- interval(as.POSIXct(paste("1970-01-", i, " 06:00:00", sep = ""), tz = "GMT"), as.POSIXct(paste("1970-01-", i, " 07:30:00", sep = ""), tz = "GMT"))
      dusk_intervals[[i]] <- interval(as.POSIXct(paste("1970-01-", i, " 18:00:00", sep = ""), tz = "GMT"), as.POSIXct(paste("1970-01-", i, " 19:30:00", sep = ""), tz = "GMT"))
    }
  } else {
    for (i in 02:08) {
      dawn_intervals[[i]] <- interval(as.POSIXct(paste("1970-01-", i, " 07:00:00", sep = ""), tz = "GMT"), as.POSIXct(paste("1970-01-", i, " 08:30:00", sep = ""), tz = "GMT"))
      dusk_intervals[[i]] <- interval(as.POSIXct(paste("1970-01-", i, " 21:00:00", sep = ""), tz = "GMT"), as.POSIXct(paste("1970-01-", i, " 22:30:00", sep = ""), tz = "GMT"))
    }
  }

  dawn_intervals <- dawn_intervals[2:8]
  dusk_intervals <- dusk_intervals[2:8]

  if (avg_days) {
    dawn_intervals <- list()
    dusk_intervals <- list()

    if (zoo_times) {
      dawn_intervals[[1]] <- interval(as.POSIXct(paste("1970-01-01 06:00:00", sep = ""), tz = "GMT"), as.POSIXct(paste("1970-01-01 07:30:00", sep = ""), tz = "GMT"))
      dusk_intervals[[1]] <- interval(as.POSIXct(paste("1970-01-01 18:00:00", sep = ""), tz = "GMT"), as.POSIXct(paste("1970-01-01 19:30:00", sep = ""), tz = "GMT"))
    } else {
      dawn_intervals[[1]] <- interval(as.POSIXct(paste("1970-01-01 07:00:00", sep = ""), tz = "GMT"), as.POSIXct(paste("1970-01-01 08:30:00", sep = ""), tz = "GMT"))
      dusk_intervals[[1]] <- interval(as.POSIXct(paste("1970-01-01 21:00:00", sep = ""), tz = "GMT"), as.POSIXct(paste("1970-01-01 22:30:00", sep = ""), tz = "GMT"))
    }
  }

  # Calculate TRUE/FALSE for each interval
  dawn_peaks <- lapply(dawn_intervals, function(x) {
    df <- als_data[als_data$peaks == "yes",]
    output <- df$datetime %within% x
    output <- any(output)
    return(output)
  })

  dusk_peaks <- lapply(dusk_intervals, function(x) {
    df <- als_data[als_data$peaks == "yes",]
    output <- df$datetime %within% x
    output <- any(output)
    return(output)
  })

  dawn_peak_percentage <- length(dawn_peaks[dawn_peaks == TRUE]) / length(dawn_peaks)
  dusk_peak_percentage <- length(dusk_peaks[dusk_peaks == TRUE]) / length(dusk_peaks)

  output <- list(dawn_peak_percentage, dusk_peak_percentage)
  names(output) <- c("dawn", "dusk")
  return(output)
}