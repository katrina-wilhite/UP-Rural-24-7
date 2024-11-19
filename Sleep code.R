# The function of this script file is develop V1 of activPal data processing for
# the nuMoM2b project. This version was created by D. Erik Boonstra if one has
# any questions, please, email daniel-boonstra@uiowa.edu

# Step 0a. Set working directory. ----
## Complete the following steps to set the working directory:
### i. Click on Session in the toolbar
### ii. Select Set Working Directory -> Choose Directory...
### iii. Navigate to the main processing folder
#### iiia. For this project it would be
#### "./nuMoM2b/AMS"

# Step 0b. Check R version ----
if (as.integer(R.version$major) != 4 & as.numeric(R.version$minor) < 1) {
  stop("R version version must be 4.1 or higher.")
}

# Step 1a. Loading required packages and functions ----
library(dplyr)
library(ggplot2)
library(lubridate)
library(progress)
library(tidyr)
library(readr)
source(file = paste0("./activpalProcessingFunctions.R"))
source(file = paste0("./ActivPalProcessing_V3_adjusted_functions.R"))
source(file = paste0("./processing_funs.R"))

# Step 1b. Setting subject specific values ----
# This values will be the arguments entered into the process_dat function
dat_source <- "Y:/UP & Rural 24-7/Participant Data/"
subject <- "8104-KS"
sleep_source <- "UPRural247SleepDatab_DATA_2024-11-12_1336.csv"
day1 <- TRUE
day2 <- TRUE
day3 <- TRUE
day4 <- TRUE
day5 <- TRUE
day6 <- TRUE
day7 <- TRUE
day8 <- TRUE


# Step 2. Check argument calls ----

## dat_source ----
hold1 <- "Y:/UP & Rural 24-7/Participant Data/"

if (dat_source %notin% hold1) {
  stop(paste0("dat_source must be from UP & Rural 24-7: ",
              hold1, "\n"))
}

## day(1-8) ----
hold2 <- c(day1, day2, day3, day4, day5, day6, day7, day8)
if (!is.logical(hold2)) {
  stop("day(1-8) must be logicals: TRUE or FALSE")
}

# Step 3. Import raw data ----

## Sleep diary data ----
sleep <- read.csv(paste0("Y:/UP & Rural 24-7/Participant Data/", sleep_source),
                  stringsAsFactors = FALSE)

## ActivPal data for a specific subject ----
## given data source
hold1 <- "Y:/UP & Rural 24-7/Participant Data/"

dat_source == hold1[1]
  paths <- paste0("Y:/UP & Rural 24-7/Participant Data/",
                  subject,
                  "/")
  files <- list.files(path = paths, pattern = "EventsEx.csv")
  dat <- read.csv(
    file = paste0(paths, files),
    skip = 2,
    header = FALSE,
    sep  = ";",
    stringsAsFactors = FALSE
  )
  
# Step 4. Processing of imported data ----

## Determining which days have valid data
valid_days <- c(day1, day2, day3, day4, day5, day6, day7, day8)
names(valid_days) <- paste0("day", 1:8)
### Listing the good days of data
good_days <- as.integer(substr(names(valid_days[valid_days]), 4, 4))
### Creating a vector of all the days
all_days <- 1:8

## Setting subject ID of interest for processing
subject_id <- subject

## Sleep diary data ----

### Creating subject ID variable in the sleep diary dataset
sleep$subject_id <- sleep$record_id

#### Moving the newly created subject IDs to be the first column in the sleep
#### diary dataset.
sleep <- dplyr::relocate(sleep, subject_id, .before = record_id)

### Pulling subject of interest sleep diary data ----
### Pulling the correct sleep information based on the subject and trimester
### of interest
sleep <- sleep[which(sleep$subject_id == subject_id), ]
row.names(sleep) <- NULL

### Changing how missing values are labeled
sleep[which(sleep == "")] <- NA

### Diary dates
### Obtaining each day's date based on the diary input.
#### This code also makes sure the data is in the correct format and exclude
#### any missing date fields.
diary_dates <- unlist(sleep[1, grep("^diary_date", colnames(sleep))])
diary_dates <- as.character(as.Date(diary_dates, format = "%m/%d/%Y"))
na_dates <- !is.na(diary_dates) # this denotes whether a date is not missing
diary_dates <- diary_dates[na_dates]

### Sleep windows ----
# Adjustments aren't working properly, I need to ask Bethany how the subjects
# interact with the diary

### Obtaining the sleep diary entry times. The entry with the last non-missing
### value is the most important because it marks the final day of the recording
### period. This value will be used to subset the data to only include the dates
### and times that occur before it.

#### Finding each day's start and end of sleep

##### Sleep stops
##### Pulling wake up times and formatting them to HH:MM:SS format
##### Considering we don't have a wake up time for the first day of the wear
##### period, I will assign a missing value to that entry
morn_diary <- unlist(sleep[1, grep("^diary_stopsleep", colnames(sleep))])
morn_diary <- c(NA, morn_diary)
morn_diary <- morn_diary[na_dates]
hold1 <- nchar(morn_diary) == 4 & substr(morn_diary, 2, 2) %in% c(":", ";")
hold2 <- nchar(morn_diary) == 5 & substr(morn_diary, 3, 3) %in% c(":", ";")
morn_diary <- ifelse(hold1 | hold2, paste0(morn_diary, ":00"), morn_diary)
morn_diary <- ifelse(nchar(morn_diary) <= 7 & !is.na(nchar(morn_diary)),
                     paste0("0", morn_diary),
                     morn_diary)

##### Sleep starts
##### Pulling go to sleep times and formatting them to HH:MM:SS format
##### For each day, we should have a begin sleep time
night_diary <- unlist(sleep[1, grep("^diary_trysleep", colnames(sleep))])
night_diary <- night_diary[na_dates]
hold1 <- nchar(night_diary) == 4 & substr(night_diary, 2, 2) %in% c(":", ";")
hold2 <- nchar(night_diary) == 5 & substr(night_diary, 3, 3) %in% c(":", ";")
night_diary <- ifelse(hold1 | hold2, paste0(night_diary, ":00"), night_diary)
night_diary <- ifelse(nchar(night_diary) <= 7 & !is.na(nchar(night_diary)),
                      paste0("0", night_diary),
                      night_diary)

#### Creating date-time values
sleep_start <- as.POSIXct(rep(NA, length(diary_dates)), tz = "UTC")
sleep_end <- as.POSIXct(rep(NA, length(diary_dates)), tz = "UTC")
for (j in seq_len(length(diary_dates))) {
  ##### End of sleep window (i.e., when they wake up and start their day)
  if (!is.na(morn_diary[j])) {
    sleep_end[j] <- as.POSIXct(paste0(diary_dates[j],
                                      " ",
                                      substr(morn_diary[j], 1, 2),
                                      ":",
                                      substr(morn_diary[j], 4, 5),
                                      ":",
                                      substr(morn_diary[j], 7, 8)),
                               tz = "UTC")
  } else {
    sleep_end[j] <- NULL
  }
  
  ##### Start of sleep window (i.e., when they go to sleep and end their day)
  if (!is.na(night_diary[j])) {
    sleep_start[j] <- as.POSIXct(paste0(diary_dates[j],
                                        " ",
                                        substr(night_diary[j], 1, 2),
                                        ":",
                                        substr(night_diary[j], 4, 5),
                                        ":",
                                        substr(night_diary[j], 7, 8)),
                                 tz = "UTC")
  } else {
    sleep_start[j] <- NULL
  }
  
  ##### Adjusting the date for the end of the sleep window. Due to the start
  ##### and of the sleep window being based on the same diary entry date,
  ##### the end of the sleep window may begin before the start of the sleep
  ##### window.
  if (!is.na(sleep_start[j]) & !is.na(sleep_end[j])) {
    if (sleep_start[j] <= sleep_end[j]) {
      sleep_start[j] <- sleep_start[j] + 86400
    } else {
      sleep_start[j] <- sleep_start[j]
    }
  }
  
  if (j == 1) {
    hold1 <- as.POSIXct(paste(diary_dates[1], sleep$diary_time1), tz = "UTC")
    if (sleep_start[j] <= hold1) {
      sleep_start[j] <- sleep_start[j] + 86400
    }
  }
  
  ##### Handling missing entries of sleep start/end times. If a subject
  ##### doesn't list a sleep start and sleep end time the data processor will
  ##### be asked whether data processing should stop.
  hold1 <- is.na(sleep_start[j]) & !is.na(sleep_end[j])
  hold2 <- !is.na(sleep_start[j]) & is.na(sleep_end[j])
  if ((hold1 | hold2) & j != 1) {
    jj <- all_days[j]
    resp <- menu(choices = c("Yes", "No"),
                 graphics = TRUE,
                 title = paste0("Day ", jj, " has a missing sleep time.",
                                "\nWould you like to stop processing?"))
    resp <- ifelse(resp == 1L, "Yes", "No")
    if (resp == "Yes") {
      stop(paste0("Day ",jj, " has a missing sleep time."))
    }
  } else if (j == 1 & is.na(sleep_start[j])) {
    stop(paste0("Day 1 has a missing sleep start time."))
  }
}

### Nap windows ----
#### Obtaining each day's nap times if a nap was taken. If a nap isn't taken,
#### then for that day the nap window will be missing.

##### Creating a vector of 0's and 1's which denote whether a nap was taken on
##### a certain day.
diary_nap <- unlist(sleep[1, grep("^diary_nap", colnames(sleep))])
diary_nap <- as.integer(diary_nap[which(nchar(names(diary_nap)) == 10)])
diary_nap <- diary_nap[na_dates]

##### Nap starts
##### Pulling go to nap times and formatting them to HH:MM:SS format
##### For each day, we should have a beginning nap time
nap_start_diary <- unlist(sleep[1, grep("^diary_napstrt", colnames(sleep))])
nap_start_diary <- nap_start_diary[na_dates]
temp <- c(":", ";")
hold1 <- nchar(nap_start_diary) == 4 & substr(nap_start_diary, 2, 2) %in% temp
hold2 <- nchar(nap_start_diary) == 5 & substr(nap_start_diary, 3, 3) %in% temp
nap_start_diary <- ifelse(hold1 | hold2,
                          paste0(nap_start_diary, ":00"),
                          nap_start_diary)
hold3 <- nchar(nap_start_diary) <= 7 & !is.na(nchar(nap_start_diary))
nap_start_diary <- ifelse(hold3,
                          paste0("0", nap_start_diary),
                          nap_start_diary)

##### Nap ends
##### Pulling go to nap times and formatting them to HH:MM:SS format
##### For each day, we should have a beginning nap time
nap_end_diary <- unlist(sleep[1, grep("^diary_napend", colnames(sleep))])
nap_end_diary <- nap_end_diary[na_dates]
temp <- c(":", ";")
hold1 <- nchar(nap_end_diary) == 4 & substr(nap_end_diary, 2, 2) %in% temp
hold2 <- nchar(nap_end_diary) == 5 & substr(nap_end_diary, 3, 3) %in% temp
nap_end_diary <- ifelse(hold1 | hold2,
                        paste0(nap_end_diary, ":00"),
                        nap_end_diary)
hold3 <- nchar(nap_end_diary) <= 7 & !is.na(nchar(nap_end_diary))
nap_end_diary <- ifelse(hold3,
                        paste0("0", nap_end_diary),
                        nap_end_diary)

#### Creating date-time values
nap_start <- as.POSIXct(rep(NA, length(diary_dates)), tz = "UTC")
nap_end <- as.POSIXct(rep(NA, length(diary_dates)), tz = "UTC")
for (j in seq_len(length(diary_dates))) {
  ##### End of nap window
  if (!is.na(nap_end_diary[j])) {
    nap_end[j] <- as.POSIXct(paste0(diary_dates[j],
                                    " ",
                                    substr(nap_end_diary[j], 1, 2),
                                    ":",
                                    substr(nap_end_diary[j], 4, 5),
                                    ":",
                                    substr(nap_end_diary[j], 7, 8)),
                             tz = "UTC")
  } else {
    nap_end[j] <- NULL
  }
  
  ##### Start of nap window
  if (!is.na(nap_start_diary[j])) {
    nap_start[j] <- as.POSIXct(paste0(diary_dates[j],
                                      " ",
                                      substr(nap_start_diary[j], 1, 2),
                                      ":",
                                      substr(nap_start_diary[j], 4, 5),
                                      ":",
                                      substr(nap_start_diary[j], 7, 8)),
                               tz = "UTC")
  } else {
    nap_start[j] <- NULL
  }
  
  ##### Adjusting the date for the end of the nap window. Due to the start
  ##### and of the nap window being based on the same diary entry date,
  ##### the end of the nap window may begin before the start of the sleep
  ##### window.
  if (!is.na(nap_start[j]) & !is.na(nap_end[j])) {
    if (nap_start[j] > nap_end[j]) {
      nap_end[j] <- nap_end[j] + 86400
    } else {
      nap_end[j] <- nap_end[j]
    }
  }
  
  ##### Handling missing entries of sleep start/end times. If a subject
  ##### doesn't list a sleep start and sleep end time the data processor will
  ##### be asked whether data processing should stop.
  hold1 <- is.na(nap_start[j]) & !is.na(nap_end[j])
  hold2 <- !is.na(nap_start[j]) & is.na(nap_end[j])
  if ((hold1 | hold2)) {
    jj <- all_days[j]
    resp <- menu(choices = c("Yes", "No"),
                 graphics = TRUE,
                 title = paste0("Day ", jj, " has a missing nap time.",
                                "\nWould you like to stop processing?"))
    resp <- ifelse(resp == 1L, "Yes", "No")
    if (resp == "Yes") {
      stop(paste0("Day ",jj, " has a missing sleep time."))
    }
  }
}

### Work times ----

#### First work interval ----
##### Obtaining the start of work times
work_start_a_diary <- unlist(sleep[1, grep("^diary_workstart_a", colnames(sleep))])
work_start_a_diary <- work_start_a_diary[na_dates]
temp <- c(":", ";")
hold1 <- nchar(work_start_a_diary) == 4 & substr(work_start_a_diary, 2, 2) %in% temp
hold2 <- nchar(work_start_a_diary) == 5 & substr(work_start_a_diary, 3, 3) %in% temp
work_start_a_diary <- ifelse(hold1 | hold2, paste0(work_start_a_diary, ":00"),
                             work_start_a_diary)
work_start_a_diary <- ifelse(nchar(work_start_a_diary) <= 7,
                             paste0("0", work_start_a_diary),
                             work_start_a_diary)

##### Obtaining the end of work times
work_end_a_diary <- unlist(sleep[1, grep("^diary_workstop_a", colnames(sleep))])
work_end_a_diary <- work_end_a_diary[na_dates]
temp <- c(":", ";")
hold1 <- nchar(work_end_a_diary) == 4 & substr(work_end_a_diary, 2, 2) %in% temp
hold2 <- nchar(work_end_a_diary) == 5 & substr(work_end_a_diary, 3, 3) %in% temp
work_end_a_diary <- ifelse(hold1 | hold2, paste0(work_end_a_diary, ":00"),
                           work_end_a_diary)
work_end_a_diary <- ifelse(nchar(work_end_a_diary) <= 7,
                           paste0("0", work_end_a_diary),
                           work_end_a_diary)

##### Adding the dates to the start and end of work times to have a full date-
##### time variable
work_start_a <- as.POSIXct(rep(NA, length(diary_dates)), tz = "UTC")
work_end_a <- as.POSIXct(rep(NA, length(diary_dates)), tz = "UTC")
for (j in 1:(length(diary_dates) - 1)) {
  if (!is.na(work_start_a_diary[j])) {
    ###### Start of work time
    work_start_a[j] <- as.POSIXct(paste(diary_dates[j], 
                                        paste0(substr(work_start_a_diary[j], 1, 2),
                                               ":", 
                                               substr(work_start_a_diary[j], 4, 5))),
                                  tz = "UTC")
  }
  
  ###### End of work time
  if (!is.na(work_end_a_diary[j])) {
    work_end_a[j] <- as.POSIXct(paste(diary_dates[j], 
                                      paste0(substr(work_end_a_diary[j], 1, 2), 
                                             ":", 
                                             substr(work_end_a_diary[j], 4, 5))), 
                                tz = "UTC")
  }
  
  ###### Adjusting the date for the end of the work window. Due to the start
  ###### and end of the work window being based on the same diary entry date,
  ###### the end of the work window may begin before the start of the work
  ###### window.
  if (!is.na(work_start_a[j]) & !is.na(work_end_a[j])) {
    if (work_start_a[j] > work_end_a[j]) {
      work_end_a[j] <- work_end_a[j] + 86400
    } else {
      work_end_a[j] <- work_end_a[j]
    }
  }
  
  ###### Handling missing entries of work start/end times. If a subject doesn't
  ###### doesn't list a work start and work stop time the data processor will
  ###### be asked whether data processing should stop.
  hold1 <- is.na(work_start_a[j]) & !is.na(work_end_a[j])
  hold2 <- !is.na(work_start_a[j]) & is.na(work_end_a[j])
  if ((hold1 | hold2)) {
    jj <- all_days[j]
    resp <- menu(choices = c("Yes", "No"),
                 graphics = TRUE,
                 title = paste0("Day ", jj, " has a missing work time.",
                                "\nWould you like to stop the data processing?"))
    resp <- ifelse(resp == 1L, "Yes", "No")
    if (resp == "Yes") {
      stop(paste0("Day ", jj, " has a missing work time."))
    }
  }
}

#### Second work interval ----
##### Obtaining the start of work times
work_start_b_diary <- unlist(sleep[1, grep("^diary_workstart_b", colnames(sleep))])
work_start_b_diary <- work_start_b_diary[na_dates]
temp <- c(":", ";")
hold1 <- nchar(work_start_b_diary) == 4 & substr(work_start_b_diary, 2, 2) %in% temp
hold2 <- nchar(work_start_b_diary) == 5 & substr(work_start_b_diary, 3, 3) %in% temp
work_start_b_diary <- ifelse(hold1 | hold2, paste0(work_start_b_diary, ":00"),
                             work_start_b_diary)
work_start_b_diary <- ifelse(nchar(work_start_b_diary) <= 7,
                             paste0("0", work_start_b_diary),
                             work_start_b_diary)

##### Obtaining the end of work times
work_end_b_diary <- unlist(sleep[1, grep("^diary_workstop_b", colnames(sleep))])
work_end_b_diary <- work_end_b_diary[na_dates]
temp <- c(":", ";")
hold1 <- nchar(work_end_b_diary) == 4 & substr(work_end_b_diary, 2, 2) %in% temp
hold2 <- nchar(work_end_b_diary) == 5 & substr(work_end_b_diary, 3, 3) %in% temp
work_end_b_diary <- ifelse(hold1 | hold2, paste0(work_end_b_diary, ":00"),
                           work_end_b_diary)
work_end_b_diary <- ifelse(nchar(work_end_b_diary) <= 7,
                           paste0("0", work_end_b_diary),
                           work_end_b_diary)

##### Adding the dates to the start and end of work times to have a full date-
##### time variable
work_start_b <- as.POSIXct(rep(NA, length(diary_dates)), tz = "UTC")
work_end_b <- as.POSIXct(rep(NA, length(diary_dates)), tz = "UTC")
for (j in 1:(length(diary_dates) - 1)) {
  if (!is.na(work_start_b_diary[j])) {
    ###### Start of work time
    work_start_b[j] <- as.POSIXct(paste(diary_dates[j], 
                                        paste0(substr(work_start_b_diary[j], 1, 2),
                                               ":", 
                                               substr(work_start_b_diary[j], 4, 5))),
                                  tz = "UTC")
  }
  
  ###### End of work time
  if (!is.na(work_end_b_diary[j])) {
    work_end_b[j] <- as.POSIXct(paste(diary_dates[j], 
                                      paste0(substr(work_end_b_diary[j], 1, 2), 
                                             ":", 
                                             substr(work_end_b_diary[j], 4, 5))), 
                                tz = "UTC")
  }
  
  ###### Adjusting the date for the end of the work window. Due to the start
  ###### and end of the work window being based on the same diary entry date,
  ###### the end of the work window may begin before the start of the work
  ###### window.
  if (!is.na(work_start_b[j]) & !is.na(work_end_b[j])) {
    if (work_start_b[j] > work_end_b[j]) {
      work_end_b[j] <- work_end_b[j] + 86400
    } else {
      work_end_b[j] <- work_end_b[j]
    }
  }
  
  ###### Handling missing entries of work start/end times. If a subject doesn't
  ###### doesn't list a work start and work stop time the data processor will
  ###### be asked whether data processing should stop.
  hold1 <- is.na(work_start_b[j]) & !is.na(work_end_b[j])
  hold2 <- !is.na(work_start_b[j]) & is.na(work_end_b[j])
  if ((hold1 | hold2)) {
    jj <- all_days[j]
    resp <- menu(choices = c("Yes", "No"),
                 graphics = TRUE,
                 title = paste0("Day ", jj, " has a missing work time.",
                                "\nWould you like to stop the data processing?"))
    resp <- ifelse(resp == 1L, "Yes", "No")
    if (resp == "Yes") {
      stop(paste0("Day ", jj, " has a missing work time."))
    }
  }
}

### Monitor off times ----

#### Obtaining when the monitor was taken off times
monitor_off_diary <- unlist(sleep[1, grep("^diary_monitorsofftime", colnames(sleep))])
monitor_off_diary <- monitor_off_diary[na_dates]
temp <- c(":", ";")
hold1 <- nchar(monitor_off_diary) == 4 & substr(monitor_off_diary, 2, 2) %in% temp
hold2 <- nchar(monitor_off_diary) == 5 & substr(monitor_off_diary, 3, 3) %in% temp
monitor_off_diary <- ifelse(hold1 | hold2, 
                            paste0(monitor_off_diary, ":00"),
                            monitor_off_diary)
monitor_off_diary <- ifelse(nchar(monitor_off_diary) <= 7,
                            paste0("0", monitor_off_diary),
                            monitor_off_diary)

#### Obtaining when the monitor was put back on times
monitor_on_diary <- unlist(sleep[1, grep("^diary_monitorsontime", colnames(sleep))])
monitor_on_diary <- monitor_on_diary[na_dates]
temp <- c(":", ";")
hold1 <- nchar(monitor_on_diary) == 4 & substr(monitor_on_diary, 2, 2) %in% temp
hold2 <- nchar(monitor_on_diary) == 5 & substr(monitor_on_diary, 3, 3) %in% temp
monitor_on_diary <- ifelse(hold1 | hold2, 
                           paste0(monitor_on_diary, ":00"),
                           monitor_on_diary)
monitor_on_diary <- ifelse(nchar(monitor_on_diary) <= 7,
                           paste0("0", monitor_on_diary),
                           monitor_on_diary)

#### Adding the dates to the off and on of monitor times to have a full date-
#### time variable
monitor_off <- as.POSIXct(rep(NA, length(diary_dates)), tz = "UTC")
monitor_on <- as.POSIXct(rep(NA, length(diary_dates)), tz = "UTC")
for (j in 1:(length(diary_dates) - 1)) {
  if (!is.na(monitor_off_diary[j])) {
    ##### Start of monitor off time
    monitor_off[j] <- as.POSIXct(paste(diary_dates[j], 
                                       paste0(substr(monitor_off_diary[j], 1, 2), 
                                              ":", 
                                              substr(monitor_off_diary[j], 4, 5))), 
                                 tz = "UTC")
  }
  
  ##### End of monitor off time
  if (!is.na(monitor_on_diary[j])) {
    monitor_on[j] <- as.POSIXct(paste(diary_dates[j], 
                                      paste0(substr(monitor_on_diary[j], 1, 2), 
                                             ":", 
                                             substr(monitor_on_diary[j], 4, 5))), 
                                tz = "UTC")
  }
  
  ##### Adjusting the date for the end of the monitor off window. Due to the start
  ##### and end of the monitor off window being based on the same diary entry date,
  ##### the end of the monitor off window may begin before the start of the monitor
  ##### off window.
  if (!is.na(monitor_off[j]) & !is.na(monitor_on[j])) {
    if (monitor_off[j] > monitor_on[j]) {
      monitor_on[j] <- monitor_on[j] + 86400
    } else {
      monitor_on[j] <- monitor_on[j]
    }
  }
  
  ##### Handling missing entries of monitor off/on times. If a subject doesn't
  ##### doesn't list a monitor off and monitor on time the data processor will
  ##### be asked whether data processing should stop.
  hold1 <- is.na(monitor_off[j]) & !is.na(monitor_on[j])
  hold2 <- !is.na(monitor_off[j]) & is.na(monitor_on[j])
  if ((hold1 | hold2)) {
    jj <- all_days[j]
    resp <- menu(choices = c("Yes", "No"),
                 graphics = TRUE,
                 title = paste0("Day ", jj, " has a missing monitor time.",
                                "\nWould you like to stop the data processing?"))
    resp <- ifelse(resp == 1L, "Yes", "No")
    if (resp == "Yes") {
      stop(paste0("Day ", jj, " has a missing monitor time."))
    }
  }
}

### Prepare sleep data for merging ----
### Adding an additional set of rows that add 1 second to the
### sleep_end (i.e., wake up times) and nap_end (i.e., wake from
### nap times) . This is being done because when you expand this data
### out to second by second data it will show that a subject is still
### asleep via the wake loop variable after they've had a record of
### waking up.
hold1 <- c(sleep_end, sleep_start,
           nap_start, nap_end,
           work_start_a, work_end_a,
           work_start_b, work_end_b,
           monitor_off, monitor_on,
           sleep_end + 1, nap_end + 1)

#### Temporary sleep data ----
#### Creating a temporary data frame that has the same column as the ActivPal
#### dataset.
temp_sleep <- data.frame(time = hold1,
                         datacount = NA,
                         interval = NA,
                         activity = NA,
                         cumulativesteps = NA,
                         methrs = NA)

#### Loop variables ----
#### Before merging the sleep data with the ActivPal data, some additional
#### variables will be created based on the sleep, nap, and work windows.
#### At this point, the monitor off times are included in creating the loop
#### variables. Therefore, if the loop variables don't have value of 1, then 
#### the value is missing. After the 

##### Wear day ----
temp_sleep$wear_day <- c(all_days,
                         rep(NA, (nrow(temp_sleep) - length(all_days))))
##### Sleep loop ----
temp_sleep$sleep_loop <- c(rep(1, length(c(sleep_end, sleep_start))),
                           rep(0, nrow(temp_sleep) - 
                                 length(c(sleep_end, sleep_start))))
##### Nap loop ----
temp_sleep$nap_loop <- c(rep(0, length(c(sleep_end, sleep_start))),
                         rep(1, length(c(nap_start, nap_end))), 
                         rep(0, nrow(temp_sleep) - 
                               (length(c(sleep_end, sleep_start)) +
                                  length(c(nap_start, nap_end)))))
##### Wake loop ----
temp_sleep$wake_loop <- c(rep(0, length(c(sleep_start, sleep_end,
                                          nap_start, nap_end))),
                          rep(1, (nrow(temp_sleep) - 
                                    length(c(sleep_start, sleep_end,
                                             nap_start, nap_end)))))
##### Work loop ----
temp_sleep$work_loop <- c(rep(0, length(c(sleep_start, sleep_end, 
                                          nap_start, nap_end))),
                          rep(1, length(c(work_start_a, work_end_a,
                                          work_start_b, work_end_b))),
                          rep(0, length(c(monitor_off, monitor_on,
                                          sleep_end, nap_end))))

#### Removing NA times ----
temp_sleep <- temp_sleep[!is.na(temp_sleep$time), ]
temp_sleep <- temp_sleep[order(temp_sleep$time), ]

## ActivPal data ----

### Rearrange the variables to be in the order Kate Lyden used
dat <- dplyr::bind_cols(dat[, c(1, 3, 5, 4, 9, 10, 11, 12, 13)],
                        dat[, c(2, 6, 12, 13, 14, 15, 16, 17)])

### Pulling the first six variables of data. To process the ActivPal data these
### are the only
### variables needed as indicated by Kate Lyden.
dat <- dat[, 1:6]
names(dat) <- c("time", "datacount", "interval",
                "activity", "cumulativesteps", "methrs")

### Doubling the number of cumulative steps because the ActivPal is only placed
### on one leg, so accounting for the other leg steps is needed.
dat$cumulativesteps <- c(dat$cumulativesteps) * 2

### Adjusting the way time is expressed
dat$time <- as.POSIXct(as.Date(dat$time, origin = "1899-12-30"))
dat$time <- lubridate::with_tz(dat$time, "UTC")

# Step 5. Merge data ----
# In this step, the sleep diary data and ActivPal data are merged
# together

## Merging sleep data with ActivPal data ----
dat <- dplyr::bind_rows(dat, temp_sleep)
dat <- dat[order(dat$time, dat$datacount), ]
dat <- dat[!duplicated(dat$time), ]
dat <- dat[order(dat$time, dat$datacount), ]
### Subsetting the data to exclude any data that comes after time a subject
### wakes up on the final day of the wear period.
hold1 <- sleep_start[!is.na(sleep_start)]
hold2 <- hold1[length(hold1)]
dat <- subset(dat, subset = time <= hold2)
### Removing the first rows of data if it occurred before the monitors were
### initial put on
hold3 <- sleep$diary_time1
temp <- c(":", ";")
hold1 <- nchar(hold3) == 4 & substr(hold3, 2, 2) %in% temp
hold2 <- nchar(hold3) == 5 & substr(hold3, 3, 3) %in% temp
hold3 <- ifelse(hold1 | hold2, 
                paste0(hold3, ":00"),
                hold3)
hold3 <- ifelse(nchar(hold3) <= 7,
                paste0("0", hold3),
                hold3)

hold3 <- as.POSIXct(paste(diary_dates[1],
                          paste0(substr(hold3, 1, 2),
                                 ":",
                                 substr(hold3, 4, 5))),
                    tz = "UTC")
dat <- dat[dat$time >= hold3, ]
### Creating some additional variables that Kate Lyden created
t <- dim(dat)[1]
n <- dim(dat)[1]

# Step 6. Filling in missing values ----

## Loop variables ----

### Wear day ----
dat <- tidyr::fill(dat, wear_day, .direction = "down")

### Sleep loop ----
dat <- tidyr::fill(dat, sleep_loop, .direction = "down")

### Nap loop ----
dat <- tidyr::fill(dat, nap_loop, .direction = "down")

### Wake loop ----
dat <- tidyr::fill(dat, wake_loop, .direction = "down")

### Work loop ----
dat <- tidyr::fill(dat, work_loop, .direction = "down")

### Day 1 before sleep missing values ----
### If there is a valid day 1 then a subject may have data that occurs before
### they fall asleep. So, with the way the looping variables were filled in
### these observations might have missing values. If there is a missing value,
### this indicates that the loop event didn't occur. Therefore, the loop values
### should be 0 for forms of sleeping and working, but a 1 for the awake loop.
dat$wear_day[is.na(dat$wear_day)] <- 1
dat$sleep_loop[is.na(dat$sleep_loop)] <- 0
dat$nap_loop[is.na(dat$nap_loop)] <- 0
dat$wake_loop[is.na(dat$wake_loop)] <- 1
dat$work_loop[is.na(dat$work_loop)] <- 0

### Adjusting the looping variables to indicate that the monitor was taken off
### and put back on at a certain time. If the devices were indicated as being
### taken off then during that time frame the looping variables will have a
### value of 99. 
hold1 <- monitor_off[!is.na(monitor_off) & !is.na(monitor_on)]
hold2 <- monitor_on[!is.na(monitor_off) & !is.na(monitor_on)]
if (length(hold1) > 0) {
  for (k in 1:length(hold1)) {
    for (j in 1:nrow(dat)) {
      if (dat$time[j] >= hold1[k] & dat$time[j] <= hold2[k]) {
        dat$sleep_loop[j] <- 99
        dat$nap_loop[j] <- 99
        dat$wake_loop [j] <- 99
      }
    }
  }
}

## Activity Data ----
## In filling in these activity variables, only the days that are considered to
## have usable data will be filled in. The variable interval may be filled for
## all days because it only depends on the time.

### Interval ---- 
for (j in 1:length(dat$time)) {
  if (is.na(dat$interval[j])) {
    dat$interval[j] <- as.numeric(difftime(dat$time[j + 1], dat$time[j], 
                                           units = "secs"))
    dat$interval[j - 1] <- as.numeric(difftime(dat$time[j], dat$time[j - 1],
                                               units = "secs"))
  } else {
    dat$interval[j] <- dat$interval[j]
  }
}

#### Giving the time the subject wakes up an interval value of 1 because there 
#### is no way to calculate the true interval value because we are stopping 
#### when they wake up. So, there is only 1 second worth of data when they 
#### wake up
dat$interval[t] <- 1

### Met hours ----
for (j in 1:length(dat$time)) {
  if (is.na(dat$methrs[j]) & (dat$wear_day[j] %in% good_days)) {
    mets <- dat$methrs[j - 1] / (dat$interval[j - 1] / 3600)
    first_position_time <- as.numeric(difftime(dat$time[j], dat$time[j - 1], 
                                               units = "hours"))
    second_position_time <- as.numeric(difftime(dat$time[j + 1], dat$time[j],
                                                units = "hours"))
    totaltime <- dat$interval[j - 1]
    first_factor <- (first_position_time / totaltime) * mets
    second_factor <- (second_position_time / totaltime) * mets
    dat$methrs[j - 1] <- first_factor * first_position_time
    dat$methrs[j] <- second_factor * second_position_time
  } else {
    dat$methrs[j] <- dat$methrs[j]
  }
}

#### Last observation
dat$methrs[t] <- dat$methrs[t - 1]

### Cumulative steps ----
for (j in 1:length(dat$cumulativesteps)) {
  if (is.na(dat$cumulativesteps[j]) & (dat$wear_day[j] %in% good_days)) {
    dat$cumulativesteps[j] <- dat$cumulativesteps[j - 1]
  } else {
    dat$cumulativesteps[j] <- dat$cumulativesteps[j]
  }
}

### Activity ----
dat$activity[is.na(dat$activity)] <- 0

### Data count ----
for (j in 1:length(dat$datacount)) {
  if (is.na(dat$datacount[j]) & (dat$wear_day[j] %in% good_days)) {
    dat$datacount[j] <- dat$datacount[j - 1] + (dat$interval[j - 1] * 10)
  } else {
    dat$datacount[j] <- dat$datacount[j]
  }
}

## Removing non-usable wear days ----
## Excluding in any observations that don't have a usable activity data.
## This is being done to account for the ActivPal or some other
## measuring device not recording data. This set is directly linked to the 
## valid days.
## This was done after all the missing values were filled in because the
## activity data needed to be filled with information from these observations
dat <- subset(dat, subset = wear_day %in% good_days)

# Step 7. Creating 1 sec epoch dataset ----

## Rounding the event times to the nearest second
## This is being done to create matching times with the basis 1 sec epoch file
dat$time <- round(dat$time, units = "secs")
dat$time <- as.POSIXct(dat$time, tz = "UTC")

## Denoting the events ----
## The activity and sleep data denote events that occurred.
dat$event <- 1:nrow(dat)
dat <- dplyr::relocate(dat, event, .before = time)

## Find the initial starting time
start_time <- strptime(dat$time[1], format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

## Creating time variables for various EPOCH files based on different number of
## seconds
secs <- as.numeric((max(dat$time) - min(dat$time)) * 86400)
times <- as.POSIXct(start_time + (0:secs), tz = "UTC")

## Creating a basis 1 second epoch dataset ----
## This dataset will use the event data converted to a sec by sec dataset
sec_by_sec <- data.frame(event = NA,
                         time = round(times, units = "secs"),
                         datacount = NA,
                         interval = NA,
                         activity = NA,
                         cumulativesteps = NA, 
                         methrs = NA,
                         wear_day = NA,
                         sleep_loop = NA,
                         nap_loop = NA,
                         wake_loop = NA,
                         work_loop = NA)
sec_by_sec$time <- as.POSIXct(sec_by_sec$time, tz = "UTC")

## Creating sec by sec dataset ----
## Note that all the time in the events file will be duplicated since the sec by
## sec dataset is based on the events file. Considering there is duplication the
## duplicates will need to be removed.
sec_by_sec <- dplyr::bind_rows(dat, sec_by_sec)
sec_by_sec <- sec_by_sec[order(sec_by_sec$time, sec_by_sec$datacount), ]
hold1 <- (!duplicated(sec_by_sec$time) | !is.na(sec_by_sec$event))
sec_by_sec <- sec_by_sec[hold1, ]
sec_by_sec <- sec_by_sec[order(sec_by_sec$time, sec_by_sec$datacount), ]

### Cleaning up sec by sec data ----

#### Removing previous row numbers ----
rownames(sec_by_sec) <- NULL

#### Filling in missing values ----

##### event ----
sec_by_sec <- tidyr::fill(sec_by_sec,
                          event,
                          datacount,
                          interval,
                          activity,
                          cumulativesteps,
                          methrs,
                          wear_day,
                          sleep_loop,
                          nap_loop,
                          wake_loop,
                          work_loop,
                          .direction = "down")

#### Renaming some variables ----
sec_by_sec <- dplyr::rename(sec_by_sec,
                            ap.posture = activity,
                            steps = cumulativesteps,
                            met.hours = methrs)

### Calculating 1 epoch dataset values ----
n <- as.numeric(nrow(sec_by_sec))

#### 1 sec met values ----
##First line deleted to remove "double dividing" of total duration 
#sec_by_sec$met.hours <- sec_by_sec$met.hours / sec_by_sec$interval
sec_by_sec$mets1 <- (sec_by_sec$met.hours * 3600) / sec_by_sec$interval
sec_by_sec <- dplyr::relocate(sec_by_sec, mets1, .after = met.hours)

#### 30 second met values ----
time30 <- start_time + (30 * rep(0:floor((n / 30)), each = 30, length = n))
sec_by_sec$mets30 <- rep(tapply(sec_by_sec$mets1, time30, mean), 
                         each = 30, length = n)
sec_by_sec <- dplyr::relocate(sec_by_sec, mets30, .after = mets1)

#### 60 second met values
time60 <- start_time + (60 * rep(0:floor((n / 60)), each = 60, length = n))
sec_by_sec$mets60 <- rep(tapply(sec_by_sec$mets1, time60, mean), 
                         each = 60, length = n)
sec_by_sec <- dplyr::relocate(sec_by_sec, mets60, .after = mets30)

#### Rounding met values
sec_by_sec$mets1 <- signif(sec_by_sec$mets1, 3)
sec_by_sec$mets30 <- signif(sec_by_sec$mets30, 3)
sec_by_sec$mets60 <- signif(sec_by_sec$mets60, 3)

#### Adding date variable ----
sec_by_sec$date <- as.Date(substring(format(sec_by_sec$time), 1, 10), "%Y-%m-%d")
sec_by_sec <- dplyr::relocate(sec_by_sec, date, .after = time)

#### Adding an activity variable ----
sec_by_sec$activity <- sec_by_sec$ap.posture
sec_by_sec <- dplyr::relocate(sec_by_sec, activity, .after = ap.posture)

#### Adjusting work interval to account for napping during the work ----
sec_by_sec$work_loop <- ifelse(sec_by_sec$work_loop == 1 & sec_by_sec$nap_loop == 1,
                               0, sec_by_sec$work_loop)

# Step 8. Graphing activity ----
## Collecting all the sleeping and napping times ----
hold1 <- c(sleep_end[-1], NA)
sleep_times <- data.frame(sleep_start = c(sleep_start, nap_start),
                          sleep_stop = c(hold1, nap_end),
                          day = c(all_days, all_days),
                          label = c(rep("sleep", length(all_days)),
                                    rep("nap", length(all_days))))
## Removing any of sleeping/napping times that occurred on invalid days.
w <- which(sleep_times$day == 8 & sleep_times$label == "sleep")
sleep_times[w, ] <- data.frame(NA, NA, 8, "sleep")
sleep_times <- subset(sleep_times, subset = day %in% good_days)

## Adjusting the sec by sec data to be plotted ---
graph_data <- sec_by_sec

### Setting the maximum duration an activity can occur to 300s
### Also, adjusting for days that have less than 5 minutes of data
hold1 <- lapply(X = table(graph_data$wear_day), FUN = function(x) {seq(1, x)})
hold2 <- lapply(X = 1:length(hold1), 
                FUN = function(j) {
                  if (max(hold1[[j]]) < 300) {
                    return(ceiling(hold1[[j]] / 50))
                  } else {
                    return(ceiling(hold1[[j]] / 300))
                  }
                })
graph_data$group_column <- unlist(hold2)

### Getting times that events occurred
graph_data$o_clock <- strftime(graph_data$time, format="%H:%M:%S", tz = "UTC")

### Grouping the data and summarizing it
graph_data <- dplyr::group_by(graph_data, wear_day, group_column) |>
  dplyr::summarise(Sleep = sum(wake_loop == 0), Wake = sum(wake_loop == 1),
                   Non_Wear = sum(wake_loop == 99), Working = sum(work_loop == 1),
                   Not_Working = sum(work_loop == 0), laying = sum(activity == 0),
                   standing = sum(activity == 1), walking = sum(activity == 2),
                   day = max(wear_day), o_clock = min(time), .groups = "keep") |>
  tidyr::pivot_longer(cols = c(laying, standing, walking), 
                      names_to = "activity", values_to = "interval")

graph_data <- dplyr::group_by(graph_data, day) |>
  dplyr::mutate(time_of_day = group_column - min(group_column) + 1)

## Plotting each valid day of wear ----
g <- ggplot2::ggplot() +
  ggplot2::geom_line(data = tidyr::drop_na(graph_data), 
                     ggplot2::aes(x = o_clock, y = interval, 
                                  color = as.factor(activity)), 
                     size = 1) +
  ggplot2::labs(x = "Time of Day", y = "Duration (s)", color = "Event Type") +
  ggplot2::theme_grey() +
  ggplot2::scale_color_manual(values = c("sky blue", "coral", "forest green")) +
  ggplot2::facet_wrap(day ~ ., scales = "free_x", ncol = 1) +
  ggplot2::scale_y_continuous(breaks = c(0, 150, 300)) + 
  ggplot2::scale_x_datetime(date_breaks = "6 hours") + 
  ggplot2::geom_rect(data = sleep_times, 
                     ggplot2::aes(xmin = sleep_stop, xmax = sleep_start, 
                                  ymin = -Inf, ymax = Inf,
                                  fill = label), 
                     alpha = .2)

# Step 9. Calculating activity measures ----

## Splitting the sec by sec and activity data into wear day data ----

### Sec by sec data ----
day_total <- split(x = sec_by_sec[sec_by_sec$wake_loop != 99, ], 
                   f = sec_by_sec$wear_day[sec_by_sec$wake_loop != 99])

#### Sec by sec data for only during awake period
day <- lapply(X = 1:length(day_total),
              FUN = function(j) {
                day_total[[j]][day_total[[j]]$wake_loop == 1, ]
              })
names(day) <- good_days

### ActivPal data ----
day_dat <- split(x = dat,
                 f = dat$wear_day)
day_dat <- lapply(X = 1:length(day_dat), 
                  FUN = function(j) {
                    day_dat[[j]][day_dat[[j]]$wake_loop == 1, ]
                  })
names(day_dat) <- good_days

## Setting up variables dataset
variables <- data.frame(wear_day = good_days)

## Obtaining activity variables ----
## This section of code will be filling in the variables dataset that will
## be exported later

### Calendar variables ----

#### Day ----
#### Number of days wearing the device
variables$days <- rep(difftime(dat[length(dat$time), ]$time, 
                               dat[1, ]$time, units = "days"), 
                      length(good_days))

#### Weekday ----
#### Number of weekdays wearing the device
variables$weekday <- rep(sum(ifelse(unique(weekdays(dat$time)) %in% c("Monday", 
                                                                      "Tuesday", 
                                                                      "Wednesday", 
                                                                      "Thursday",
                                                                      "Friday"), 
                                    1, 0)), 
                         length(good_days))

#### Weekend ----
#### Number of weekend days wearing the device
variables$weekend <- rep(sum(ifelse(unique(weekdays(dat$time)) %in% c("Sunday", 
                                                                      "Saturday"), 
                                    1, 0)), 
                         length(good_days))

#### Primary day of week ----
#### This was determined by using the median time of each day. Note that the 
#### final day won't have a day of week value because we are only capturing the 
#### sleep data on that day
variables$day_of_week <- NA
for (j in 1:length(good_days)) {
  variables$day_of_week[j] <- weekdays(median(day_total[[j]]$time))
}

### Activity variables ----

#### Position/activity ----
#### Calculating the number of certain activity per time period of interest
variables$steps <- NA
variables$step_min <- 0
variables$stand_min <- 0
variables$upright_min <- NA
variables$sed_min <- 0
for (j in 1:length(good_days)) {
  if (nrow(day[[j]]) != 0) {
    ##### Steps per day
    variables$steps[j] <- day[[j]][length(day[[j]]$wear_day), ]$steps - day[[j]][1, ]$steps
    
    ##### Steps per minute per day
    variables$step_min[j] <- sum(ifelse(day[[j]]$activity == 2, 1, 0)) / 60
    
    ##### Stand minutes per day
    variables$stand_min[j] <- sum(ifelse(day[[j]]$activity == 1, 1, 0)) / 60
    
    ##### Upright minutes per day
    variables$upright_min[j] <- variables$stand_min[j] + variables$step_min[j]
    
    ##### Sedentary minutes per day
    variables$sed_min[j] <- sum(ifelse(day[[j]]$activity == 0, 1, 0)) / 60
  }
}

#### Activity bouts ----
#### Calculating the number of bouts by certain activity per time 
#### period of interest
variables$sed30_bout <- 0
variables$sed60_bout <- 0
variables$sed30_min <- 0
variables$sed60_min <- 0
for (j in 1:length(good_days)) {
  if (nrow(day[[j]]) != 0) {
    ##### Number of sit bouts > 30 minutes per day
    variables$sed30_bout[j] <- prolonged.sed.bouts.num(posture = day[[j]]$ap.posture, 
                                                       epoch = 1, n = 30)
    
    ##### Number of sit bouts > 60 minutes per day
    variables$sed60_bout[j] <- prolonged.sed.bouts.num(posture = day[[j]]$ap.posture, 
                                                       epoch = 1, n = 60)
    
    ##### Time spent in sitting bouts > 30 minutes per day
    variables$sed30_min[j] <- prolonged.sed.bouts.min(posture = day[[j]]$ap.posture, 
                                                      epoch = 1, n = 30)
    
    ##### Time spent in sitting bouts > 60 minutes per day
    variables$sed60_min[j] <- prolonged.sed.bouts.min(posture = day[[j]]$ap.posture, 
                                                      epoch = 1, n = 60)
  }
}

#### Descriptive measures for movement behaviors ----
variables$mean_sed_bout <- 0
variables$sed_perc <- 0
variables$sitstand <- 0
time_awake <- variables$sed_min + variables$stand_min + variables$step_min
stand_perc <- rep(0, length(good_days))
step_perc <- rep(0, length(good_days))
for (j in 1:length(good_days)) {
  if (nrow(day_dat[[j]]) != 0) {
    ##### Mean sedentary bout length per minute
    variables$mean_sed_bout[j] <- mean(day_dat[[j]][day_dat[[j]]$activity == 0, ]$interval) / 60
  }
  
  ##### Total time spend sedentary, standing, and stepping expressed as percent 
  ##### of wake and wear times. Calculated by dividing total sedentary time 
  ##### (sed_min) by the total time subject was awake and wear the ActivPal
  variables$sed_perc[j] <- variables$sed_min[j] / time_awake[j]
  stand_perc[j] <- variables$stand_min[j] / time_awake[j]
  step_perc[j] <- variables$step_min[j] / time_awake[j]
  
  if (nrow(day[[j]]) != 0) {
    ##### Number of sit to stand transitions per day
    variables$sitstand[j] <- breaks.AP(day[[j]]$ap.posture)
  }
}

##### Adjusting for 0 / 0 producing NaN
variables$sed_perc <- ifelse(is.nan(variables$sed_perc), 0, variables$sed_perc)
stand_perc <- ifelse(is.nan(stand_perc), 0, stand_perc)
step_perc <- ifelse(is.nan(step_perc), 0, step_perc)

#### Physical activity measures ----
variables$met_hrs <- NA
variables$lpa1_min <- 0
variables$lpa30_min <- 0
variables$lpa60_min <- 0
variables$mpa1_min <- 0
variables$mpa30_min <- 0
variables$mpa60_min <- 0
variables$vpa1_min <- 0
variables$vpa30_min <- 0
variables$vpa60_min <- 0
variables$mvpa1_min <- 0
variables$mvpa30_min <- 0
variables$mvpa60_min <- 0
variables$lpa1_perc <- NA
variables$lpa30_perc <- NA
variables$lpa60_perc <- NA
variables$mpa1_perc <- NA
variables$mpa30_perc <- NA
variables$mpa60_perc <- NA
variables$vpa1_perc <- NA
variables$vpa30_perc <- NA
variables$vpa60_perc <- NA
variables$mvpa1_perc <- NA
variables$mvpa30_perc <- NA
variables$mvpa60_perc <- NA
variables$guideline_minbout_10 <- 0
variables$guideline_numbout_10 <- 0
for (j in 1:length(good_days)) {
  if (nrow(day[[j]]) != 0) {
    ##### ActivPal estimate of met-hours per day which is the sum of met-hours
    ##### awake and sleep
    variables$met_hrs[j] <- sum(day_dat[[j]]$methrs, na.rm = TRUE)
    
    ##### Total time in light intensity (1.5 to < 3 METS) activity per day
    variables$lpa1_min[j] <- lit.min.AP(mets = day[[j]]$mets1, 
                                        posture = day[[j]]$ap.posture, 
                                        epoch = 1)
    variables$lpa30_min[j] <- lit.min.AP(mets = day[[j]]$mets30, 
                                         posture = day[[j]]$ap.posture, 
                                         epoch = 1)
    variables$lpa60_min[j] <- lit.min.AP(mets = day[[j]]$mets60, 
                                         posture = day[[j]]$ap.posture, 
                                         epoch = 1)
    
    ##### Total time in moderate intensity (>= 3 to 6 METS) activity per day
    variables$mpa1_min[j] <- MPA.min.AP(mets = day[[j]]$mets1, epoch = 1)
    variables$mpa30_min[j] <- MPA.min.AP(mets = day[[j]]$mets30, epoch = 1)
    variables$mpa60_min[j] <- MPA.min.AP(mets = day[[j]]$mets60, epoch = 1)
    
    ##### Total time in vigorous intensity (>= 6 METS) activity per day
    variables$vpa1_min[j] <- VPA.min.AP(mets = day[[j]]$mets1, epoch = 1)
    variables$vpa30_min[j] <- VPA.min.AP(mets = day[[j]]$mets30, epoch = 1)
    variables$vpa60_min[j] <- VPA.min.AP(mets = day[[j]]$mets60, epoch = 1)
    
    ##### Total time in moderate-to-vigorous intensity (>= 3 METS) activity per day
    variables$mvpa1_min[j] <- mvpa.min.AP(day[[j]]$mets1, epoch = 1)
    variables$mvpa30_min[j] <- mvpa.min.AP(day[[j]]$mets30, epoch = 1)
    variables$mvpa60_min[j] <- mvpa.min.AP(day[[j]]$mets60, epoch = 1)
    
    ##### Estimating the minutes spent in bouts of activity that qualify towards
    ##### meeting the physical activity guidelines (10 minutes)
    variables$guideline_minbout_10[j] <- guideline.bouts.min(mets = day[[j]]$mets1)
    
    ##### Estimating the nubmer of bouts to activity that qualify towards meeting
    ##### the physical activity guidelines (10 minutes)
    variables$guideline_numbout_10[j] <- guideline.bouts.num(mets = day[[j]]$mets1)
    
    ##### Total time spend in LPA expressed as a percent of waking time and wear time
    variables$lpa1_perc[j] <- variables$lpa1_min[j] / time_awake[j]
    variables$lpa30_perc[j] <- variables$lpa30_min[j] / time_awake[j]
    variables$lpa60_perc[j] <- variables$lpa60_min[j] / time_awake[j]
    
    ##### Total time spend in MPA expressed as a percent of waking time and wear time
    variables$mpa1_perc[j] <- variables$mpa1_min[j] / time_awake[j]
    variables$mpa30_perc[j] <- variables$mpa30_min[j] / time_awake[j]
    variables$mpa60_perc[j] <- variables$mpa60_min[j] / time_awake[j]
    
    ##### Total time spend in VPA expressed as a percent of waking time and wear time
    variables$vpa1_perc[j] <- variables$vpa1_min[j] / time_awake[j]
    variables$vpa30_perc[j] <- variables$vpa30_min[j] / time_awake[j]
    variables$vpa60_perc[j] <- variables$vpa60_min[j] / time_awake[j]
    
    ##### Total time spend in MVPA expressed as a percent of waking time and wear time
    variables$mvpa1_perc[j] <- variables$mvpa1_min[j] / time_awake[j]
    variables$mvpa30_perc[j] <- variables$mvpa30_min[j] / time_awake[j]
    variables$mvpa60_perc[j] <- variables$mvpa60_min[j] / time_awake[j]
  }
}

#### Cadence grouping ----

##### Finding initial cadence values
cadence30 <- list()
cadence60 <- list()
for (j in 1:length(good_days)) {
  fc <- dplyr::filter(sec_by_sec, wear_day == good_days[j])
  rownum <- seq(1, nrow(fc))
  group_column_30 <- ceiling(rownum / 30)
  group_column_60 <- ceiling(rownum / 60)
  fc30 <- cbind(fc, group_column_30)
  fc60 <- cbind(fc, group_column_60)
  cadence30[[j]] <- dplyr::group_by(fc30, group_column_30) |>
    dplyr::summarise(cadence = max(steps) - min(steps))
  cadence60[[j]] <- dplyr::group_by(fc60, group_column_60) |>
    dplyr::summarise(cadence = max(steps) - min(steps))
}

##### Finding stepping time and amount
variables$step_time_75_30 <- 0
variables$step_num_75_30 <- 0
variables$step_time_100_30 <- 0
variables$step_num_100_30 <- 0
variables$step_time_75_60 <- 0
variables$step_num_75_60 <- 0
variables$step_time_100_60 <- 0
variables$step_num_100_60 <- 0
for (j in 1:length(good_days)) {
  ## Stepping time cadence >= 75 for 30 and 60s 
  variables$step_time_75_30[j] <- sum(ifelse(cadence30[[j]]$cadence >= 75, 1, 0))
  variables$step_time_75_60[j] <- sum(ifelse(cadence60[[j]]$cadence >= 75, 1, 0))
  
  ## Number of steps of cadence >= 75 for 30 and 60s
  variables$step_num_75_30[j] <- sum(cadence30[[j]][cadence30[[j]]$cadence >= 75, ]$cadence)
  variables$step_num_75_60[j] <- sum(cadence60[[j]][cadence60[[j]]$cadence >= 75, ]$cadence)
  
  ## Stepping time cadence >= 100 for 30 and 60s
  variables$step_time_100_30[j] <- sum(ifelse(cadence30[[j]]$cadence >= 100, 1, 0))
  variables$step_time_100_60[j] <- sum(ifelse(cadence60[[j]]$cadence >= 100, 1, 0))
  
  ## Number of steps of cadence >= 100 for 30 and 60s
  variables$step_num_100_30[j] <- sum(cadence30[[j]][cadence30[[j]]$cadence >= 100, ]$cadence)
  variables$step_num_100_60[j] <- sum(cadence60[[j]][cadence60[[j]]$cadence >= 100, ]$cadence)
}

### Activity data during sleep windows ----

#### Finding sleep windows ----
sleep_window <- list()
for (j in 1:length(good_days)) {
  sleep_window[[j]] <- day_total[[j]][day_total[[j]]$sleep_loop == 1, ]
}

#### Position/activity ----
#### Calculating the number of certain activity per time period of interest
variables$steps_sleep <- NA
variables$step_min_sleep <- NA
variables$stand_min_sleep <- NA
variables$upright_min_sleep <- NA
variables$sed_min_sleep <- NA
for (j in 1:length(good_days)) {
  ### Steps per day
  if (nrow(sleep_window[[j]]) != 0) {
    variables$steps_sleep[j] <- sleep_window[[j]][dim(sleep_window[[j]])[1], ]$steps - 
      sleep_window[[j]][1, ]$steps
    
    ### Steps per minute per day
    variables$step_min_sleep[j] <- sum(ifelse(sleep_window[[j]]$activity == 2, 
                                              1, 0)) / 60
    
    ### Stand minutes per day
    variables$stand_min_sleep[j] <- sum(ifelse(sleep_window[[j]]$activity == 1, 
                                               1, 0)) / 60
    
    ### Upright minutes per day
    variables$upright_min_sleep[j] <- variables$stand_min_sleep[j] + 
      variables$step_min_sleep[j]
    
    ### Sedentary minutes per day (i.e., how many minutes did they sleep per day
    ### not counting naps)
    variables$sed_min_sleep[j] <- sum(ifelse(sleep_window[[j]]$activity == 0, 
                                             1, 0)) / 60
  }
}

#### Sit to stand transitions ----
variables$sitstand_sleep <- NA
for (j in 1:length(good_days)) {
  if (nrow(sleep_window[[j]]) != 0) {
    variables$sitstand_sleep[j] <- breaks.AP(sleep_window[[j]]$ap.posture)
  }
}

### Obtaining total wear time ----

#### Awake time ----
variables$wakewear_min <- time_awake

#### Sleep time ----
variables$sleepwear_min <- variables$sed_min_sleep + variables$stand_min_sleep +
  variables$step_min_sleep
variables$sleepwear_min[is.na(variables$sleepwear_min)] <- 0

#### Nap time ----
nap_window <- list()
nap_window <- lapply(X = day_total, 
                     FUN = function(x) {x[which(x$nap_loop == 1), ]})
variables$napwear_min <- unlist(lapply(X = nap_window, 
                                       FUN = function(x) {sum(x$nap_loop) / 60}))

#### Total wear time ----
variables$wear_min <- variables$wakewear_min + variables$sleepwear_min + 
  variables$napwear_min

### Determining valid wear days ----
### This is based on total device wear time
variables <- variables[order(variables$wear_day), ]
variables$valid_day <- NA
variables$valid_day[1] <- ifelse(variables$wear_day[1] == 1, 0, NA)
variables$valid_day[nrow(variables)] <- ifelse(variables$wear_day[nrow(variables)] == 9,
                                               0, NA)
variables$valid_day <- ifelse(variables$wakewear_min >= 600 & is.na(variables$valid_day), 
                              1, 0)
if (variables$wear_day[1] == 1) {
  if (sum(variables$valid_day) < 7 & variables$wakewear_min[1] >= 600) {
    variables$valid_day[1] <- 1
  } else {
    variables$valid_day[1] <- variables$valid_day[1]
  }
}

## Relocating variables ----
variables <- dplyr::relocate(variables, days, .after = wear_day)
variables <- dplyr::relocate(variables, weekday, .after = days)
variables <- dplyr::relocate(variables, weekend, .after = weekday)
variables <- dplyr::relocate(variables, day_of_week, .after = weekend)
variables <- dplyr::relocate(variables, steps, .after = day_of_week)
variables <- dplyr::relocate(variables, wear_min, .after = steps)
variables <- dplyr::relocate(variables, wakewear_min, .after = wear_min)
variables <- dplyr::relocate(variables, sleepwear_min, .after = wakewear_min)
variables <- dplyr::relocate(variables, napwear_min, .after = sleepwear_min)
variables <- dplyr::relocate(variables, valid_day, .after = napwear_min)
variables <- dplyr::relocate(variables, step_min, .after = valid_day)
variables <- dplyr::relocate(variables, stand_min, .after = step_min)
variables <- dplyr::relocate(variables, upright_min, .after = stand_min)
variables <- dplyr::relocate(variables, sed_min, .after = upright_min)
variables <- dplyr::relocate(variables, sed30_bout, .after = sed_min)
variables <- dplyr::relocate(variables, sed60_bout, .after = sed30_bout)
variables <- dplyr::relocate(variables, sed30_min, .after = sed60_bout)
variables <- dplyr::relocate(variables, sed60_min, .after = sed30_min)
variables <- dplyr::relocate(variables, mean_sed_bout, .after = sed60_min)
variables <- dplyr::relocate(variables, sed_perc, .after = mean_sed_bout)
variables <- dplyr::relocate(variables, sitstand, .after = sed_perc)
variables <- dplyr::relocate(variables, met_hrs, .after = sitstand)
variables <- dplyr::relocate(variables, lpa1_min, .after = met_hrs)
variables <- dplyr::relocate(variables, mpa1_min, .after = lpa1_min)
variables <- dplyr::relocate(variables, vpa1_min, .after = mpa1_min)
variables <- dplyr::relocate(variables, mvpa1_min, .after = vpa1_min)
variables <- dplyr::relocate(variables, lpa30_min, .after = mvpa1_min)
variables <- dplyr::relocate(variables, mpa30_min, .after = lpa30_min)
variables <- dplyr::relocate(variables, vpa30_min, .after = mpa30_min)
variables <- dplyr::relocate(variables, mvpa30_min, .after = vpa30_min)
variables <- dplyr::relocate(variables, lpa60_min, .after = mvpa30_min)
variables <- dplyr::relocate(variables, mpa60_min, .after = lpa60_min)
variables <- dplyr::relocate(variables, vpa60_min, .after = mpa60_min)
variables <- dplyr::relocate(variables, mvpa60_min, .after = vpa60_min)
variables <- dplyr::relocate(variables, guideline_minbout_10, .after = mvpa60_min)
variables <- dplyr::relocate(variables, guideline_numbout_10, .after = guideline_minbout_10)
variables <- dplyr::relocate(variables, lpa1_perc, .after = guideline_numbout_10)
variables <- dplyr::relocate(variables, mpa1_perc, .after = lpa1_perc)
variables <- dplyr::relocate(variables, vpa1_perc, .after = mpa1_perc)
variables <- dplyr::relocate(variables, mvpa1_perc, .after = vpa1_perc)
variables <- dplyr::relocate(variables, lpa30_perc, .after = mvpa1_perc)
variables <- dplyr::relocate(variables, mpa30_perc, .after = lpa30_perc)
variables <- dplyr::relocate(variables, vpa30_perc, .after = mpa30_perc)
variables <- dplyr::relocate(variables, mvpa30_perc, .after = vpa30_perc)
variables <- dplyr::relocate(variables, lpa60_perc, .after = mvpa30_perc)
variables <- dplyr::relocate(variables, mpa60_perc, .after = lpa60_perc)
variables <- dplyr::relocate(variables, vpa60_perc, .after = mpa60_perc)
variables <- dplyr::relocate(variables, mvpa60_perc, .after = vpa60_perc)
variables <- dplyr::relocate(variables, step_time_75_30, .after = mvpa60_perc)
variables <- dplyr::relocate(variables, step_num_75_30, .after = step_time_75_30)
variables <- dplyr::relocate(variables, step_time_100_30, .after = step_num_75_30)
variables <- dplyr::relocate(variables, step_num_100_30, .after = step_time_100_30)
variables <- dplyr::relocate(variables, step_time_75_60, .after = step_num_100_30)
variables <- dplyr::relocate(variables, step_num_75_60, .after = step_time_75_60)
variables <- dplyr::relocate(variables, step_time_100_60, .after = step_num_75_60)
variables <- dplyr::relocate(variables, step_num_100_60, .after = step_time_100_60)
variables <- dplyr::relocate(variables, steps_sleep, .after = step_num_100_60)
variables <- dplyr::relocate(variables, step_min_sleep, .after = steps_sleep)
variables <- dplyr::relocate(variables, stand_min_sleep, .after = step_min_sleep)
variables <- dplyr::relocate(variables, upright_min_sleep, .after = stand_min_sleep)
variables <- dplyr::relocate(variables, sed_min_sleep, .after = upright_min_sleep)
variables <- dplyr::relocate(variables, sitstand_sleep, .after = sed_min_sleep)

## Adding in days labeled as invalid due missing activity data ----
hold1 <- all_days[all_days %notin% good_days]
if (length(hold1) != 0) {
  hold2 <- data.frame(matrix(data = NA, 
                             nrow = length(hold1), ncol = dim(variables)[2]))
  names(hold2) <- names(variables)
  hold2$wear_day <- hold1
  variables <- dplyr::bind_rows(variables, hold2)
  variables <- variables[order(variables$wear_day), ]
}

# Step 10. Exporting ----
hold1 <- "Y:/UP & Rural 24-7/Participant Data/"

dat_source == hold1[1]
  paths <- paste0("Y:/UP & Rural 24-7/Participant Data/",
                  subject_id,
                  "/")
  readr::write_csv(x = dat,
                   file = paste0(paths, subject_id, "_eventfile.csv"),
                   na = "")
  readr::write_csv(x = variables,
                   file = paste0(paths, subject_id, "_daily_values.csv"),
                   na = "")
  pdf(file = paste0(paths, subject_id, "_graphs.pdf"), width = 11, height = 8.5)
  plot(g)
  dev.off()
