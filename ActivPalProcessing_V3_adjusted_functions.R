## Since work and monitor off times are dependent on diary information and have
## the same setup, I will create a function that gives me the start and end times
## for each activity.
start_end <- function(diarydate, start_time, end_time, activity_start, activity_end) {
  activity_start <- as.POSIXct(NA, tz = "UTC")
  activity_end <- as.POSIXct(NA, tz = "UT")
  if (!is.na(start_time) & !is.na(end_time)) {
    activity_start <- as.POSIXct(paste0(diarydate, " ", substr(start_time, 1, 2), ":", substr(start_time, 4, 5)), tz = "UTC")
    activity_end <- as.POSIXct(paste0(diarydate, " ", substr(end_time, 1, 2), ":", substr(end_time, 4, 5)), tz = "UTC")
  } else {
    activity_start <- NA
    activity_end <- NA
  }
  return(list(activity_start, activity_end))
}

## Adjusting Kate Lyden's functions to fit our needs
MPA.min.AP <-function (mets, epoch = 1)
{
  mpa.mins <- sum((mets >= 3)& (mets < 6))/(60/epoch)
  if (is.na(mpa.mins) ) {
    mpa.mins <- 0}
  return(mpa.mins)
}

VPA.min.AP <-function (mets, epoch = 1) 
{
  vpa.mins <- sum(mets >= 6)/(60/epoch)
  if (is.na(vpa.mins) ) {
    vpa.mins <- 0}
  return(vpa.mins)
}

mvpa.min.AP <-function (mets, epoch = 1)
{
  mvpa.mins <- sum(mets >= 3)/(60/epoch)
  if (is.na(mvpa.mins) ) {
    mvpa.mins <- 0}
  return(mvpa.mins)
}

guideline.bouts.min1 <- function (mets) 
{
  total.active.time.in.bouts <- 0
  mets.length <- length(mets)
  one <- mets[-mets.length]
  two <- mets[-1]
  trans <- c(FALSE, ((one < 3) & (two >= 3)) | ((one >= 3) & 
                                                  (two < 3)))
  trans.inds <- c(1, seq_along(mets)[trans], (mets.length + 
                                                1))
  durations <- trans.inds[-1] - trans.inds[-length(trans.inds)]
  active.interval <- rep(FALSE, length = length(durations))
  if (mets[1] < 3) 
    active.interval <- rep(c(FALSE, TRUE), length = length(durations))
  if (mets[1] >= 3) 
    active.interval <- rep(c(TRUE, FALSE), length = length(durations))
  bout.starts <- c()
  bout.ends <- c()
  active.inds <- seq_along(durations)[active.interval]
  if (length(active.inds) > 0) {
    possible.bout.active.inds <- 1
    possible.bout.active.inds.length <- 1
    bout.inactivity <- rep(0, durations[active.inds[possible.bout.active.inds]])
    end.possible.bout <- FALSE
    while (possible.bout.active.inds[possible.bout.active.inds.length] < 
           length(active.inds)) {
      bout.inactivity.with.next.interval <- c(bout.inactivity, 
                                              rep(1, durations[active.inds[possible.bout.active.inds[possible.bout.active.inds.length]] + 
                                                                 1]))
      secs.to.extract <- min(length(bout.inactivity.with.next.interval), 
                             60)
      last.1.min.of.possible.bout <- bout.inactivity.with.next.interval[seq_len(secs.to.extract) + 
                                                                          (length(bout.inactivity.with.next.interval) - 
                                                                             secs.to.extract)]
      if (sum(last.1.min.of.possible.bout) <= 2 * 60) {
        bout.inactivity <- c(bout.inactivity.with.next.interval, 
                             rep(0, durations[active.inds[possible.bout.active.inds[possible.bout.active.inds.length]] + 
                                                2]))
        possible.bout.active.inds <- c(possible.bout.active.inds, 
                                       possible.bout.active.inds[possible.bout.active.inds.length] + 
                                         1)
        possible.bout.active.inds.length <- possible.bout.active.inds.length + 
          1
        if (possible.bout.active.inds[possible.bout.active.inds.length] == 
            length(active.inds)) {
          end.possible.bout <- TRUE
        }
      }
      else {
        end.possible.bout <- TRUE
      }
      if (end.possible.bout) {
        possible.bout.length <- sum(durations[seq(from = active.inds[possible.bout.active.inds[1]], 
                                                  to = active.inds[possible.bout.active.inds[possible.bout.active.inds.length]])])
        if (possible.bout.length >= 10 * 60) {
          bout.starts <- c(bout.starts, sum(durations[seq_len(active.inds[possible.bout.active.inds[1]] - 
                                                                1)]) + 1)
          bout.ends <- c(bout.ends, sum(durations[seq_len(active.inds[possible.bout.active.inds[possible.bout.active.inds.length]])]))
          total.active.time.in.bouts <- total.active.time.in.bouts + 
            length(bout.inactivity) - sum(bout.inactivity)
        }
        else {
          seconds.missing <- 10 * 60 - possible.bout.length
          if (seconds.missing + sum(bout.inactivity) <= 
              2 * 60) {
            bout.start <- sum(durations[seq_len(active.inds[possible.bout.active.inds[1]] - 
                                                  1)]) + 1
            bout.end <- sum(durations[seq_len(active.inds[possible.bout.active.inds[possible.bout.active.inds.length]])])
            if (length(bout.ends) > 0) {
              last.bout.end <- bout.ends[length(bout.ends)]
            }
            else {
              last.bout.end <- 0
            }
            seconds.to.add <- min(bout.start - last.bout.end + 
                                    1, seconds.missing)
            bout.start <- bout.start - seconds.to.add
            seconds.missing <- seconds.missing - seconds.to.add
            if (seconds.missing > 0) {
              bout.end <- bout.end + seconds.missing
            }
            bout.starts <- c(bout.starts, bout.start)
            bout.ends <- c(bout.ends, bout.end)
            total.active.time.in.bouts <- total.active.time.in.bouts + 
              length(bout.inactivity) - sum(bout.inactivity)
          }
        }
        possible.bout.active.inds <- possible.bout.active.inds[possible.bout.active.inds.length] + 
          1
        possible.bout.active.inds.length <- 1
        end.possible.bout <- FALSE
        if (possible.bout.active.inds[possible.bout.active.inds.length] < 
            length(active.inds)) {
          bout.inactivity <- rep(0, durations[active.inds[possible.bout.active.inds]])
        }
      }
    }
  }
  return(sum((bout.ends + 1) - bout.starts)/60)
}

guideline.bouts.num1 <- function (mets) 
{
  total.active.time.in.bouts <- 0
  mets.length <- length(mets)
  one <- mets[-mets.length]
  two <- mets[-1]
  trans <- c(FALSE, ((one < 3) & (two >= 3)) | ((one >= 3) & 
                                                  (two < 3)))
  trans.inds <- c(1, seq_along(mets)[trans], (mets.length + 
                                                1))
  durations <- trans.inds[-1] - trans.inds[-length(trans.inds)]
  active.interval <- rep(FALSE, length = length(durations))
  if (mets[1] < 3) 
    active.interval <- rep(c(FALSE, TRUE), length = length(durations))
  if (mets[1] >= 3) 
    active.interval <- rep(c(TRUE, FALSE), length = length(durations))
  bout.starts <- c()
  bout.ends <- c()
  active.inds <- seq_along(durations)[active.interval]
  if (length(active.inds) > 0) {
    possible.bout.active.inds <- 1
    possible.bout.active.inds.length <- 1
    bout.inactivity <- rep(0, durations[active.inds[possible.bout.active.inds]])
    end.possible.bout <- FALSE
    while (possible.bout.active.inds[possible.bout.active.inds.length] < 
           length(active.inds)) {
      bout.inactivity.with.next.interval <- c(bout.inactivity, 
                                              rep(1, durations[active.inds[possible.bout.active.inds[possible.bout.active.inds.length]] + 
                                                                 1]))
      secs.to.extract <- min(length(bout.inactivity.with.next.interval), 
                             60)
      last.1.min.of.possible.bout <- bout.inactivity.with.next.interval[seq_len(secs.to.extract) + 
                                                                          (length(bout.inactivity.with.next.interval) - 
                                                                             secs.to.extract)]
      if (sum(last.1.min.of.possible.bout) <= 2 * 60) {
        bout.inactivity <- c(bout.inactivity.with.next.interval, 
                             rep(0, durations[active.inds[possible.bout.active.inds[possible.bout.active.inds.length]] + 
                                                2]))
        possible.bout.active.inds <- c(possible.bout.active.inds, 
                                       possible.bout.active.inds[possible.bout.active.inds.length] + 
                                         1)
        possible.bout.active.inds.length <- possible.bout.active.inds.length + 
          1
        if (possible.bout.active.inds[possible.bout.active.inds.length] == 
            length(active.inds)) {
          end.possible.bout <- TRUE
        }
      }
      else {
        end.possible.bout <- TRUE
      }
      if (end.possible.bout) {
        possible.bout.length <- sum(durations[seq(from = active.inds[possible.bout.active.inds[1]], 
                                                  to = active.inds[possible.bout.active.inds[possible.bout.active.inds.length]])])
        if (possible.bout.length >=  60) {
          bout.starts <- c(bout.starts, sum(durations[seq_len(active.inds[possible.bout.active.inds[1]] - 
                                                                1)]) + 1)
          bout.ends <- c(bout.ends, sum(durations[seq_len(active.inds[possible.bout.active.inds[possible.bout.active.inds.length]])]))
          total.active.time.in.bouts <- total.active.time.in.bouts + 
            length(bout.inactivity) - sum(bout.inactivity)
        }
        else {
          seconds.missing <-  60 - possible.bout.length
          if (seconds.missing + sum(bout.inactivity) <= 
              2 * 60) {
            bout.start <- sum(durations[seq_len(active.inds[possible.bout.active.inds[1]] - 
                                                  1)]) + 1
            bout.end <- sum(durations[seq_len(active.inds[possible.bout.active.inds[possible.bout.active.inds.length]])])
            if (length(bout.ends) > 0) {
              last.bout.end <- bout.ends[length(bout.ends)]
            }
            else {
              last.bout.end <- 0
            }
            seconds.to.add <- min(bout.start - last.bout.end + 
                                    1, seconds.missing)
            bout.start <- bout.start - seconds.to.add
            seconds.missing <- seconds.missing - seconds.to.add
            if (seconds.missing > 0) {
              bout.end <- bout.end + seconds.missing
            }
            bout.starts <- c(bout.starts, bout.start)
            bout.ends <- c(bout.ends, bout.end)
            total.active.time.in.bouts <- total.active.time.in.bouts + 
              length(bout.inactivity) - sum(bout.inactivity)
          }
        }
        possible.bout.active.inds <- possible.bout.active.inds[possible.bout.active.inds.length] + 
          1
        possible.bout.active.inds.length <- 1
        end.possible.bout <- FALSE
        if (possible.bout.active.inds[possible.bout.active.inds.length] < 
            length(active.inds)) {
          bout.inactivity <- rep(0, durations[active.inds[possible.bout.active.inds]])
        }
      }
    }
  }
  return(length(bout.starts))
}

breaks.AP <- function (posture) 
{
  y <- posture
  n <- length(y)
  mmm <- length(y)
  one <- y[-mmm]
  two <- y[-1]
  trans.up <- (one == "0") & (two != "0")
  num.up.AP <- sum(trans.up, na.rm = T)
  if (num.up.AP == 0) 
    num.up.AP <- 0
  return(num.up.AP = num.up.AP)
}