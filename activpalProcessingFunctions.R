activpal.file.reader <-
  function(file.name.and.path)
  {
    data <- read.csv(file.name.and.path, stringsAsFactors=FALSE)
    data <- data[,(1:6)]
    names(data) <- c("time","datacount","interval","activity","cumulativesteps","methrs")
    
    data$time <- sub("#","",data$time)
    data$time <- sub("#","",data$time)
    data[,2] <- as.numeric(as.character(data[,2]))
    data[,3] <- as.numeric(as.character(data[,3]))
    data[,4] <- as.numeric(as.character(data[,4]))
    data[,5] <- as.numeric(as.character(data[,5]))*2 #event files have half the actual number of steps for some reason
    data[,6] <- as.numeric(as.character(data[,6]))
    
    t <- dim(data)[1]
    
    data <- data[!(data[,"time"] == "1899-12-30"),]
    data <- data[!(data[,"time"] == "0"),]
    n <- dim(data)[1]		
    
    if(is.character(data$time)==TRUE&t==n)
    {
      data$time <- as.numeric(data$time)
      data$time <- as.POSIXct(as.Date(data$time,origin="1899-12-30"))
      data$time <- as.POSIXlt(data$time,tz="GMT")
      data$time <- strptime(data$time,format="%Y-%m-%d %H:%M:%S")
    }
    
    return(data)
  }

breaks.AP <-
  function(posture)
  {
    y <- posture
    n <- length(y)
    
    mmm <- length(y)
    one <- y[-mmm]
    two <- y[-1]
    
    # transitions from sed to not
    trans.up <- 
      (one=="0")&(two!="0")
    num.up.AP <- sum(trans.up,na.rm=T)
    if (num.up.AP==0)
      num.up.AP <- NA
    return(num.up.AP=num.up.AP)	
  }

guideline.bouts.min <-
  function(mets) {
    total.active.time.in.bouts <- 0
    
    # indices where transitions take place
    mets.length <- length(mets)
    one <- mets[-mets.length]
    two <- mets[-1]
    
    trans <- c( FALSE, ((one<3)&(two>=3)) | ((one>=3)&(two<3)) )
    trans.inds <- c(1, seq_along(mets)[trans], (mets.length+1))
    
    # how long are the periods of activity and inactivity
    durations <- trans.inds[-1]-trans.inds[-length(trans.inds)]
    
    # identify if interval is activity or inactivity (they alternate)
    active.interval <- rep(FALSE,length=length(durations))
    
    if (mets[1]<3)
      active.interval <- rep(c(FALSE, TRUE),length=length(durations))
    if (mets[1]>=3)
      active.interval <- rep(c(TRUE, FALSE),length=length(durations))
    
    
    # Create some empty vectors which will be used to keep track of the
    # start and end points of the bouts in the durations vector.
    bout.starts <- c()
    bout.ends <- c()
    
    # Create some variables which will be used in constructing the bouts.
    active.inds <- seq_along(durations)[active.interval]
    if(length(active.inds) > 0) {
      possible.bout.active.inds <- 1
      possible.bout.active.inds.length <- 1
      bout.inactivity <- rep(0, durations[active.inds[possible.bout.active.inds]])
      end.possible.bout <- FALSE
      
      while(possible.bout.active.inds[possible.bout.active.inds.length] < length(active.inds)) {
        # Determine if adding the next inactive interval to the current possible bout
        # would cause the last 10 minutes of the bout to be more than 20% inactive
        
        bout.inactivity.with.next.interval <- c(bout.inactivity, rep(1, durations[active.inds[possible.bout.active.inds[possible.bout.active.inds.length]] + 1]))
        secs.to.extract <- min(length(bout.inactivity.with.next.interval), 10*60)
        last.10.min.of.possible.bout <- bout.inactivity.with.next.interval[seq_len(secs.to.extract) + (length(bout.inactivity.with.next.interval) - secs.to.extract)]
        
        if( sum(last.10.min.of.possible.bout) <= 2*60 ) {
          # If adding the next inactive interval means the last 10 min has <= 2 min of inactivity,
          # add it and the next active interval to the possible-bout.
          bout.inactivity <- c(bout.inactivity.with.next.interval, rep(0, durations[active.inds[possible.bout.active.inds[possible.bout.active.inds.length]] + 2]))
          possible.bout.active.inds <- c(possible.bout.active.inds, possible.bout.active.inds[possible.bout.active.inds.length] + 1)
          possible.bout.active.inds.length <- possible.bout.active.inds.length + 1
          if(possible.bout.active.inds[possible.bout.active.inds.length] == length(active.inds)) {
            # If we just added the last active interval to the possible bout, end the possible bout.
            end.possible.bout <- TRUE
          }
        } else {
          # If adding the next inactive interval means the last 10 min has > 2 min of inactivity,
          # stop building this bout.
          end.possible.bout <- TRUE
        }
        
        if(end.possible.bout) {
          # If the possible bout is long enough, add it to the list of bouts.
          # Reset to start building the next possible bout.
          
          possible.bout.length <- sum(durations[seq(from=active.inds[possible.bout.active.inds[1]], to=active.inds[possible.bout.active.inds[possible.bout.active.inds.length]])]) 
          if(possible.bout.length >= 10*60) {
            # If the total duration of the possible bout is >= 10 min, it is a bout.
            
            # The start position of the bout is recorded as an index in the input mets vector.
            # This is calculated as the sum of the durations of all intervals before the start of this bout, plus 1
            bout.starts <- c(bout.starts, sum(durations[seq_len(active.inds[possible.bout.active.inds[1]] - 1)]) + 1)
            
            # The end position of the bout is recorded as an index in the input mets vector.
            # This is calculated as the sum of the durations of all intervals up through the end this bout
            bout.ends <- c(bout.ends, sum(durations[seq_len(active.inds[possible.bout.active.inds[possible.bout.active.inds.length]])]))
            
            # add the active time in this bout to the total active time in all bouts
            total.active.time.in.bouts <- total.active.time.in.bouts + length(bout.inactivity) - sum(bout.inactivity)
          } else {
            # If it is < 10 min, see if we can add on some inactive time to the beginning and/or end
            # to reach the 10 min threshold and make it into a bout.  Otherwise, discard it.
            
            # Calculate the number of seconds needed to fill in to reach the 10 minute threshold.
            seconds.missing <- 10*60 - possible.bout.length
            if(seconds.missing + sum(bout.inactivity) <= 2*60) {
              # If the number of seconds we need to fill in to reach 10 minutes
              # would not push us over the 10 minute threshold, add in time
              # from the beginning and then from the end to reach 10 minutes.
              
              # Get the current start time and end time of the bout, calculated as above.
              bout.start <- sum(durations[seq_len(active.inds[possible.bout.active.inds[1]] - 1)]) + 1
              bout.end <- sum(durations[seq_len(active.inds[possible.bout.active.inds[possible.bout.active.inds.length]])])
              
              # Find the number of seconds we can add on to the beginning.
              # We do not want to overlap with the previous bout.
              if(length(bout.ends) > 0) {
                last.bout.end <- bout.ends[length(bout.ends)]
              } else {
                last.bout.end <- 0
              }
              seconds.to.add <- min(bout.start - last.bout.end + 1, seconds.missing)
              bout.start <- bout.start - seconds.to.add
              seconds.missing <- seconds.missing - seconds.to.add
              
              # If necessary, add the rest of the time on to the end.
              # (We know there are "enough" extra seconds at the end because we exceeded the 2 minute threshold
              # when we tried to add on the whole inactive interval.)
              if(seconds.missing > 0) {
                bout.end <- bout.end + seconds.missing
              }
              
              # Add the new bout to the list of bouts.
              bout.starts <- c(bout.starts, bout.start)
              bout.ends <- c(bout.ends, bout.end)
              
              # add the active time in this bout to the total active time in all bouts
              total.active.time.in.bouts <- total.active.time.in.bouts + length(bout.inactivity) - sum(bout.inactivity)
            }
          }
          
          # Set up to start building the next possible bout.
          possible.bout.active.inds <- possible.bout.active.inds[possible.bout.active.inds.length] + 1
          possible.bout.active.inds.length <- 1
          
          end.possible.bout <- FALSE
          
          if(possible.bout.active.inds[possible.bout.active.inds.length] < length(active.inds)) {
            bout.inactivity <- rep(0, durations[active.inds[possible.bout.active.inds]])
          }
        }
      }
    }
    
    
    return(sum((bout.ends + 1) - bout.starts)/60)
    
  }

guideline.bouts.num <-
  function(mets) {
    total.active.time.in.bouts <- 0
    
    # indices where transitions take place
    mets.length <- length(mets)
    one <- mets[-mets.length]
    two <- mets[-1]
    
    trans <- c( FALSE, ((one<3)&(two>=3)) | ((one>=3)&(two<3)) )
    trans.inds <- c(1, seq_along(mets)[trans], (mets.length+1))
    
    # how long are the periods of activity and inactivity
    durations <- trans.inds[-1]-trans.inds[-length(trans.inds)]
    
    # identify if interval is activity or inactivity (they alternate)
    active.interval <- rep(FALSE,length=length(durations))
    
    if (mets[1]<3)
      active.interval <- rep(c(FALSE, TRUE),length=length(durations))
    if (mets[1]>=3)
      active.interval <- rep(c(TRUE, FALSE),length=length(durations))
    
    
    # Create some empty vectors which will be used to keep track of the
    # start and end points of the bouts in the durations vector.
    bout.starts <- c()
    bout.ends <- c()
    
    # Create some variables which will be used in constructing the bouts.
    active.inds <- seq_along(durations)[active.interval]
    if(length(active.inds) > 0) {
      possible.bout.active.inds <- 1
      possible.bout.active.inds.length <- 1
      bout.inactivity <- rep(0, durations[active.inds[possible.bout.active.inds]])
      end.possible.bout <- FALSE
      
      while(possible.bout.active.inds[possible.bout.active.inds.length] < length(active.inds)) {
        # Determine if adding the next inactive interval to the current possible bout
        # would cause the last 10 minutes of the bout to be more than 20% inactive
        
        bout.inactivity.with.next.interval <- c(bout.inactivity, rep(1, durations[active.inds[possible.bout.active.inds[possible.bout.active.inds.length]] + 1]))
        secs.to.extract <- min(length(bout.inactivity.with.next.interval), 10*60)
        last.10.min.of.possible.bout <- bout.inactivity.with.next.interval[seq_len(secs.to.extract) + (length(bout.inactivity.with.next.interval) - secs.to.extract)]
        
        if( sum(last.10.min.of.possible.bout) <= 2*60 ) {
          # If adding the next inactive interval means the last 10 min has <= 2 min of inactivity,
          # add it and the next active interval to the possible-bout.
          bout.inactivity <- c(bout.inactivity.with.next.interval, rep(0, durations[active.inds[possible.bout.active.inds[possible.bout.active.inds.length]] + 2]))
          possible.bout.active.inds <- c(possible.bout.active.inds, possible.bout.active.inds[possible.bout.active.inds.length] + 1)
          possible.bout.active.inds.length <- possible.bout.active.inds.length + 1
          if(possible.bout.active.inds[possible.bout.active.inds.length] == length(active.inds)) {
            # If we just added the last active interval to the possible bout, end the possible bout.
            end.possible.bout <- TRUE
          }
        } else {
          # If adding the next inactive interval means the last 10 min has > 2 min of inactivity,
          # stop building this bout.
          end.possible.bout <- TRUE
        }
        
        if(end.possible.bout) {
          # If the possible bout is long enough, add it to the list of bouts.
          # Reset to start building the next possible bout.
          
          possible.bout.length <- sum(durations[seq(from=active.inds[possible.bout.active.inds[1]], to=active.inds[possible.bout.active.inds[possible.bout.active.inds.length]])]) 
          if(possible.bout.length >= 10*60) {
            # If the total duration of the possible bout is >= 10 min, it is a bout.
            
            # The start position of the bout is recorded as an index in the input mets vector.
            # This is calculated as the sum of the durations of all intervals before the start of this bout, plus 1
            bout.starts <- c(bout.starts, sum(durations[seq_len(active.inds[possible.bout.active.inds[1]] - 1)]) + 1)
            
            # The end position of the bout is recorded as an index in the input mets vector.
            # This is calculated as the sum of the durations of all intervals up through the end this bout
            bout.ends <- c(bout.ends, sum(durations[seq_len(active.inds[possible.bout.active.inds[possible.bout.active.inds.length]])]))
            
            # add the active time in this bout to the total active time in all bouts
            total.active.time.in.bouts <- total.active.time.in.bouts + length(bout.inactivity) - sum(bout.inactivity)
          } else {
            # If it is < 10 min, see if we can add on some inactive time to the beginning and/or end
            # to reach the 10 min threshold and make it into a bout.  Otherwise, discard it.
            
            # Calculate the number of seconds needed to fill in to reach the 10 minute threshold.
            seconds.missing <- 10*60 - possible.bout.length
            if(seconds.missing + sum(bout.inactivity) <= 2*60) {
              # If the number of seconds we need to fill in to reach 10 minutes
              # would not push us over the 10 minute threshold, add in time
              # from the beginning and then from the end to reach 10 minutes.
              
              # Get the current start time and end time of the bout, calculated as above.
              bout.start <- sum(durations[seq_len(active.inds[possible.bout.active.inds[1]] - 1)]) + 1
              bout.end <- sum(durations[seq_len(active.inds[possible.bout.active.inds[possible.bout.active.inds.length]])])
              
              # Find the number of seconds we can add on to the beginning.
              # We do not want to overlap with the previous bout.
              if(length(bout.ends) > 0) {
                last.bout.end <- bout.ends[length(bout.ends)]
              } else {
                last.bout.end <- 0
              }
              seconds.to.add <- min(bout.start - last.bout.end + 1, seconds.missing)
              bout.start <- bout.start - seconds.to.add
              seconds.missing <- seconds.missing - seconds.to.add
              
              # If necessary, add the rest of the time on to the end.
              # (We know there are "enough" extra seconds at the end because we exceeded the 2 minute threshold
              # when we tried to add on the whole inactive interval.)
              if(seconds.missing > 0) {
                bout.end <- bout.end + seconds.missing
              }
              
              # Add the new bout to the list of bouts.
              bout.starts <- c(bout.starts, bout.start)
              bout.ends <- c(bout.ends, bout.end)
              
              # add the active time in this bout to the total active time in all bouts
              total.active.time.in.bouts <- total.active.time.in.bouts + length(bout.inactivity) - sum(bout.inactivity)
            }
          }
          
          # Set up to start building the next possible bout.
          possible.bout.active.inds <- possible.bout.active.inds[possible.bout.active.inds.length] + 1
          possible.bout.active.inds.length <- 1
          
          end.possible.bout <- FALSE
          
          if(possible.bout.active.inds[possible.bout.active.inds.length] < length(active.inds)) {
            bout.inactivity <- rep(0, durations[active.inds[possible.bout.active.inds]])
          }
        }
      }
    }
    
    
    return(length(bout.starts))
    
    #	return( c(length(bout.starts), sum((bout.ends + 1) - bout.starts)/60, total.active.time.in.bouts/60) )
  }

identifyDirectory <-
  function(path)
  {
    directory <- path
    return(directory)
  }

identifyStudy <-
  function(directory=directory,name.of.log.subjects)
  {
    list.subs <-read.csv(paste(directory,name.of.log.subjects,".csv",sep=""))
    study <- unique(as.character(list.subs$study))
    return(study)
  }

identifySubjects <-
  function(directory=directory,name.of.log.subjects)
  {
    list.subs <-read.csv(paste(directory,name.of.log.subjects,".csv",sep=""))
    subs <- unique(as.numeric(list.subs$id))
    return(subs)
  }

identifyVisits <-
  function(directory=directory,name.of.log.subjects)
  {
    list.subs <-read.csv(paste(directory,name.of.log.subjects,".csv",sep=""))
    visit <- unique(as.character(list.subs$visit))
    return(visit)
  }

lit.min.AP <-
  function(mets,posture,epoch=1)
  {
    lit.mins.temp <- sum((posture==2)&(mets>=1.5)&(mets<3))/(60/epoch)
    lit.mins.stand <- sum(posture==1)/(60/epoch)
    lit.mins <- lit.mins.temp+lit.mins.stand
    if (lit.mins==0)
      lit.mins <- NA
    return(lit.mins)
  }

mvpa.min.AP <-
  function(mets,epoch=1)
  {
    mvpa.mins <- sum(mets>=3)/(60/epoch)
    if (mvpa.mins==0)
      mvpa.mins <- NA
    return(mvpa.mins)
  }

on.off <-
  function(directory=directory,id,visit,name.of.log.on.off,data)
  {
    
    on.off.log <- read.csv(paste(directory,name.of.log.on.off,".csv",sep=""))
    on.off.log$id <- as.character(on.off.log$id)
    on.off.log <- on.off.log[on.off.log$id==id& on.off.log$visit==visit,]
    
    on.off.log$date.on <- paste(on.off.log$date.on.month,on.off.log$date.on.day,on.off.log$date.on.year,sep="/")
    on.off.log$time.on <- paste(on.off.log$time.on.hour,on.off.log$time.on.minute,on.off.log$time.on.seconds,sep=":")
    
    on.off.log$date.off <- paste(on.off.log$date.off.month,on.off.log$date.off.day,on.off.log$date.off.year,sep="/")
    on.off.log$time.off <- paste(on.off.log$time.off.hour,on.off.log$time.off.minute,on.off.log$time.off.seconds,sep=":")
    
    on.off.log$date.time.on <- paste(on.off.log$date.on, on.off.log$time.on, sep=" ")
    on.off.log$date.time.off <- paste(on.off.log$date.off, on.off.log$time.off, sep=" ")
    
    on.off.log$date.time.on <- strptime(on.off.log$date.time.on,"%m/%d/%Y %H:%M:%S")
    on.off.log$date.time.off <- strptime(on.off.log$date.time.off,"%m/%d/%Y %H:%M:%S")
    
    on.off.log$hours.on <- as.vector(difftime(strptime(on.off.log$date.time.off,format="%Y-%m-%d %H:%M:%S"),strptime(on.off.log$date.time.on,format="%Y-%m-%d %H:%M:%S"), units="hours"))
    
    #	if on/off times recorded - loop through and label time monitor is not worn
    if(dim(on.off.log)[1]>0)
    {
      data$off <- 1	
      for (t in (1:dim(on.off.log)[1]))
      {
        on <- strptime(on.off.log$date.time.on[t],"%Y-%m-%d %H:%M:%S")
        off <- strptime(on.off.log$date.time.off[t],"%Y-%m-%d %H:%M:%S")
        n <- dim(data)[1]
        inds <- (1:n)[((data$time>=on)&(data$time<=off))]
        if (length(inds)>0)
          data$off[inds] <- 0
      }
      if(dim(on.off.log)[1]==0)
        data$off <- "No.On.Off.Log"	
    }	#end bed loop
    return(data)
  }

process.AP <-
  function(directory,name.of.log.subjects,name.of.log.bed=NULL,name.of.log.on.off=NULL)
  {
    counter <- 1
    
    subs <- identifySubjects(directory, name.of.log.subjects)
    visit <- identifyVisits(directory, name.of.log.subjects)
    study <- identifyStudy(directory, name.of.log.subjects)
    
    for (s in subs)
    {
      for (v in visit)
      {
        print(s)
        print(v)
        print(study)
        
        temp <- Sys.glob(paste(directory,study,"_",s,"_",v,".csv",sep=""))
        
        if (length(temp)>0)
        {
          file.name.and.path <- paste(directory,study,"_",s,"_",v,".csv",sep="")
          
          temp <- activpal.file.reader(file.name.and.path)
          
          #		head(temp)
          #		dim(temp)
          
          n <- dim(temp)[1]		
          
          temp <- second.by.second(temp)
          
          #	loop to label sleep time
          
          bed.log.temp <- Sys.glob(paste(directory,name.of.log.bed,".csv",sep=""))
          
          temp$in.bed <- 1
          temp$day.for.wearer <- NA	
          
          if(length(bed.log.temp>0))
          { 
            bed.log <- read.csv(bed.log.temp)
            bed.log$id <- as.character(bed.log$id)
            bed.log <- bed.log[bed.log$id==s&bed.log$visit==v,]
            
            if(dim(bed.log)[1]>0)
            {
              
              bed.log$date.out <- paste(bed.log$date.out.month,bed.log$date.out.day,bed.log$date.out.year,sep="/")
              bed.log$time.out <- paste(bed.log$time.out.hour,bed.log$time.out.minute,bed.log$time.out.seconds,sep=":")
              
              bed.log$date.in <- paste(bed.log$date.in.month,bed.log$date.in.day,bed.log$date.in.year,sep="/")
              bed.log$time.in <- paste(bed.log$time.in.hour,bed.log$time.in.minute,bed.log$time.in.seconds,sep=":")
              
              bed.log$date.time.out <- paste(bed.log$date.out, bed.log$time.out, sep=" ")
              bed.log$date.time.in <- paste(bed.log$date.in, bed.log$time.in, sep=" ")
              
              bed.log$date.time.out <- strptime(bed.log$date.time.out,"%m/%d/%Y %H:%M:%S")
              bed.log$date.time.in <- strptime(bed.log$date.time.in,"%m/%d/%Y %H:%M:%S")
              
              bed.log$hours.up <- as.vector(difftime(strptime(bed.log$date.time.in,format="%Y-%m-%d %H:%M:%S"),strptime(bed.log$date.time.out,format="%Y-%m-%d %H:%M:%S"), units="hours"))
              
              #	if bed times recorded - loop through and label time in bed
              
              bed.log$day.for.wearer <- 1:dim(bed.log)[1]
              
              for (t in (1:dim(bed.log)[1]))
              {
                wake <- strptime(bed.log$date.time.out[t],"%Y-%m-%d %H:%M:%S")
                bed <- strptime(bed.log$date.time.in[t],"%Y-%m-%d %H:%M:%S")
                n <- dim(temp)[1]
                inds <- (1:n)[((temp$time>=wake)&(temp$time<=bed))]
                
                if (length(inds)>0)
                  temp$in.bed[inds] <- 0
                temp$day.for.wearer[inds] <- bed.log$day.for.wearer[t]
              }
              
              inds.time.awake <- (1:(dim(temp)[1]))[temp$in.bed==0]
              a <- length(inds.time.awake)
              if(a==0)
                temp$in.bed <- "AP and bed.log do not match"
              
            }
            
            if(dim(bed.log)[1]==0)
              temp$in.bed <- "Subject/Visit not in bed.log"					
          }	#end sleep time loop
          
          if(length(bed.log.temp)==0)
            temp$in.bed <- "No.Bed.Log"	
          
          
          #	loop to remove label on/off time
          on.off.log.temp <- Sys.glob(paste(directory,name.of.log.on.off,".csv",sep=""))
          
          temp$off <- 1	
          
          if(length(on.off.log.temp>0))
          { 
            on.off.log <- read.csv(on.off.log.temp)
            on.off.log$id <- as.character(on.off.log$id)
            on.off.log <- on.off.log[on.off.log$id==s& on.off.log$visit==v,]
            
            if(dim(on.off.log)[1]>0)
            {
              
              on.off.log$date.on <- paste(on.off.log$date.on.month,on.off.log$date.on.day,on.off.log$date.on.year,sep="/")
              on.off.log$time.on <- paste(on.off.log$time.on.hour,on.off.log$time.on.minute,on.off.log$time.on.seconds,sep=":")
              
              on.off.log$date.off <- paste(on.off.log$date.off.month,on.off.log$date.off.day,on.off.log$date.off.year,sep="/")
              on.off.log$time.off <- paste(on.off.log$time.off.hour,on.off.log$time.off.minute,on.off.log$time.off.seconds,sep=":")
              
              on.off.log$date.time.on <- paste(on.off.log$date.on, on.off.log$time.on, sep=" ")
              on.off.log$date.time.off <- paste(on.off.log$date.off, on.off.log$time.off, sep=" ")
              
              on.off.log$date.time.on <- strptime(on.off.log$date.time.on,"%m/%d/%Y %H:%M:%S")
              on.off.log$date.time.off <- strptime(on.off.log$date.time.off,"%m/%d/%Y %H:%M:%S")
              
              on.off.log$hours.on <- as.vector(difftime(strptime(on.off.log$date.time.off,format="%Y-%m-%d %H:%M:%S"),strptime(on.off.log$date.time.on,format="%Y-%m-%d %H:%M:%S"), units="hours"))
              
              #	if on/off times recorded - loop through and label time monitor is not worn
              
              for (t in (1:dim(on.off.log)[1]))
              {
                on <- strptime(on.off.log$date.time.on[t],"%Y-%m-%d %H:%M:%S")
                off <- strptime(on.off.log$date.time.off[t],"%Y-%m-%d %H:%M:%S")
                n <- dim(temp)[1]
                inds <- (1:n)[((temp$time>=on)&(temp$time<=off))]
                if (length(inds)>0)
                  temp$off[inds] <- 0
              }
              
              inds.time.worn <- (1:(dim(temp)[1]))[temp$off==0]
              i <- length(inds.time.worn)
              if(i==0)
                temp$off <- "AP and on.off.log do not match"
              
            }	
            
            if(dim(on.off.log)[1]==0)
              temp$off <- "Subject/Visit not in on.off.log"		
          }	#end on/off loop
          
          if(length(on.off.log.temp)==0)
            temp$off <- "No.On.Off.Log"		
          
          temp$counter <- 1
          
          ## add intensity column to file
          
          d <- dim(temp)[1]
          
          temp$intensity <- "light"
          inds.sed <- (1:d)[temp$ap.posture==0]
          temp$intensity[inds.sed] <- "sedentary"
          inds.mvpa <- (1:d)[temp$mets>=3]
          temp$intensity[inds.mvpa] <- "mvpa"
          
          #	get step count - cumulative steps reported in file so 	need to figure out total steps/day
          
          d <- dim(temp)[1]
          
          steps.inds <- c((1:d)[temp$date[-1]!=temp$date[-d]],d)
          steps.1 <- temp[steps.inds,]
          steps.2 <- as.vector(steps.1$steps)
          d.2 <- length(steps.2)
          steps.3 <- data.frame(date=temp$date[steps.inds],steps=c(steps.2[1],steps.2[-1]-steps.2[-d.2]))
          
          ####		
          
          if(is.numeric(temp$in.bed)==T&is.numeric(temp$off)==T)
          {
            inds.time.worn <- (1:(dim(temp)[1]))[temp$off==0]
            inds.first.time.worn <- inds.time.worn[1]
            first.time.worn <- temp$time[inds.first.time.worn]
            
            inds.last.time.worn <- inds.time.worn[i]
            last.time.worn <- temp$time[inds.last.time.worn]
            
            only.measurement.period <- temp[temp$time>=first.time.worn&temp$time<=last.time.worn,]
            dim(only.measurement.period)
            only.measurement.period.awake <- only.measurement.period[only.measurement.period$in.bed==0,]
            dim(only.measurement.period.awake)
            
            sleep.wake.wear.table <- data.frame(study=study,sub=as.numeric(s),visit=v,date=unique(only.measurement.period$date),
                                                awake.hours=tapply(only.measurement.period$in.bed==0, only.measurement.period$date,sum)/3600,
                                                total.sleep.hours=tapply(only.measurement.period$in.bed==1, only.measurement.period$date,sum)/3600,
                                                total.wear.hours=tapply(only.measurement.period$off==0, only.measurement.period$date,sum)/3600,
                                                non.wear.hours=tapply(only.measurement.period$off==1, only.measurement.period$date,sum)/3600,
                                                hours.awake.worn=(tapply(only.measurement.period$in.bed==0&only.measurement.period$off==0,only.measurement.period$date,sum))/3600,
                                                hours.awake.not.worn=(tapply(only.measurement.period$in.bed==0&only.measurement.period$off==1,only.measurement.period$date,sum))/3600,
                                                hours.sleep.worn=(tapply(only.measurement.period$in.bed==1&only.measurement.period$off==0,only.measurement.period$date,sum))/3600,
                                                hours.sleep.not.worn=(tapply(only.measurement.period$in.bed==1&only.measurement.period$off==1,only.measurement.period$date,sum))/3600
            )		
            
            # make file with bed and off time cleaned out
            data <- temp[temp$in.bed==0&temp$off==0,]
            
            
            if(dim(data)[1]>1)
            {
              #		head(data)	
              
              #	get step count - cumulative steps reported in file so 		need to figure out total steps/day
              
              date <- unique(data$date)
              dd <- dim(steps.3)[1]
              
              inds <- vector(length=0)
              
              for(i in (1:length(date)))
              {
                inds <- c(inds,(1:dd)[date[i]==steps.3$date])
              }
              
              steps <- steps.3$steps[inds]
              
              # make .csv file with PA and SB variables per day
              
              results.table <- data.frame(study=study,sub=as.numeric(s),visit=v,date=unique(data$date),
                                          
                                          hours.awake.worn = tapply(data$off==0,data$date,sum)/3600,
                                          
                                          met.hours = tapply(data$met.hours,data$date,sum),	step.count = steps,
                                          
                                          sed.mins = tapply(data$ap.posture,data$date,sed.min.AP),
                                          stand.mins = tapply(data$ap.posture,data$date,stand.min.AP),
                                          step.mins = tapply(data$ap.posture,data$date,step.min.AP),
                                          
                                          lit.mins = tapply(data$intensity=="light",data$date,sum)/60,
                                          mvpa.mins = tapply(data$mets,data$date,mvpa.min.AP),
                                          
                                          breaks = tapply(data$ap.posture,data$date,breaks.AP),
                                          break.rate = tapply(data$ap.posture,data$date,breaks.AP)/((tapply(data$ap.posture,data$date,sed.min.AP))/60),
                                          
                                          guideline.minutes = tapply(data$one.min.mets,data$date,guideline.bouts.min),
                                          num.guideline.bouts = tapply(data$one.min.mets,data$date,guideline.bouts.num),
                                          
                                          min.in.sed.30 = tapply(data$ap.posture,data$date,prolonged.sed.bouts.min,n=30),
                                          min.in.sed.60 = tapply(data$ap.posture,data$date,prolonged.sed.bouts.min,n=60),
                                          
                                          num.bouts.in.sed.30 = tapply(data$ap.posture,data$date,prolonged.sed.bouts.num,n=30),
                                          num.bouts.in.sed.60 = tapply(data$ap.posture,data$date,prolonged.sed.bouts.num,n=60))
              
            }
            
            if(dim(data)[1]==0)
            {
              
              results.table <- data.frame(study=study,sub=as.numeric(s),visit=v,date="No Valid Wear Hours.  Check that AP file matches with on.off.log and bed.log",
                                          
                                          hours.awake.worn = NA,
                                          
                                          met.hours = NA,
                                          step.count = NA,
                                          
                                          sed.mins = NA,
                                          stand.mins = NA,
                                          step.mins = NA,
                                          
                                          lit.mins = NA,
                                          mvpa.mins = NA,
                                          
                                          breaks = NA,
                                          break.rate = NA,
                                          
                                          guideline.minutes = NA,
                                          num.guideline.bouts = NA,
                                          
                                          min.in.sed.30 = NA,
                                          min.in.sed.60 = NA,
                                          
                                          num.bouts.in.sed.30 = NA,
                                          num.bouts.in.sed.60 = NA)
              
            }
          }
          
          ####
          
          if(is.numeric(temp$in.bed)==F&is.numeric(temp$off)==T)
          {
            inds.time.worn <- (1:(dim(temp)[1]))[temp$off==0]
            i <- length(inds.time.worn)
            inds.first.time.worn <- inds.time.worn[1]
            first.time.worn <- temp$time[inds.first.time.worn]
            
            inds.last.time.worn <- inds.time.worn[i]
            last.time.worn <- temp$time[inds.last.time.worn]
            
            only.measurement.period <- temp[temp$time>=first.time.worn&temp$time<=last.time.worn,]
            dim(only.measurement.period)
            
            sleep.wake.wear.table <- data.frame(study=study,sub=as.numeric(s),visit=v,date=unique(only.measurement.period$date),
                                                awake.hours="No valid bed.log data",
                                                sleep.hours="No valid bed.log data",
                                                total.wear.hours=tapply(only.measurement.period$off==0, only.measurement.period$date,sum)/3600,
                                                total.non.wear.hours=tapply(only.measurement.period$off==1, only.measurement.period$date,sum)/3600,
                                                hours.awake.worn="No valid bed.log data",
                                                hours.awake.not.worn="No valid bed.log data",
                                                hours.sleep.worn="No valid bed.log data",
                                                hours.sleep.not.worn="No valid bed.log data"
            )		
            
            # make file with bed and off time cleaned out
            data <- temp[temp$off==0,]
            
            if(dim(data)[1]>1)
            {
              #		head(data)	
              
              #	get step count - cumulative steps reported in file so 		need to figure out total steps/day
              
              date <- unique(data$date)
              dd <- dim(steps.3)[1]
              
              inds <- vector(length=0)
              
              for(i in (1:length(date)))
              {
                inds <- c(inds,(1:dd)[date[i]==steps.3$date])
              }
              
              steps <- steps.3$steps[inds]
              
              
              
              # make .csv file with PA and SB variables per day
              
              results.table <- data.frame(study=study,sub=as.numeric(s),visit=v,date=unique(data$date),
                                          
                                          hours.awake.worn = tapply(data$off==0,data$date,sum)/3600,
                                          
                                          met.hours = tapply(data$met.hours,data$date,sum),	step.count = steps,
                                          
                                          sed.mins = tapply(data$ap.posture,data$date,sed.min.AP),
                                          stand.mins = tapply(data$ap.posture,data$date,stand.min.AP),
                                          step.mins = tapply(data$ap.posture,data$date,step.min.AP),
                                          
                                          lit.mins = tapply(data$intensity=="light",data$date,sum)/60,
                                          mvpa.mins = tapply(data$mets,data$date,mvpa.min.AP),
                                          
                                          breaks = tapply(data$ap.posture,data$date,breaks.AP),
                                          break.rate = tapply(data$ap.posture,data$date,breaks.AP)/((tapply(data$ap.posture,data$date,sed.min.AP))/60),
                                          
                                          guideline.minutes = tapply(data$one.min.mets,data$date,guideline.bouts.min),
                                          num.guideline.bouts = tapply(data$one.min.mets,data$date,guideline.bouts.num),
                                          
                                          min.in.sed.30 = tapply(data$ap.posture,data$date,prolonged.sed.bouts.min,n=30),
                                          min.in.sed.60 = tapply(data$ap.posture,data$date,prolonged.sed.bouts.min,n=60),
                                          
                                          num.bouts.in.sed.30 = tapply(data$ap.posture,data$date,prolonged.sed.bouts.num,n=30),
                                          num.bouts.in.sed.60 = tapply(data$ap.posture,data$date,prolonged.sed.bouts.num,n=60))
              
            }
            
            if(dim(data)[1]==0)
            {
              
              results.table <- data.frame(study=study,sub=as.numeric(s),visit=v,date="No Valid Wear Hours.  Check that AP file matches with on.off.log and bed.log",
                                          
                                          hours.awake.worn = NA,
                                          
                                          met.hours = NA,
                                          step.count = NA,
                                          
                                          sed.mins = NA,
                                          stand.mins = NA,
                                          step.mins = NA,
                                          
                                          lit.mins = NA,
                                          mvpa.mins = NA,
                                          
                                          breaks = NA,
                                          break.rate = NA,
                                          
                                          guideline.minutes = NA,
                                          num.guideline.bouts = NA,
                                          
                                          min.in.sed.30 = NA,
                                          min.in.sed.60 = NA,
                                          
                                          num.bouts.in.sed.30 = NA,
                                          num.bouts.in.sed.60 = NA)
              
            }
          }
          
          ####
          
          if(is.numeric(temp$in.bed)==T&is.numeric(temp$off)==F)
          {
            only.measurement.period.awake <- temp[temp$in.bed==0,]
            dim(only.measurement.period.awake)
            
            sleep.wake.wear.table <- data.frame(study=study,sub=as.numeric(s),visit=v,date=unique(only.measurement.period.awake$date),
                                                awake.hours=tapply(only.measurement.period.awake$in.bed==0, only.measurement.period.awake$date,sum)/3600,
                                                sleep.hours=tapply(only.measurement.period.awake$in.bed==1, only.measurement.period.awake$date,sum)/3600,
                                                total.wear.hours="No valid on.off.log data",
                                                total.non.wear.hours="No valid on.off.log data",
                                                hours.awake.worn="No valid on.off.log data",
                                                hours.awake.not.worn="No valid on.off.log data",
                                                hours.sleep.worn="No valid on.off.log data",
                                                hours.sleep.not.worn="No valid on.off.log data"
            )		
            
            # make file with bed and off time cleaned out
            data <- temp[temp$in.bed==0,]
            
            if(dim(data)[1]>1)
            {
              #		head(data)	
              
              #	get step count - cumulative steps reported in file so 		need to figure out total steps/day
              
              date <- unique(data$date)
              dd <- dim(steps.3)[1]
              
              inds <- vector(length=0)
              
              for(i in (1:length(date)))
              {
                inds <- c(inds,(1:dd)[date[i]==steps.3$date])
              }
              
              steps <- steps.3$steps[inds]
              
              # make .csv file with PA and SB variables per day
              
              results.table <- data.frame(study=study,sub=as.numeric(s),visit=v,date=unique(data$date),
                                          
                                          hours.awake.worn = tapply(data$off==0,data$date,sum)/3600,
                                          
                                          met.hours = tapply(data$met.hours,data$date,sum),	step.count = steps,
                                          
                                          sed.mins = tapply(data$ap.posture,data$date,sed.min.AP),
                                          stand.mins = tapply(data$ap.posture,data$date,stand.min.AP),
                                          step.mins = tapply(data$ap.posture,data$date,step.min.AP),
                                          
                                          lit.mins = tapply(data$intensity=="light",data$date,sum)/60,
                                          mvpa.mins = tapply(data$mets,data$date,mvpa.min.AP),
                                          
                                          breaks = tapply(data$ap.posture,data$date,breaks.AP),
                                          break.rate = tapply(data$ap.posture,data$date,breaks.AP)/((tapply(data$ap.posture,data$date,sed.min.AP))/60),
                                          
                                          guideline.minutes = tapply(data$one.min.mets,data$date,guideline.bouts.min),
                                          num.guideline.bouts = tapply(data$one.min.mets,data$date,guideline.bouts.num),
                                          
                                          min.in.sed.30 = tapply(data$ap.posture,data$date,prolonged.sed.bouts.min,n=30),
                                          min.in.sed.60 = tapply(data$ap.posture,data$date,prolonged.sed.bouts.min,n=60),
                                          
                                          num.bouts.in.sed.30 = tapply(data$ap.posture,data$date,prolonged.sed.bouts.num,n=30),
                                          num.bouts.in.sed.60 = tapply(data$ap.posture,data$date,prolonged.sed.bouts.num,n=60))
              
            }
            
            if(dim(data)[1]==0)
            {
              
              results.table <- data.frame(study=study,sub=as.numeric(s),visit=v,date="No Valid Wear Hours.  Check that AP file matches with on.off.log and bed.log",
                                          
                                          hours.awake.worn = NA,
                                          
                                          met.hours = NA,
                                          step.count = NA,
                                          
                                          sed.mins = NA,
                                          stand.mins = NA,
                                          step.mins = NA,
                                          
                                          lit.mins = NA,
                                          mvpa.mins = NA,
                                          
                                          breaks = NA,
                                          break.rate = NA,
                                          
                                          guideline.minutes = NA,
                                          num.guideline.bouts = NA,
                                          
                                          min.in.sed.30 = NA,
                                          min.in.sed.60 = NA,
                                          
                                          num.bouts.in.sed.30 = NA,
                                          num.bouts.in.sed.60 = NA)
              
            }
          }		
          
          ####
          
          if(is.numeric(temp$in.bed)==F&is.numeric(temp$off)==F)
          {
            
            sleep.wake.wear.table <- data.frame(study=study,sub=as.numeric(s),visit=v,date=unique(temp$date),
                                                awake.hours="No valid bed.log or on.off.log data",
                                                sleep.hours="No valid bed.log or on.off.log data",
                                                total.wear.hours="No valid bed.log or on.off.log data",
                                                total.non.wear.hours="No valid bed.log or on.off.log data",
                                                hours.awake.worn="No valid bed.log or on.off.log data",
                                                hours.awake.not.worn="No valid bed.log or on.off.log data",
                                                hours.sleep.worn="No valid bed.log or on.off.log data",
                                                hours.sleep.not.worn="No valid bed.log or on.off.log data"
            )				
            
            # make file with bed and off time cleaned out
            data <- temp
            if(dim(data)[1]>1)
            {
              #		head(data)	
              
              #	get step count - cumulative steps reported in file so 		need to figure out total steps/day
              
              date <- unique(data$date)
              dd <- dim(steps.3)[1]
              
              inds <- vector(length=0)
              
              for(i in (1:length(date)))
              {
                inds <- c(inds,(1:dd)[date[i]==steps.3$date])
              }
              
              steps <- steps.3$steps[inds]
              
              # make .csv file with PA and SB variables per day
              
              results.table <- data.frame(study=study,sub=as.numeric(s),visit=v,date=unique(data$date),
                                          
                                          hours.awake.worn = tapply(data$off==0,data$date,sum)/3600,
                                          
                                          met.hours = tapply(data$met.hours,data$date,sum),	step.count = steps,
                                          
                                          sed.mins = tapply(data$ap.posture,data$date,sed.min.AP),
                                          stand.mins = tapply(data$ap.posture,data$date,stand.min.AP),
                                          step.mins = tapply(data$ap.posture,data$date,step.min.AP),
                                          
                                          lit.mins = tapply(data$intensity=="light",data$date,sum)/60,
                                          mvpa.mins = tapply(data$mets,data$date,mvpa.min.AP),
                                          
                                          breaks = tapply(data$ap.posture,data$date,breaks.AP),
                                          break.rate = tapply(data$ap.posture,data$date,breaks.AP)/((tapply(data$ap.posture,data$date,sed.min.AP))/60),
                                          
                                          guideline.minutes = tapply(data$one.min.mets,data$date,guideline.bouts.min),
                                          num.guideline.bouts = tapply(data$one.min.mets,data$date,guideline.bouts.num),
                                          
                                          min.in.sed.30 = tapply(data$ap.posture,data$date,prolonged.sed.bouts.min,n=30),
                                          min.in.sed.60 = tapply(data$ap.posture,data$date,prolonged.sed.bouts.min,n=60),
                                          
                                          num.bouts.in.sed.30 = tapply(data$ap.posture,data$date,prolonged.sed.bouts.num,n=30),
                                          num.bouts.in.sed.60 = tapply(data$ap.posture,data$date,prolonged.sed.bouts.num,n=60))
              
            }
          }
          
          results.table$percent.of.hours.awake.worn.sed <- results.table$sed.mins/(results.table$hours.awake.worn*60)
          results.table$percent.of.hours.awake.worn.lit <- results.table$lit.mins/(results.table$hours.awake.worn*60)
          results.table$percent.of.hours.awake.worn.mvpa <- results.table$mvpa.mins/(results.table$hours.awake.worn*60)	
          
          rt <- dim(results.table)[1]
          inds.sed <- results.table$percent.of.hours.awake.worn.sed
          inds.inf.sed <- (1:rt)[inds.sed=="Inf"]
          results.table$percent.of.hours.awake.worn.sed[inds.inf.sed] <- NA
          
          inds.lit <- results.table$percent.of.hours.awake.worn.lit
          inds.inf.lit <- (1:rt)[inds.lit=="Inf"]
          results.table$percent.of.hours.awake.worn.lit[inds.inf.lit] <- NA
          
          inds.mvpa <- results.table$percent.of.hours.awake.worn.mvpa
          inds.inf.mvpa <- (1:rt)[inds.mvpa=="Inf"]
          results.table$percent.of.hours.awake.worn.mvpa[inds.inf.mvpa] <- NA
          
          means.table <- data.frame(study=study,sub=as.numeric(s),visit=v,
                                    
                                    hours.awake.worn = mean(results.table$hours.awake.worn,na.rm=T),
                                    sd.hours.awake.worn = sd(results.table$hours.awake.worn,na.rm=T),
                                    low.hours.awake.worn = mean(results.table$hours.awake.worn,na.rm=T)-1.96*(sd(results.table$hours.awake.worn,na.rm=T)/(sqrt(dim(results.table)[1]))),
                                    up.hours.awake.worn = mean(results.table$hours.awake.worn,na.rm=T)+1.96*(sd(results.table$hours.awake.worn,na.rm=T)/(sqrt(dim(results.table)[1]))),
                                    
                                    met.hours = mean(results.table$met.hours,na.rm=T),	
                                    sd.met.hours = sd(results.table$met.hours,na.rm=T),	
                                    low.met.hours = mean(results.table$met.hours,na.rm=T)-1.96*(sd(results.table$met.hours,na.rm=T)/(sqrt(dim(results.table)[1]))),
                                    up.met.hours = mean(results.table$met.hours,na.rm=T)+1.96*(sd(results.table$met.hours,na.rm=T)/(sqrt(dim(results.table)[1]))),
                                    
                                    step.count = mean(results.table$step.count,na.rm=T),
                                    sd.step.count = sd(results.table$step.count,na.rm=T),
                                    low.step.count = mean(results.table$step.count,na.rm=T)-1.96*(sd(results.table$step.count,na.rm=T)/(sqrt(dim(results.table)[1]))),
                                    up.step.count = mean(results.table$step.count,na.rm=T)+1.96*(sd(results.table$step.count,na.rm=T)/(sqrt(dim(results.table)[1]))),
                                    
                                    sed.mins = mean(results.table$sed.mins,na.rm=T),
                                    sd.sed.mins = sd(results.table$sed.mins,na.rm=T),
                                    low.sed.mins = mean(results.table$sed.mins,na.rm=T)-1.96*(sd(results.table$sed.mins,na.rm=T)/(sqrt(dim(results.table)[1]))),
                                    up.sed.mins = mean(results.table$sed.mins,na.rm=T)+1.96*(sd(results.table$sed.mins,na.rm=T)/(sqrt(dim(results.table)[1]))),
                                    
                                    stand.mins = mean(results.table$stand.mins,na.rm=T),
                                    sd.stand.mins = sd(results.table$stand.mins,na.rm=T),
                                    low.stand.mins = mean(results.table$stand.mins,na.rm=T)-1.96*(sd(results.table$stand.mins,na.rm=T)/(sqrt(dim(results.table)[1]))),
                                    up.stand.mins = mean(results.table$stand.mins,na.rm=T)+1.96*(sd(results.table$stand.mins,na.rm=T)/(sqrt(dim(results.table)[1]))),
                                    
                                    step.mins = mean(results.table$step.mins,na.rm=T),
                                    sd.step.mins = sd(results.table$step.mins,na.rm=T),
                                    low.step.mins = mean(results.table$step.mins,na.rm=T)-1.96*(sd(results.table$step.mins,na.rm=T)/(sqrt(dim(results.table)[1]))),
                                    up.step.mins = mean(results.table$step.mins,na.rm=T)+1.96*(sd(results.table$step.mins,na.rm=T)/(sqrt(dim(results.table)[1]))),
                                    
                                    lit.mins = mean(results.table$lit.mins,na.rm=T),
                                    sd.lit.mins = sd(results.table$lit.mins,na.rm=T),
                                    low.lit.mins = mean(results.table$lit.mins,na.rm=T)-1.96*(sd(results.table$lit.mins,na.rm=T)/(sqrt(dim(results.table)[1]))),
                                    up.lit.mins = mean(results.table$lit.mins,na.rm=T)+1.96*(sd(results.table$lit.mins,na.rm=T)/(sqrt(dim(results.table)[1]))),
                                    
                                    mvpa.mins = mean(results.table$mvpa.mins,na.rm=T),
                                    sd.mvpa.mins = sd(results.table$mvpa.mins,na.rm=T),
                                    low.mvpa.mins = mean(results.table$mvpa.mins,na.rm=T)-1.96*(sd(results.table$mvpa.mins,na.rm=T)/(sqrt(dim(results.table)[1]))),
                                    up.mvpa.mins = mean(results.table$mvpa.mins,na.rm=T)+1.96*(sd(results.table$mvpa.mins,na.rm=T)/(sqrt(dim(results.table)[1]))),
                                    
                                    breaks = mean(results.table$breaks,na.rm=T),
                                    sd.breaks = sd(results.table$breaks,na.rm=T),
                                    low.breaks = mean(results.table$breaks,na.rm=T)-1.96*(sd(results.table$breaks,na.rm=T)/(sqrt(dim(results.table)[1]))),
                                    up.breaks = mean(results.table$breaks,na.rm=T)+1.96*(sd(results.table$breaks,na.rm=T)/(sqrt(dim(results.table)[1]))),
                                    
                                    break.rate = mean(results.table$break.rate,na.rm=T),
                                    sd.break.rate = sd(results.table$break.rate,na.rm=T),
                                    low.break.rate = mean(results.table$break.rate,na.rm=T)-1.96*(sd(results.table$break.rate,na.rm=T)/(sqrt(dim(results.table)[1]))),
                                    up.break.rate = mean(results.table$break.rate,na.rm=T)+1.96*(sd(results.table$break.rate,na.rm=T)/(sqrt(dim(results.table)[1]))),
                                    
                                    guideline.minutes = mean(results.table$guideline.minutes,na.rm=T),
                                    sd.guideline.minutes = sd(results.table$guideline.minutes,na.rm=T),
                                    low.guideline.minutes = mean(results.table$guideline.minutes,na.rm=T)-1.96*(sd(results.table$guideline.minutes,na.rm=T)/(sqrt(dim(results.table)[1]))),
                                    up.guideline.minutes = mean(results.table$guideline.minutes,na.rm=T)+1.96*(sd(results.table$guideline.minutes,na.rm=T)/(sqrt(dim(results.table)[1]))),
                                    
                                    num.guideline.bouts = mean(results.table$num.guideline.bouts,na.rm=T),
                                    sd.num.guideline.bouts = sd(results.table$num.guideline.bouts,na.rm=T),
                                    low.num.guideline.bouts = mean(results.table$num.guideline.bouts,na.rm=T)-1.96*(sd(results.table$num.guideline.bouts,na.rm=T)/(sqrt(dim(results.table)[1]))),
                                    up.num.guideline.bouts = mean(results.table$num.guideline.bouts,na.rm=T)+1.96*(sd(results.table$num.guideline.bouts,na.rm=T)/(sqrt(dim(results.table)[1]))),
                                    
                                    min.in.sed.30 = mean(results.table$min.in.sed.30,na.rm=T),
                                    sd.min.in.sed.30 = sd(results.table$min.in.sed.30,na.rm=T),
                                    low.min.in.sed.30 = mean(results.table$min.in.sed.30,na.rm=T)-1.96*(sd(results.table$min.in.sed.30,na.rm=T)/(sqrt(dim(results.table)[1]))),
                                    up.min.in.sed.30 = mean(results.table$min.in.sed.30,na.rm=T)+1.96*(sd(results.table$min.in.sed.30,na.rm=T)/(sqrt(dim(results.table)[1]))),
                                    
                                    min.in.sed.60 = mean(results.table$min.in.sed.60,na.rm=T),
                                    sd.min.in.sed.60 = sd(results.table$min.in.sed.60,na.rm=T),
                                    low.min.in.sed.60 = mean(results.table$min.in.sed.60,na.rm=T)-1.96*(sd(results.table$min.in.sed.60,na.rm=T)/(sqrt(dim(results.table)[1]))),
                                    up.min.in.sed.60 = mean(results.table$min.in.sed.60,na.rm=T)+1.96*(sd(results.table$min.in.sed.60,na.rm=T)/(sqrt(dim(results.table)[1]))),
                                    
                                    num.bouts.in.sed.30 = mean(results.table$num.bouts.in.sed.30,na.rm=T),
                                    sd.num.bouts.in.sed.30 = sd(results.table$num.bouts.in.sed.30,na.rm=T),
                                    low.num.bouts.in.sed.30 = mean(results.table$num.bouts.in.sed.30,na.rm=T)-1.96*(sd(results.table$num.bouts.in.sed.30,na.rm=T)/(sqrt(dim(results.table)[1]))),
                                    up.num.bouts.in.sed.30 = mean(results.table$num.bouts.in.sed.30,na.rm=T)+1.96*(sd(results.table$num.bouts.in.sed.30,na.rm=T)/(sqrt(dim(results.table)[1]))),
                                    
                                    num.bouts.in.sed.60 = mean(results.table$num.bouts.in.sed.60,na.rm=T),
                                    sd.num.bouts.in.sed.60 = sd(results.table$num.bouts.in.sed.60,na.rm=T),
                                    low.num.bouts.in.sed.60 = mean(results.table$num.bouts.in.sed.60,na.rm=T)-1.96*(sd(results.table$num.bouts.in.sed.60,na.rm=T)/(sqrt(dim(results.table)[1]))),
                                    up.num.bouts.in.sed.60 = mean(results.table$num.bouts.in.sed.60,na.rm=T)+1.96*(sd(results.table$num.bouts.in.sed.60,na.rm=T)/(sqrt(dim(results.table)[1]))),
                                    
                                    percent.of.hours.awake.worn.sed = mean(results.table$percent.of.hours.awake.worn.sed,na.rm=T),
                                    sd.percent.of.hours.awake.worn.sed = sd(results.table$percent.of.hours.awake.worn.sed,na.rm=T),
                                    low.percent.of.hours.awake.worn.sed = mean(results.table$percent.of.hours.awake.worn.sed,na.rm=T)-1.96*(sd(results.table$percent.of.hours.awake.worn.sed,na.rm=T)/(sqrt(dim(results.table)[1]))),
                                    up.percent.of.hours.awake.worn.sed = mean(results.table$percent.of.hours.awake.worn.sed,na.rm=T)+1.96*(sd(results.table$percent.of.hours.awake.worn.sed,na.rm=T)/(sqrt(dim(results.table)[1]))),
                                    
                                    percent.of.hours.awake.worn.lit = mean(results.table$percent.of.hours.awake.worn.lit,na.rm=T),
                                    sd.percent.of.hours.awake.worn.lit = sd(results.table$percent.of.hours.awake.worn.lit,na.rm=T),
                                    low.percent.of.hours.awake.worn.lit = mean(results.table$percent.of.hours.awake.worn.lit,na.rm=T)-1.96*(sd(results.table$percent.of.hours.awake.worn.lit,na.rm=T)/(sqrt(dim(results.table)[1]))),
                                    up.percent.of.hours.awake.worn.lit = mean(results.table$percent.of.hours.awake.worn.lit,na.rm=T)+1.96*(sd(results.table$percent.of.hours.awake.worn.lit,na.rm=T)/(sqrt(dim(results.table)[1]))),
                                    
                                    percent.of.hours.awake.worn.mvpa = mean(results.table$percent.of.hours.awake.worn.mvpa,na.rm=T),
                                    sd.percent.of.hours.awake.worn.mvpa = sd(results.table$percent.of.hours.awake.worn.mvpa,na.rm=T),
                                    low.percent.of.hours.awake.worn.mvpa = mean(results.table$percent.of.hours.awake.worn.mvpa,na.rm=T)-1.96*(sd(results.table$percent.of.hours.awake.worn.mvpa,na.rm=T)/(sqrt(dim(results.table)[1]))),
                                    up.percent.of.hours.awake.worn.mvpa = mean(results.table$percent.of.hours.awake.worn.mvpa,na.rm=T)+1.96*(sd(results.table$percent.of.hours.awake.worn.mvpa,na.rm=T)/(sqrt(dim(results.table)[1])))
                                    
          )
          
          if (counter==1)
            write.table(results.table,file=paste(directory,"results.table.csv",sep=""),sep=",",row.names=F,col.names=T,append=F)
          
          if (counter==1)
            
            write.table(sleep.wake.wear.table,file=paste(directory,"sleep.wake.wear.table.csv",sep=""),sep=",",row.names=F,col.names=T,append=F)
          
          if (counter==1)
            write.table(means.table,file=paste(directory,"means.table.csv",sep=""),sep=",",row.names=F,col.names=T,append=F)
          
          
          if (counter>1)
            write.table(results.table,file=paste(directory,"results.table.csv",sep=""),sep=",",row.names=F,col.names=F,append=T)
          
          if (counter>1)
            write.table(sleep.wake.wear.table,file=paste(directory,"sleep.wake.wear.table.csv",sep=""),sep=",",row.names=F,col.names=F,append=T)
          
          if (counter>1)
            write.table(means.table,file=paste(directory,"means.table.csv",sep=""),sep=",",row.names=F,col.names=F,append=T)
          
          
          
          counter <- counter+1
          
        }
      }
    }
    
  }

prolonged.sed.bouts.min <-
  function(posture,epoch=1,n) {	
    acts <- posture == 0
    lengths.of.continuous.bouts.of.sed <- apply(as.matrix(strsplit(paste(acts, collapse=""), split="FALSE", fixed=TRUE)[[1]]), 1, function(x) {nchar(x)/4})
    lengths <- lengths.of.continuous.bouts.of.sed > n*(60/epoch)
    return(sum(lengths.of.continuous.bouts.of.sed[lengths])/(60/epoch))
  }

prolonged.sed.bouts.num <-
  function(posture,epoch=1,n) {
    acts <- posture == 0
    lengths.of.continuous.bouts.of.sed <- apply(as.matrix(strsplit(paste(acts, collapse=""), split="FALSE", fixed=TRUE)[[1]]), 1, function(x) {nchar(x)/4})
    inds.lengths <- (1:length(lengths.of.continuous.bouts.of.sed))[lengths.of.continuous.bouts.of.sed>n*(60/epoch)]
    return(length(inds.lengths))
  }

second.by.second <-
  function(data)
  {
    sec.by.sec.data <- data.frame(time=NA, date=NA, ap.posture=NA, mets=NA, met.hours=NA, steps=NA)
    sec.by.sec.data <- sec.by.sec.data[-1,]
    
    data$interval <- as.numeric(data$interval)
    
    data$methrs <- as.numeric(data$methrs)
    
    n <- dim(data)[1]
    time.of.each.event <- as.vector(difftime(strptime(data$time[seq_len(n - 1) + 1],format="%Y-%m-%d %H:%M:%S"),strptime(data$time[seq_len(n - 1)],format="%Y-%m-%d %H:%M:%S"), units="secs"))
    start.time <- strptime(data$time[1],format="%Y-%m-%d %H:%M:%S")
    
    time.of.each.event <- c(time.of.each.event, round(data[n,"interval"],0))
    te <- length(time.of.each.event)
    time.of.each.event[is.na(time.of.each.event)==T] <- 1
    events <- rep((1:te),time.of.each.event)
    
    acts <- rep(data$activity,time.of.each.event)
    n <- length(acts)
    # The met hours per second in the interval.
    met.hours <- data$methrs/data$interval 	
    met.hours <- rep(met.hours,time.of.each.event)
    # To compute mets per second in the interval, multiply methours by 3600 sec/hour and divide by number of seconds.
    mets <- data$methrs * 3600/data$interval
    mets <- rep(mets,time.of.each.event)
    steps <- rep(data$cumulativesteps,time.of.each.event)
    # Make 15-sec epoch variable and METs
    times <- start.time+(0:(n-1))
    fifteen.sec.times <- start.time + (15*rep(0:(floor(n/15)),each=15,length=n))
    fifteen.sec.mets <- tapply(mets, fifteen.sec.times, mean)
    fifteen.sec.mets <- rep(fifteen.sec.mets, each=15, length=n)
    
    # Make 1-min epoch variable and METs
    times <- start.time+(0:(n-1))
    one.min.times <- start.time + (60*rep(0:(floor(n/60)),each=60,length=n))
    one.min.mets <- tapply(mets, one.min.times, mean)
    one.min.mets <- rep(one.min.mets, each=60, length=n)
    
    date <- substring(format(times),1,10)
    
    sec.by.sec.data <- merge(sec.by.sec.data, data.frame(time=times, date=date, ap.posture=acts, mets=mets, fifteen.sec.mets=fifteen.sec.mets, one.min.mets=one.min.mets, met.hours=met.hours, steps=steps, num.events=events, stringsAsFactors=FALSE), all=TRUE)
    
    sec.by.sec.data$mets <- signif(sec.by.sec.data$mets,3)
    
    return(sec.by.sec.data)
  }

sed.min.AP <-
  function(posture,epoch=1)
  {
    sed.mins <- sum((posture==0),na.rm=T)/(60/epoch)
    if (sed.mins==0)
      sed.mins <- NA
    return(as.numeric(sed.mins))
  }

stand.min.AP <-
  function(posture,epoch=1)
  {
    stand.mins <- sum((posture=="1"),na.rm=T)/(60/epoch)
    if (stand.mins==0)
      stand.mins <- NA
    return(stand.mins)
  }

step.min.AP <-
  function(posture,epoch=1)
  {
    step.mins <- sum((posture=="2"),na.rm=T)/(60/epoch)
    if (step.mins==0)
      step.mins <- NA
    return(step.mins)
  }

wake <-
  function(directory=directory,id,visit,name.of.log.bed,data)
  {
    bed.log <- read.csv(paste(directory,name.of.log.bed,".csv",sep=""))
    bed.log$id <- as.character(bed.log$id)
    bed.log <- bed.log[bed.log$id==id&bed.log$visit==visit,]
    
    
    bed.log$date.out <- paste(bed.log$date.out.month,bed.log$date.out.day,bed.log$date.out.year,sep="/")
    bed.log$time.out <- paste(bed.log$time.out.hour,bed.log$time.out.minute,bed.log$time.out.seconds,sep=":")
    
    bed.log$date.in <- paste(bed.log$date.in.month,bed.log$date.in.day,bed.log$date.in.year,sep="/")
    bed.log$time.in <- paste(bed.log$time.in.hour,bed.log$time.in.minute,bed.log$time.in.seconds,sep=":")
    
    bed.log$date.time.out <- paste(bed.log$date.out, bed.log$time.out, sep=" ")
    bed.log$date.time.in <- paste(bed.log$date.in, bed.log$time.in, sep=" ")
    
    bed.log$date.time.out <- strptime(bed.log$date.time.out,"%m/%d/%Y %H:%M:%S")
    bed.log$date.time.in <- strptime(bed.log$date.time.in,"%m/%d/%Y %H:%M:%S")
    
    bed.log$hours.up <- as.vector(difftime(strptime(bed.log$date.time.in,format="%Y-%m-%d %H:%M:%S"),strptime(bed.log$date.time.out,format="%Y-%m-%d %H:%M:%S"), units="hours"))
    
    #	if bed times recorded - loop through and label time in bed
    if(dim(bed.log)[1]>0)
    {
      data$in.bed <- 1
      data$day.for.wearer <- NA	
      bed.log$day.for.wearer <- 1:dim(bed.log)[1]
      #	t <- 1
      for (t in (1:dim(bed.log)[1]))
      {
        wake <- strptime(bed.log$date.time.out[t],"%Y-%m-%d %H:%M:%S")
        bed <- strptime(bed.log$date.time.in[t],"%Y-%m-%d %H:%M:%S")
        n <- dim(data)[1]
        inds <- (1:n)[((data$time>=wake)&(data$time<=bed))]
        
        if (length(inds)>0)
          data$in.bed[inds] <- 0
        data$day.for.wearer[inds] <- bed.log$day.for.wearer[t]
      }
      if(dim(bed.log)[1]==0)
        data$in.bed <- "No.Bed.Log"	
    }	#end bed loop
    
    return(data)
  }
