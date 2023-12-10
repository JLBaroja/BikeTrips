rm(list=ls())
bikes <- read.csv('metro-trips-2023-q3.csv') 
# Trip duration in minutes (original 'duration' column is
# flawed: it only runs up to 1440 secs even for trips that
# lasted days according to their start and end dates).
get_duration <- function(dtset){
	# Calculates trip duration in minutes.
	# Assumes dtset has columns 'start_time' and 'end_time',
	# both in a specific date format.
	start_time <- as.POSIXct(dtset$start_time,format='%m/%d/%Y %H:%M')
	end_time <- as.POSIXct(dtset$end_time,format='%m/%d/%Y %H:%M')
	duration_mins <- as.numeric(end_time - start_time)/60
	return(duration_mins)
}
bikes$trip_duration <- get_duration(bikes)
# Define start_time globally for use in next computations	
start_time <- as.POSIXct(bikes$start_time,format='%m/%d/%Y %H:%M')
# Day of the week of each trip
bikes$weekday <- weekdays.POSIXt(start_time)
# Interarrivals
interarrivals <- rep(NA,nrow(bikes))
interarrivals[2:length(interarrivals)] <- start_time[2:length(start_time)]-start_time[1:(length(start_time)-1)]
bikes$interarrivals <- interarrivals/60
bikes$interarrivals[1] <- as.numeric(format(start_time[1],'%H'))*60+as.numeric(format(start_time[1],'%M'))
# Calculating start times with respect to each day,
# as it makes sense to expect variations between weekends and
# working days, and also across times with respect to midnight.
# Iterate over unique days (in this quarter)
dates_quarter <- format(start_time,'%Y-%m-%d')
bikes$day_quarter <- NA
bikes$start_min_day <- NA
bikes$start_hour_day <- NA
for(qd in unique(dates_quarter)){
	day_indexes <- which(dates_quarter==qd)
	bikes$day_quarter[day_indexes] <- which(unique(dates_quarter)==qd)
	day_trips <- start_time[day_indexes] 
	# When did trips start relative to 00:00:00 of current day?
	#day_start <- as.POSIXct(paste(qd,'00:00:00'),format='%Y-%m-%d %H:%M:%S')
	bikes$start_min_day[day_indexes] <- as.numeric(format(day_trips,'%H'))*60+as.numeric(format(day_trips,'%M'))
	bikes$start_hour_day[day_indexes] <- as.numeric(format(day_trips,'%H'))+1
}

write.csv(bikes,'bikes.csv',row.names=F)
