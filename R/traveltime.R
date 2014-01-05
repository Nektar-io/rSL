#' Get travel time from current position
#' 
#' Get travel time (including walking distance) in minutes from current position to central places in Stockholm
#' 
#' @param coords A vector of WGS coordinates on [northing, easting] format for the original point of travel
#' @param destinations The destination station IDs we want to calculate travel times to. Default is 9001, which is T-Centralen. Other examples include 9192 (Slussen) and 9117 (Odenplan).
#' @export

travelTimeFromPos <- function(WGS, destinations=c(9001)) {
	# Get point of origin in an API friendly format
	SID <- paste0("@Y=",format(WGS*10^6, digits=8)[1],"@X=",format(WGS*10^6, digits=8)[2])
	
	# Define time sums
	sumtime <- list()
	
	for (dest in 1:length(destinations)) {
		# Create call
		url <- OpenSth:::build_url(
			url = .reseplanerareUrl,
			path = .reseplanerarePath,
			params = list(
				key = .reseplanerareKey,
				SID = SID,
				Z = destinations[dest],
				Time = "12:00"
			))
		
		x <- GET(url)
		xdata <- xmlToList(xmlParse(x), simplify = TRUE)
		
		sumtime[[dest]] <- 0
		for (i in 3:7) {
			dur <- xdata[[i]]$Summary$Duration
			
			# Get number of minutes
			time <- suppressMessages(as.integer(as.duration(hm(dur))))/60
			
			# If the trip starts with a walk, add those minutes
			if (xdata[[i]][2]$SubTrip$Transport$Type == "Walk") {
				time <- time + as.integer(xdata[[i]][2]$SubTrip$Duration)
			}
			
			sumtime[[dest]] <- sumtime[[dest]] + time
		}
	}
	
	# Convert the list of times to a vector and divide by the number of trips
	times <- sapply(sumtime, function(i) i) / 5
	names(times) <- destinations
	
	# Return the average trip duration (in minutes)
	return(times)
}
