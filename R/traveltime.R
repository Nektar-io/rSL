#' Get travel time from current position
#' 
#' Get travel time (including walking distance) in minutes from current position to central places in Stockholm
#' 
#' @param coords A vector of WGS coordinates on [northing, easting] format for the original point of travel
#' @param destinations The destination station IDs we want to calculate travel times to. Default is 9001, which is T-Centralen. Other examples include 9192 (Slussen) and 9117 (Odenplan).
#' @param time A character string on the "HH:MM" format.
#' @param date A character string on the "DD.MM.YYYY" format.
#' @export

travelTimeFromPos <- function(
	WGS,
	destinations = c(9001),
	Time = "12:00",
	Date = strftime(today(), "%d.%m.%Y")
) {
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
				Time = Time,
				Date = Date
			))
		
		x <- GET(url)
		xdata <- xmlToList(xmlParse(x), simplify = TRUE)
		
		# Error handling: If the first element of xdata is not a list, an error
		# has been returned instead of travel data. However, we don't want the
		# function to crash so we assume the reason for this is that this is because
		# SID and Z are too close. Hence we return "0" instead.
		if (is.character(xdata[[1]])) {
			warning(
				"In rSL::travelTimeFromPos():\n",
				"An error was returned from the SL travel time API.\n",
				"Since the API does this when the address is too close to the destination,\n",
				"we simply assume that this means that the travel time is zero."
			)
			sumtime[[dest]] <- 0
			next()
		}
		
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
