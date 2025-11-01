get_lat <- function(loc){
	# loc <- "GRU"
	airports <- read.delim("C:/Users/guilherme.bernardo/Desktop/DASC 2025/KPI17/dash_kpi17_micro/airports_world.txt", sep ="\t")
	idx <- substring(airports$IATA,2,4) == loc
	lat <- airports$lat[idx]
	return(lat)
}