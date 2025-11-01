get_lat <- function(loc){
	# loc <- "GRU"
	airports <- read.delim("C:/Users/guilherme.bernardo/Desktop/DASC 2025/KPI19/dash_kpi19_micro/airports_brazil.txt", sep ="\t")
	idx <- substring(airports$IATA,2,4) == loc
	lat <- airports$lat[idx]
	return(lat)
}