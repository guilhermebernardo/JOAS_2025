get_iatafromicao <- function(loc){
	# loc <- "SBGR"
	airports <- read.delim("C:/Users/guilherme.bernardo/Desktop/DASC 2025/KPI17/dash_kpi17_micro/airports_world.txt", sep ="\t")
	idx <- substring(airports$ICAO,2,5) == loc
	loc_iata <- substring(airports$IATA[idx],2,4)
	return(loc_iata)
}