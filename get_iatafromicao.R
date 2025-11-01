get_iatafromicao <- function(loc){
	# loc <- "SBGR"
	airports <- read.delim("airports_world.txt", sep ="\t")
	idx <- substring(airports$ICAO,2,5) == loc
	loc_iata <- substring(airports$IATA[idx],2,4)
	return(loc_iata)
}