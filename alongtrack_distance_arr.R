alongtrack_distance_arr <- function(track_data){
  
  # REQUIRE LIBRARY(PRACMA)
  library(pracma)
  
  track_data$track_dist_arr <- 0
  
  flights <- unique(track_data$indicat)
  
  for (i in flights){
    aux <- track_data[which(track_data$indicat==i),]
    aux <- aux[order(nrow(aux):1),]
    
    D <- numeric(nrow(aux))
    D[1] <- 0
    for (j in 2:length(aux$indicat)){
      D[j] <- D[j-1] + 0.539957*haversine(c(aux$lat[j],aux$lon[j]),c(aux$lat[j-1],aux$lon[j-1]))
    }
    
    track_data$track_dist_arr[which(track_data$indicat==i)] <- rev(D)
  }
  
  return(track_data)
}

