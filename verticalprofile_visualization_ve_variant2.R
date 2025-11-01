
verticalprofile_visualization_ve_variant2 <- function(track_data, kpi_data){

  upper_limit <- 20
  
  #source('alongtrack_distance_arr.R')
  
  #track_data <- df[which(df$real_arr == 'SBGR' & (df$phase == 'ARR_100' | df$phase == 'ARR_40')),]
  
  track_data <- track_data[which(track_data$phase == 'ARR_100' | track_data$phase == 'ARR_40'),]
  
  #track_data <- track_data[which(track_data$dist_arr <= 200),]
  
  track_data <- alongtrack_distance_arr(track_data)
  
  track_data$kpi19_time <- 0
  track_data <- track_data[which(track_data$indicat %in% kpi_data$indicat == TRUE),]
  flights <- unique(kpi_data$indicat)
  
  for (i in flights){
    track_data$kpi19_time[track_data$indicat == i] <- kpi_data$kpi19_time[kpi_data$indicat == i]
  }
  
  library(ggplot2)
  
  #track_data <- track_data[track_data$kpi19_time <= upper_limit,]
  track_data$kpi19_time[track_data$kpi19_time > upper_limit] <- upper_limit
  chart <- ggplot(data=track_data, aes(x=track_dist_arr, y=alt, group=indicat, color=kpi19_time)) +
    geom_line(alpha=1) +
    scale_colour_distiller(palette='Spectral', limits=c(0,upper_limit), name = 'KPI (min)') +
    xlab('Distance flown (NM)') +
    ylab('Altitude (ft)') +
    xlim(0, 200) +
    theme_minimal(base_size = 16) +  # Use a clean white background
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid.major = element_line(color = "gray90"),
      panel.grid.minor = element_line(color = "gray95"),
      legend.text = element_text(size = 26),
      legend.title = element_text(size = 38),
      axis.title = element_text(size = 38),
      axis.text = element_text(size = 34)
    )
  
  
  
  return(chart)

}




