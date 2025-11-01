
verticalprofile_visualization_ve <- function(track_data, kpi_data){

  upper_limit <- 50
  
  #source('alongtrack_distance_arr.R')
  
  #track_data <- df[which(df$real_arr == 'SBGR' & (df$phase == 'ARR_100' | df$phase == 'ARR_40')),]
  
  track_data <- track_data[which(track_data$phase == 'ARR_100' | track_data$phase == 'ARR_40'),]
  
  #track_data <- track_data[which(track_data$dist_arr <= 200),]
  
  track_data <- alongtrack_distance_arr(track_data)
  
  track_data$kpi19_distance <- 0
  track_data <- track_data[which(track_data$indicat %in% kpi_data$indicat == TRUE),]
  flights <- unique(kpi_data$indicat)
  
  for (i in flights){
    track_data$kpi19_distance[track_data$indicat == i] <- kpi_data$kpi19_distance[kpi_data$indicat == i]
  }
  
  library(ggplot2)
  
  #track_data <- track_data[track_data$kpi19_distance <= upper_limit,]
  track_data$kpi19_distance[track_data$kpi19_distance > upper_limit] <- upper_limit
  chart <- ggplot(data=track_data, aes(x=track_dist_arr, y=alt, group=indicat, color=kpi19_distance)) +
    geom_line(alpha=0.3) +
    scale_colour_distiller(palette='Spectral', limits=c(0,upper_limit), name = 'KPI 19 (NM)') +
    xlab('Distance flown (NM)') +
    ylab('Altitude (ft)') +
    xlim(0, 200)
  
  
  
  
  #library(viridisLite)
  #cols <- viridis(3)
  #cols <- substr(cols, 0, 7)
  
  #pal <- colorNumeric(palette = 'Spectral', domain=c(0,50))
  
  #chart_vertical <- highchart() %>%
  #  hc_add_series(track_data, "line", hcaes(x = track_dist_arr, y = alt, group = indicat, color = kpi19_distance)) %>%
  #  hc_colors(cols) %>% 
  #  hc_yAxis(title=list(text='Altitude (ft)'), reversedStacks=TRUE) %>% 
  #  hc_xAxis(title=list(text='Distance (NM)')) %>% 
  #hc_tooltip(pointFormat="<span style='color:{point.color}'>●</span> {series.name}: <b>{point.number:,.0f}</b> ({point.percent:,.1f}%)<br>", shared=TRUE) %>% 
  #  hc_legend(enabled=FALSE)
  
  
  
  # PLOT TIME SERIES HORIZONTAL EFFICIENY
  
  #chart_timeseries_hte_dep <- highchart() %>%
  #  hc_add_series(dataplot_timeseries_hte, "line", hcaes(x = as.character(datetime2), y = median_he)) %>%
  #  hc_colors("#000000") %>% 
  #  hc_yAxis(title=list(text='Median HE - departures'), min=0.75, max=1) %>% 
  #  hc_xAxis(title=list(text='Time (UTC)'), labels=list(format=" ")) %>% 
  #  hc_tooltip(pointFormat="<span style='color:{point.color}'>●</span> Median HE: <b>{point.median_he:,.3f}</b>") %>% 
  #  hc_legend(enabled=FALSE)
  
  
  return(chart)

}




