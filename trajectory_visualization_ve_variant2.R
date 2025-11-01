trajectory_visualization_ve_variant2 <- function(track_data, kpi_data){
  
  library("leaflet")
  source('get_lat.R')
  source('get_lon.R')
  
  #upper_limit <- 20
  
  #track_data <- track_data[which(track_data$phase == 'DEP_40' | track_data$phase == 'DEP_100'),]
  
  track_data <- track_data[which(track_data$dist_dep <= 200 & substr(track_data$phase,1,3) != 'GND'),]
  
  #pal <- colorNumeric(palette = 'Spectral', seq(0,upper_limit,0.05), reverse=TRUE)
  
  
  kpi_bins_number <- 5
  kpi_bins_label <- c('[0,5)', '[5,10)', '[10,15)', '[15,20)', '[20,Inf)')
  kpi_bins_lower <- c(0, 5, 10, 15, 20)
  kpi_bins_upper <- c(5, 10, 15, 20, Inf)
  
  
  m <- leaflet()
  #m <- addTiles(m)
  m <- addProviderTiles(m, providers$CartoDB.Positron)
  for (i in unique(track_data$indicat)){
    idx <- which(track_data$indicat==i)
    idx2 <- which(kpi_data$indicat==i)
    if (length(idx) > 0 & length(idx2) > 0){
      table_aux <- track_data[idx,]
      
      table_aux$level <- 0
      table_aux$level_segment <- 0
      for (j in 2:nrow(table_aux)){
        ROC <- 60*(table_aux$alt[j] - table_aux$alt[j-1])/(as.numeric(table_aux$time[j]) - as.numeric(table_aux$time[j-1]))  # rate of climb
        if (is.na(ROC) == 1){
          ROC <- 0
        }
        if (ROC >= 0 & ROC < 300){
          table_aux$level[j] <- 1
          if (table_aux$level[j] != table_aux$level[j-1]){
            table_aux$level_segment[j] <- max(table_aux$level_segment) + 1
          }
          if (table_aux$level[j] == table_aux$level[j-1]){
            table_aux$level_segment[j] <- table_aux$level_segment[j-1]
          }
        }
      }
      
      #txtlabel <- paste(table_aux$id_icao[1], paste0(round(kpi_data$kpi17_time[idx2],1),'min'), table_aux$indicat[1], table_aux$time[1], sep=': ')
      txtlabel <- paste(table_aux$id_icao[1], paste0(round(kpi_data$kpi17_time[idx2],1),' min'), sep=': ') 
      #txtlabel <- paste0(paste(table_aux$id_icao[1], paste0(round(kpi_data$kpi17_time[idx2],1),'min'), sep=': '), ' (', date(table_aux$time[1]), ')')
      #m <- addPolylines(m, lng = table_aux$lon, lat = table_aux$lat, color = pal(min(c(kpi_data$kpi17_time[idx2],upper_limit))), weight = 2, opacity = 0.3, label=txtlabel)
      #m <- addPolylines(m, lng = table_aux$lon, lat = table_aux$lat, color = 'blue', weight = 1, opacity = 0.2, label=txtlabel)
      
      for (k in 1:kpi_bins_number){
        
        if (kpi_data$kpi17_time[idx2[length(idx2)]] >= kpi_bins_lower[k] & kpi_data$kpi17_time[idx2[length(idx2)]] < kpi_bins_upper[k]){
          
          m <- addPolylines(m, lng = table_aux$lon, lat = table_aux$lat, color = 'blue', weight = 1, opacity = 0.2, label=txtlabel, group = kpi_bins_label[k])
          
          
          if (max(table_aux$level_segment) > 0){
            for (l in 1:max(table_aux$level_segment)){
              idx3 <- which(table_aux$level_segment==l)
              
              #if (table_aux$alt[idx3[length(idx3)]] <= 0.9*kpi_data$TOC[idx2[length(idx2)]] & substr(table_aux$phase[idx3[length(idx3)]],1,3) != 'ARR'){
              if (table_aux$alt[idx3[length(idx3)]] < kpi_data$TOC[idx2[length(idx2)]] & substr(table_aux$phase[idx3[length(idx3)]],1,3) != 'ARR'){
                
                if (as.numeric(table_aux$time[idx3[length(idx3)]]) - as.numeric(table_aux$time[idx3[1]]) >= 20){        # level segments > 20 s
                  m <- addPolylines(m, lng = table_aux$lon[table_aux$level_segment==l], lat = table_aux$lat[table_aux$level_segment==l], color = 'black', weight = 1, opacity = 1, label=txtlabel, group = kpi_bins_label[k])
                }
                
              }
            }
          }
        }
      }
    }
  }
  
  #m <- addLegend(m, "bottomright", pal = pal, values = seq(0,upper_limit,0.05), title = "KPI 17 (NM)", opacity = 1)
  
  #m <- addLegend(m, "bottomright", colors = c('blue', 'orange'), labels = c('> 300','<= 300'), title = "ROC (ft/min)", opacity = 1)
  #m <- addLegend(m, "bottomright", colors = c('orange', 'blue'), labels = c('Yes','No'), title = "Level-off", opacity = 1)
  
  
  m <- addCircles(m, lng=get_lon(get_iatafromicao(as.character(track_data$plan_dep[1]))), lat=get_lat(get_iatafromicao(as.character(track_data$plan_dep[1]))), color='black', weight = 2.5, radius=370400, fill = FALSE, stroke=TRUE, fillOpacity = 0.7, dashArray = 5)
  
  #m <- addCircles(m, lng=get_lon(get_iatafromicao(as.character(track_data$real_arr[1]))), lat=get_lat(get_iatafromicao(as.character(track_data$real_arr[1]))), color='black', weight = 2.5, radius=185200, fill = FALSE, stroke=TRUE, fillOpacity = 0.7, dashArray = 5)
  
  m <- addLayersControl(m, position="topleft", baseGroups = c("KPI 17 (min):"), overlayGroups = c(kpi_bins_label), options=layersControlOptions(collapsed=TRUE))
  
  return(m)
  
}


