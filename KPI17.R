

KPI17 <- function(track_data, radius = NULL, vs_limit = NULL, level_band = NULL, min_level_time = NULL, box = NULL,
                  min_alt = NULL, max_time = NULL, airport = NULL, fir = NULL, voo = NULL){
  
  library(data.table)
  library(geosphere)
  library(airportr)
  
  
  
  if(missing(radius)) {
    radiusx <- 200
  } else {
    radiusx <- radius
  }
  
  if(missing(vs_limit)) {
    vs_limitx <- 300/60     # 5 ft/s == 500 ft/min
  } else {
    vs_limitx <- vs_limit/60
  }
  
  if(missing(level_band)) {
    level_bandx <- 300
  } else {
    level_bandx <- level_band
  }
  
  
  if(missing(min_level_time)) {
    min_level_timex <- 20
  } else {
    min_level_timex <- min_level_time
  }
  
  
  if(missing(box)) {
    boxx <- 0.9
  } else {
    boxx <- box
  }
  
  if(missing(min_alt)) {
    min_altx <- 3000
  } else {
    min_altx <- min_alt
  }
  
  if(missing(max_time)) {
    max_timex <- 60*5
  } else {
    max_timex <- 60*max_time
  }
  
  
  
  if(missing(airport)) {
    track_data <- track_data
  } else {
    track_data <- track_data %>% filter(plan_dep %in% c(airport))
  }
  
  
  if(missing(fir)) {
    track_data <- track_data
  } else {
    track_data <- track_data %>% filter(fir %in% c(fir))
  }
  
  
  if(missing(voo)) {
    track_data <- track_data
  } else {
    track_data <- track_data %>% filter(id_icao %in% c(voo))
  }
  
  # load("/share/mineracao/BD_FR24_Positions_new2/2023/10/2023-10-02.Rda")
  # track_data <- df
  #  radiusx <- 200
  #  vs_limitx <- 300/60     # 5 ft/s == 500 ft/min
  #  level_bandx <- 300
  #  min_level_timex <- 20
  #  boxx <- 0.9
  #  min_altx <- 1800
  #  max_timex <- 60*2
  
  airports_elev <- airportr::airports%>%
    dplyr::select(ICAO, Altitude)%>%
    rename(plan_dep = ICAO)
  
  track_data <- track_data%>%
    mutate(alt2 = round(alt, digits = -2))
  
  # exclusion box --------------------------------------------------------------------------------------
  
  
  exclusion_box1 <- track_data%>%
    subset(dist_dep < radiusx)%>%                   # raio de análise
    subset(alt > 0)%>%
    group_by(indicat) %>%
    mutate(timex = time)%>%
    mutate(time = as.numeric(time))%>%
    dplyr::arrange(time, .by_group = TRUE)%>% 
    mutate(altmax =  max(alt2, na.rm = T))%>%
    mutate(rn = row_number())
  
  exclusion_box1 <- exclusion_box1%>%
    mutate(altlimit1 = max(alt2, na.rm = T))%>%    
    mutate(altlimit2 = altlimit1*boxx)%>%                            # box
    mutate(position = which.max(alt2))%>%
    slice(which((row_number() < position)))%>%
    subset(alt > altlimit2)
  
  if (nrow(exclusion_box1>0)){
    indicats <- exclusion_box1%>%
      mutate(leveltime = ifelse((abs((lead(alt)-alt)/(lead(time)-time))) < vs_limitx, lead(time)-time,0))%>%       # vertical speed limit
      #mutate(leveltime = ifelse(abs(lead(alt)-alt) < level_bandx, lead(time)-time,0))%>%   # vertical speed limit  # band
      mutate(leveltime = ifelse(is.na(leveltime),0,leveltime))%>%                                                 
      mutate(leveldistance = ifelse(leveltime > 0, distHaversine(cbind(lon, lat), cbind(lead(lon),lead(lat))),0))%>% #level distance
      mutate(leveldistance = ifelse(is.na(leveldistance),0, leveldistance))%>%
      mutate(flown_distance = distHaversine(cbind(lon, lat), cbind(lead(lon),lead(lat))))%>%                         # flown distance
      #filter(any(leveltime > 0))%>%
      mutate(tleveltime = sum(leveltime, na.rm = T))%>%
      mutate(cruise_flag = ifelse(tleveltime >  max_timex, 1, 0))%>%                                   # exclusion box 1
      subset(cruise_flag > 0)%>%
      group_by(indicat)%>%
      dplyr::summarise(altlimit2 = altlimit2[1])%>%
      mutate(box_flag = 1)
  }
  
  
  
  
  #-- KPI 17 ----------------------------------------------------------------------------
  
  
  vertical_eff <- track_data%>%
    subset(dist_dep < radiusx)%>%                   # raio de análise
    subset(alt > 0)%>%  
    
    merge(airports_elev, by = "plan_dep")%>%
    mutate(AGL = alt - Altitude)%>%
    subset(AGL > min_altx)%>%                        # altura minima de analise
    
    group_by(indicat) %>%
    mutate(timex = time)%>%
    mutate(time = as.numeric(time))%>%
    dplyr::arrange(time, .by_group = TRUE)%>% 
    
    left_join(indicats, by = "indicat") %>%
    mutate(maxalt = max(alt2, na.rm = T))%>%
    mutate(box_flag = ifelse(is.na(box_flag),0,box_flag))%>%
    mutate(altlimit = ifelse(box_flag == 1, altlimit2, maxalt))%>%
    mutate(position = which.max(alt2))%>%
    slice(which((row_number() < position)))%>%
    subset(alt < altlimit)%>%
    
    mutate(leveltime = ifelse((abs((lead(alt)-alt)/(lead(time)-time))) < vs_limitx, lead(time)-time,0))%>%       # vertical speed limit
    #mutate(leveltime = ifelse(abs(lead(alt)-alt) < level_bandx, lead(time)-time,0))%>%   # vertical speed limit  # band
    mutate(leveltime = ifelse(is.na(leveltime),0,leveltime))%>%
    
    mutate(leveldistance = ifelse(leveltime > 0, distHaversine(cbind(lon, lat), cbind(lead(lon),lead(lat))),0))%>%
    mutate(leveldistance = ifelse(is.na(leveldistance),0, leveldistance))%>%
    mutate(flown_distance = distHaversine(cbind(lon, lat), cbind(lead(lon),lead(lat))))%>%
    mutate(mean_alt_leveloffs = alt*leveltime)
  
  
  # get individual level segments -  min time ----------------------------------------------------
  
  data <- vertical_eff %>% 
    subset(leveltime > 0 | lag(leveltime) > 0)%>%
    mutate(indicat2 = 1)%>%
    group_by(indicat)%>%
    arrange(time, .by_group = T)
  
  for (i in 2:nrow(data)) {
    try({
      if (abs(data$alt[i]-data$alt[i-1]) >= level_bandx & data$indicat[i] == data$indicat[i-1]) {
        
        data$indicat2[i] <- data$indicat2[i-1]+1
      } 
      if (abs(data$alt[i]-data$alt[i-1]) < level_bandx & data$indicat[i] == data$indicat[i-1]) {
        
        data$indicat2[i] <- data$indicat2[i-1]
      }
    })
  }
  
  level_min_time <- data %>%
    group_by(indicat,indicat2)%>%
    mutate(segtime = sum(leveltime,na.rm = T))%>%
    subset(segtime < min_level_timex)%>%
    mutate(min_time_flag = 1)%>%
    ungroup()%>%
    dplyr::select(indicat,time,min_time_flag)
  
  level_segments <- data %>%
    group_by(indicat,indicat2)%>%
    mutate(segtime = sum(leveltime,na.rm = T))%>%
    subset(segtime > min_level_timex)%>%
    ungroup()%>%
    group_by(indicat)%>%
    mutate(level_seg_number = as.numeric(as.factor(indicat2)))
  #select(indicat,time,indicat2)
  
  number_level_seg <- level_segments %>%
    group_by(indicat)%>%
    summarise(level_seg_number = max(level_seg_number,na.rm = T))
  
  vertical_eff <-  vertical_eff %>%
    distinct(indicat,time, .keep_all = T)%>%
    left_join(level_min_time, by = c("indicat","time"))%>%
    left_join(number_level_seg, by = "indicat")%>%
    mutate(level_seg_number = ifelse(is.na(level_seg_number),0,level_seg_number))%>%
    mutate(min_time_flag = ifelse(is.na(min_time_flag),0,min_time_flag))%>%
    mutate(leveltime = ifelse(min_time_flag == 1, 0,leveltime))%>%
    mutate(leveldistance = ifelse(min_time_flag == 1, 0,leveldistance))
  
  
  speed <- vertical_eff %>%
    subset(leveltime > 0 | lag(leveltime) > 0)%>%
    group_by(indicat)%>%
    summarise(mean_speed = mean(speed, na.rm = T))
  
  
  vertical_eff <- vertical_eff %>%
    summarise(kpi17_distance = sum(leveldistance, na.rm = T),
              flown_distance = sum(flown_distance, na.rm = T),
              kpi17_time = sum(leveltime, na.rm = T),
              transit_time = last(time)-first(time),
              plan_dep = plan_dep[1],real_arr = real_arr[1],
              plan_arr = plan_arr[1],time = timex[1],id_iata = id_iata[1],
              id_icao = id_icao[1],reg = reg[1],
              mean_alt_leveloffs = sum(mean_alt_leveloffs,na.rm = T)/sum(leveltime,na.rm = T),
              number_level_seg = max(level_seg_number, na.rm = T),
              TOC = max(altlimit,na.rm = T)) %>%
    left_join(speed, by = "indicat")%>%
    mutate(kpi17_distance = 0.000539957*kpi17_distance, 
           flown_distance = flown_distance*0.000539957,
           kpi17_time      = kpi17_time/60,
           transit_time = transit_time/60,
           percentage_leveltime = 100*(kpi17_time/transit_time),
           percentage_leveldistance = 100*(kpi17_distance/flown_distance)) %>%
    distinct() #%>%
  #mutate(across(.cols = everything(), ~ ifelse(is.infinite(.x), 0, .x)))  
  
  #vertical_eff[is.na(vertical_eff)] <- 0
  vertical_eff[mapply(is.infinite, vertical_eff)] <- 0
  
  
  kpi17 <- list(kpi = vertical_eff,level_segments = level_segments)
  
  return(kpi17)
}
