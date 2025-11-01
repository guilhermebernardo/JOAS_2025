rm(list=ls())

library(miceadds)
library(dplyr)
library(stringr)
library(lubridate)
library(webshot2)
library(htmlwidgets)
library(stringr)
library(htmltools)
library(leaflet)
library(leaflegend)

your_directory <- "C:/Users/guilherme.bernardo/Desktop/JOAS"

setwd(paste0(your_directory, "/KPI17/dash_kpi17_micro"))

source('alongtrack_distance_dep.R')
source('verticalprofile_visualization_ve_variant2.R')
source('trajectory_visualization_ve_variant2.R')
source('get_iatafromicao.R')
source('get_lat.R')
source('get_lon.R')

setwd(your_directory)

load(paste0(your_directory,"/KPI17/dep_filtered.Rda"))
load(paste0(your_directory, "/KPI17/kpi17.Rda"))

track <- dep_filtered %>%
  filter(
    time >= as.POSIXct('2025-01-01', tz = "UTC") &
      time <  as.POSIXct('2025-01-02', tz = "UTC") &   # Date range for analysis
      plan_dep == 'CYYZ' # Airport desired for analysis
  )

kpis <- kpi17

kpis_filtered <- kpis %>%
  inner_join(track, by = "indicat")

track <- kpis %>%
  inner_join(kpis_filtered, by = "indicat")

m2 <- verticalprofile_visualization_ve_variant2(track, kpis_filtered)
m2

ggsave(paste0(your_directory,"/verticalprofiles_KPI17.png"), width = 1920*3, height = 1080*3, units = "px")
  
# Generates horizontal profile map
m <- trajectory_visualization_ve_variant2(track, kpis_filtered)
m

saveWidget(m, file = paste0(your_directory, "/horizontalprofile_KPI17", ".html"))
files <- list.files(your_directory, pattern = "html",full.names = T)

# Convert webshot to png files
for (i in files){
  chromote::set_chrome_args("--disable-crash-reporter")
  webshot2::webshot(i, file = gsub("html","png",i))
}

