rm(list=ls())

library(miceadds)
library(dplyr)
library(stringr)
library(lubridate)
library(webshot2)
library(htmlwidgets)
library(stringr)
library(leaflet)

your_directory <- "C:/Users/guilherme.bernardo/Desktop/JOAS"

setwd(paste0(your_directory, "/KPI19/dash_kpi19_micro"))

source('alongtrack_distance_arr.R')
source('verticalprofile_visualization_ve_variant2.R')
source('trajectory_visualization_ve_variant2.R')
source('get_iatafromicao.R')
source('get_lat.R')
source('get_lon.R')

setwd(your_directory)

load(paste0(your_directory,"/KPI19/arr_filtered.Rda"))
load(paste0(your_directory, "/KPI19/kpi19.Rda"))

track <- arr_filtered %>%
  filter(time >= as.POSIXct('2025-03-01') & 
           time < as.POSIXct('2025-04-01') & 
           real_arr == "CYVR")

kpis <- kpi19

kpis_filtered <- kpis %>%
  inner_join(track, by = "indicat")

track <- kpis %>%
  inner_join(kpis_filtered, by = "indicat")

m2 <- verticalprofile_visualization_ve_variant2(track, kpis_filtered)
m2

ggsave(paste0(your_directory,"/verticalprofiles_KPI19.png"), width = 1920*3, height = 1080*3, units = "px")

# Generates horizontal profile map
m <- trajectory_visualization_ve_variant2(track, kpis_filtered)
m

saveWidget(m, file = paste0(your_directory, "/horizontalprofile_KPI19", ".html"))
files <- list.files(your_directory, pattern = "html",full.names = T)

# Convert webshot to png files
for (i in files){
  chromote::set_chrome_args("--disable-crash-reporter")
  webshot2::webshot(i, file = gsub("html","png",i))
}

