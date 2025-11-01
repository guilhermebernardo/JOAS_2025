rm(list = ls())
gc()

# If you don't have these libraries, install first with install.packages("library_name")
library(dplyr)
library(lubridate)
library(reticulate)

# We are going to use a function in Python through reticulate, so it is import to set up your python directory correctly
use_python("C:/Users/guilherme.bernardo/AppData/Local/R/cache/R/reticulate/uv/cache/archive-v0/LrshW5ZKYEFRq63asrdqm/Scripts/python.exe", required = TRUE)

your_directory <- "C:/Users/guilherme.bernardo/Desktop/JOAS"

airports <- c("CYYZ", "CYVR", "CYUL", "CYYC", "CYWG", "CYOW", "CYTZ", "CYYJ", "CYQT", "CYHU", "CYEG", "CYQB")

setwd(your_directory)

source_python(paste0(your_directory, "/collect_OSN.py"))

# Set up a date time range to collect flight data from OSN
start_date <- as.Date("YYYY-MM-DD")
end_date <- as.Date("YYYY-MM-DD")

all_dates <- seq(start_date, end_date, by = "day")

for (i in seq_along(all_dates)) {
  
  print(all_dates[i])
  
  year <- format(all_dates[i], "%Y")
  month <- format(all_dates[i], "%m")
  date_str <- format(all_dates[i], "%Y-%m-%d")
  
  dir_path <- file.path(year, month)
  
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
  
  # This is going to collect trajectories from flights arriving and/or departuring from these airports
  # Feel free to change for any airport in the world
  raw_df <- collect_osn_data(as.character(all_dates[i]), airports)
  
  save_path <- file.path(dir_path, paste0(date_str, ".Rda"))
  save(raw_df, file = save_path)
    
}
