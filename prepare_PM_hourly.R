#!/usr/bin/env Rscript

# This script uses a template script from Allan Just to prepare daily and hourly PM measurements from EPA data
# The script then finds stations that have both daily and hourly data and plots statistics and maps for 2018
# This combines previous code in import_PM_monitors.R and primary_PM_monitors.R

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Libraries                                         ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

suppressMessages(library(data.table))
suppressMessages(library(fst))
library(sf)
# devtools::install_github("rushgeo/nngeo") # use for parallel calculation with unprojected coords
library(nngeo)
library(ggplot2)
fig_dir = '/Users/yshiga/Documents/Research/MtSinai/Figures/'
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Open Measurement Data. Hourly, then daily                         ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Hourly
# to unzip the files, we have to temporarily change working directory (calling out to unzip on linux, got weird)
storewd <- getwd()
# change working directory to where the data live, open as data.table
filepath <- "~/Documents/Research/MtSinai/Data/aqs_raw/hourly" # "data/aqs_raw/epa_daily"
setwd(filepath)
zipfiles <- list.files(pattern = "hourly.*\\.zip")
# load 2018 data only 
hourlydt <- rbindlist(lapply(zipfiles[4], function(x) fread(paste0("unzip -cq ", x)))) 


# restore the working directory
setwd(storewd)
rm(zipfiles, filepath)

# how big is the data
paste("The loaded hourly PM measurement data table has dimensions", paste(dim(hourlydt), collapse = ",")) # 9009658,29 for 1998-2017
# pryr::object_size(dailydt) #  1.8 GB

hourlydt[, day := as.Date(`Date Local`)]

# look at 2018 all sites in lower 48
states_non_contig <- c("Alaska", "Hawaii","Puerto Rico")

hourlydt <- hourlydt[!(`State Name` %in% states_non_contig) & day > as.Date("2017-12-31") & day < as.Date("2019-01-01"), ]
paste("PM Hourly measurements table subset to 2018 has dimensions", paste(dim(hourlydt), collapse = ",")) # 1243560,30


# Daily
# to unzip the files, we have to temporarily change working directory (calling out to unzip on linux, got weird)
storewd <- getwd()
# change working directory to where the data live, open as data.table
filepath <- "~/Documents/Research/MtSinai/Data/aqs_raw/hourly" # "data/aqs_raw/epa_daily"
setwd(filepath)
zipfiles <- list.files(pattern = "daily.*\\.zip")
dailydt <- rbindlist(lapply(zipfiles[4], function(x) fread(paste0("unzip -cq ", x))))

# restore the working directory
setwd(storewd)
rm(zipfiles, filepath)

# how big is the data
paste("The loaded daily PM measurement data table has dimensions", paste(dim(dailydt), collapse = ",")) 
# pryr::object_size(dailydt) #  1.8 GB

# clean the day
dailydt[, day := as.Date(`Date Local`)]

# look at 2018 all sites
dailydt <- dailydt[!(`State Name` %in% states_non_contig) & day > as.Date("2017-12-31") & day < as.Date("2019-01-01"), ]
paste("PM Daily measurements table subset to 2018 has dimensions", paste(dim(dailydt), collapse = ",")) # 1243560,30

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Variable construction. Hourly, then daily                        ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Hourly
# create a unique station identifier with uniform length
hourlydt[, stn := paste0(
  stringr::str_pad(`State Code`, 2, "left", pad = "0"), # two digit for state
  stringr::str_pad(`County Code`, 3, "left", pad = "0"),# three digits for county
  stringr::str_pad(`Site Num`, 4, "left", pad = "0"))]  # four digits site

# rename the main parameter
setnames(hourlydt, "Sample Measurement", "pm25")

# calculate daily std, range and mean
hourlydt[, daily_std := sd(pm25), by = .(stn, day)]
hourlydt[, daily_range := diff(range(pm25)), by = .(stn, day)]
hourlydt[, daily_mean := mean(pm25), by = .(stn, day)]
hourlydt[, daily_max := max(pm25), by = .(stn, day)]

# Open site description file
sites <- fread("~/Documents/Research/MtSinai/Data/monitors/epa_monitor_descriptions/aqs_sites.csv")

# In site table, create a unique station identifier with uniform length
sites[, stn := paste0(
  stringr::str_pad(`State Code`, 2, "left", pad = "0"), # two digit for state
  stringr::str_pad(`County Code`, 3, "left", pad = "0"),# three digits for county
  stringr::str_pad(`Site Number`, 4, "left", pad = "0"))]  # four digits site

# Join Land Use and Location Setting fields to the daily measurements
setkey(hourlydt, stn)
setkey(sites, stn)
hourlydt[sites, c("Land_Use", "Location_Setting") := .(`Land Use`, `Location Setting`)]
rm(sites)

# Daily
# create a unique station identifier with uniform length
dailydt[, stn := paste0(
  stringr::str_pad(`State Code`, 2, "left", pad = "0"), # two digit for state
  stringr::str_pad(`County Code`, 3, "left", pad = "0"),# three digits for county
  stringr::str_pad(`Site Num`, 4, "left", pad = "0"))]  # four digits site

# rename the main parameter
setnames(dailydt, "Arithmetic Mean", "pm25")

# Open site description file
sites <- fread("~/Documents/Research/MtSinai/Data/monitors/epa_monitor_descriptions/aqs_sites.csv")

# In site table, create a unique station identifier with uniform length
sites[, stn := paste0(
  stringr::str_pad(`State Code`, 2, "left", pad = "0"), # two digit for state
  stringr::str_pad(`County Code`, 3, "left", pad = "0"),# three digits for county
  stringr::str_pad(`Site Number`, 4, "left", pad = "0"))]  # four digits site

# Join Land Use and Location Setting fields to the daily measurements
setkey(dailydt, stn)
setkey(sites, stn)
dailydt[sites, c("Land_Use", "Location_Setting") := .(`Land Use`, `Location Setting`)]
rm(sites)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Selecting a single measurement per station & day  ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# often have >1 obs per station per day
# we want to get to one (valid) observation per station per day
# dailydt[, .N, by = c("stn", "day")][, table(N)]
# three reasons: 
# 1. redundant records with different sample duration (1 HOUR and 24 HR BLK)
# 2. event type flagged rows coded redundantly
# 3. multiple instruments (POC)

# 1: restrict to 24 hour measures (the 1 HOUR measures are for a different analysis)
dailydt <- dailydt[`Sample Duration` %in% "24 HOUR",]
#dailydt <- dailydt[`Sample Duration` %in% c("24 HOUR", "24-HR BLK AVG"),]
paste("PM measurements table subset to 24 hour samples has dimensions", paste(dim(dailydt), collapse = ",")) # 907098,33

# 2: create a field of whether there was an included and or excluded event type for every day
setkey(dailydt, stn, day, POC, `Event Type`)
# takes ~ 54 seconds
dailydt[unique(dailydt[`Event Type` != "None", .(stn, day, POC)]), 
        eventflag := .SD[`Event Type` != "None", paste0(`Event Type`, collapse = ",")], 
        by = c("stn", "day", "POC")]
# dailydt[, table(eventflag)]    # Excluded,Included: 21;  Included: 6769
# drop the data if it was a weird Event Type (duplicates)
dailydt <- dailydt[`Event Type` == "None", ]
paste("PM measurements table subset Event Type == None has dimensions", paste(dim(dailydt), collapse = ",")) # 900673,34


# 3: Select preferred measurement when multiple instruments are reporting that day

# Load the EPA monitor description file, which includes a flag for NAAQS Primary Monitor
monitors <- fread("~/Documents/Research/MtSinai/Data/monitors/epa_monitor_descriptions/aqs_monitors.csv")
# dim(monitors) # 349505, 30

# In monitor table, create a unique station identifier with uniform length
monitors[, stn := paste0(
  stringr::str_pad(`State Code`, 2, "left", pad = "0"), # two digit for state
  stringr::str_pad(`County Code`, 3, "left", pad = "0"),# three digits for county
  stringr::str_pad(`Site Number`, 4, "left", pad = "0"))]  # four digits site

# Subset monitors to NEMIA, dates since 2000, PM measurements
# monitors <- monitors[`State Name` %in% nemiastates & `Parameter Code` %in% c(88101, 88502) & 
#                     `Last Sample Date` >= as.Date("2000-01-01"), ]
monitors <- monitors[`Parameter Code` %in% c(88101, 88502) & 
                       `Last Sample Date` >= as.Date("2017-12-31"), ]

# Note that 44% of these stations do not have a primary monitor assigned
# prop.table(table(monitors[, .(has_primary = any(`NAAQS Primary Monitor` == "Y")), by = .(stn)][,has_primary]))

# Join the primary monitor flag to the daily measurements
setkey(dailydt, stn, `Parameter Code`, POC)
setkey(monitors, stn, `Parameter Code`, POC)
dailydt[monitors, NAAQS_Primary := `NAAQS Primary Monitor`]

# While we have the monitor table handy, copy the Monitoring Objective column
dailydt[monitors, Monitoring_Objective := `Monitoring Objective`]

# 3a: select the measurement from a Primary Monitor when it is available
dailydt[, day_has_primary := any(NAAQS_Primary == "Y"), by = .(stn, day)]
dailybest <- dailydt[`NAAQS_Primary` == "Y", ]

# 3b: when a Primary Monitor is not available, prefer measurements in the following priority:
#  Parameter Code 88101 over 88502, Sample Duration of "24 HOUR" over "24 HOUR AVG", and the lowest number of POC
setkey(dailydt, stn, day, `Parameter Code`, `Sample Duration`, POC)
dailybest_3b <- dailydt[unique(dailydt[day_has_primary == FALSE, .(stn, day)]), ,mult = "first"]

# combine the two steps to form the best daily measurement table
dailybest = rbind(dailybest, dailybest_3b)
paste("PM measurements table after selecting best daily measurement has dimensions", paste(dim(dailybest), collapse = ",")) # 702571,37
rm(dailybest_3b, dailydt)

# Note, a station 401359021 outside of NEMIA had two monitors marked as primary that reported at the same time, and the above filters would not remove such duplicates

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Fixing Inconsistent Datums. Hourly, then Daily      ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# general function for converting CRS
toEPSG <- function(x,y, from_epsg, to_epsg){ 
  fromPoint = st_sfc(st_point(c(x, y)), crs = from_epsg)
  toPoint = st_transform(fromPoint, crs = to_epsg)
  return(toPoint[[1]])}

# Hourly

# Transform coordinates to WGS84 where the Datum is known
uniquelonglatdatum <- unique(hourlydt[, .(Longitude, Latitude, Datum)])
uniquelonglatdatum[Datum == "NAD27", datum_epsg := 4267]
uniquelonglatdatum[Datum == "NAD83", datum_epsg := 4269]
uniquelonglatdatum[Datum == "WGS84", datum_epsg := 4326]
uniquelonglatdatum[Datum %in% c("NAD27", "NAD83"), 
                   c("long_wgs84", "lat_wgs84"):= as.data.table(t(mapply(Longitude, Latitude, 
                                                                         from_epsg = datum_epsg, to_epsg = 4326, FUN = toEPSG)))]
# Merge these unique transformed coords back to measurement table
setkey(hourlydt, Longitude, Latitude, Datum)
setkey(uniquelonglatdatum, Longitude, Latitude, Datum)
hourlydt[uniquelonglatdatum, c("long_wgs84", "lat_wgs84") := list(long_wgs84, lat_wgs84)]
# if WGS84 was original Datum, copy the coords to the _wgs84 columns
hourlydt[Datum == "WGS84", c("long_wgs84", "lat_wgs84") := list(Longitude, Latitude)]
rm(uniquelonglatdatum)

# Number of stations with UNKNOWN Datums (9)
# dailybest[Datum == "UNKNOWN", uniqueN(stn)] 
# How many of these stations sometimes did have a known datum? (0)
# dailybest[stn %in% dailybest[Datum == "UNKNOWN", unique(stn)] & Datum != "UNKNOWN" ]
# In previous testing, there were some stations outside NEMIA that had both UNKNOWN and known records, 
#  and for 29/30 of those stations, the UNKNOWNs were actually NAD27

# Assume UNKNOWN datums are WGS84. Assuming being the error is low if datum is incorrect
hourlydt[Datum == "UNKNOWN", c("long_wgs84", "lat_wgs84") := list(Longitude, Latitude)]

# Daily

# Transform coordinates to WGS84 where the Datum is known
uniquelonglatdatum <- unique(dailybest[, .(Longitude, Latitude, Datum)])
uniquelonglatdatum[Datum == "NAD27", datum_epsg := 4267]
uniquelonglatdatum[Datum == "NAD83", datum_epsg := 4269]
uniquelonglatdatum[Datum == "WGS84", datum_epsg := 4326]
uniquelonglatdatum[Datum %in% c("NAD27", "NAD83"), 
                   c("long_wgs84", "lat_wgs84"):= as.data.table(t(mapply(Longitude, Latitude, 
                                                                         from_epsg = datum_epsg, to_epsg = 4326, FUN = toEPSG)))]
# Merge these unique transformed coords back to measurement table
setkey(dailybest, Longitude, Latitude, Datum)
setkey(uniquelonglatdatum, Longitude, Latitude, Datum)
dailybest[uniquelonglatdatum, c("long_wgs84", "lat_wgs84") := list(long_wgs84, lat_wgs84)]
# if WGS84 was original Datum, copy the coords to the _wgs84 columns
dailybest[Datum == "WGS84", c("long_wgs84", "lat_wgs84") := list(Longitude, Latitude)]
rm(uniquelonglatdatum)

# Number of stations with UNKNOWN Datums (9)
# dailybest[Datum == "UNKNOWN", uniqueN(stn)] 
# How many of these stations sometimes did have a known datum? (0)
# dailybest[stn %in% dailybest[Datum == "UNKNOWN", unique(stn)] & Datum != "UNKNOWN" ]
# In previous testing, there were some stations outside NEMIA that had both UNKNOWN and known records, 
#  and for 29/30 of those stations, the UNKNOWNs were actually NAD27

# Assume UNKNOWN datums are WGS84. Assuming being the error is low if datum is incorrect
dailybest[Datum == "UNKNOWN", c("long_wgs84", "lat_wgs84") := list(Longitude, Latitude)]


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Output                                            ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


output_file = "/Users/yshiga/Documents/Research/MtSinai/Data/monitors/epa_daily/dailybest_2018.fst"
write_fst(dailybest, output_file, compress = 100)
paste("Wrote output file:", output_file)
paste0("Final table had dimensions ", paste0(dim(dailybest), collapse = ","),
       ", included dates from ", min(dailybest$day), " to ", max(dailybest$day), 
       ", and included ", dailybest[,uniqueN(stn)], " unique stations.")




output_file = "/Users/yshiga/Documents/Research/MtSinai/Data/monitors/epa_daily/hourlydt_2018.fst"
write_fst(hourlydt, output_file, compress = 100)
paste("Wrote output file:", output_file)
paste0("Final table had dimensions ", paste0(dim(hourlydt), collapse = ","),
       ", included dates from ", min(hourlydt$day), " to ", max(hourlydt$day), 
       ", and included ", hourlydt[,uniqueN(stn)], " unique stations.")



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Filtering - no zero values - only 24 hour
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# add number of obs per station
hourlydt[, N_stn := .N, by = stn] # n obs per station
dailybest[, N_stn := .N, by = stn] # n obs per station

# Mean and STD Total num. of non-NA obs. per station
hourlydt[, mean_pm25 := mean(pm25, na.rm =T), by = stn]
hourlydt[, std_pm25 := sd(pm25, na.rm =T), by = stn]
hourlydt[, mean_daily_std_pm25 := mean(daily_std, na.rm =T), by = stn]
hourlydt[, max_daily_std_pm25 := max(daily_std, na.rm =T), by = stn]
hourlydt[, mean_daily_range_pm25 := mean(daily_range, na.rm =T), by = stn]
hourlydt[, max_daily_range_pm25 := max(daily_range, na.rm =T), by = stn]
hourlydt[, mean_max_diff_avg := mean(daily_max-daily_mean, na.rm =T), by = stn]
hourlydt[, mean_max_diff_max := max(daily_max-daily_mean, na.rm =T), by = stn]

dailybest[,mean_pm25_pos := mean(pm25[pm25>=0], na.rm=T), by = stn]
dailybest[,std_pm25_pos := sd(pm25[pm25>=0], na.rm=T), by = stn]

# without zeros (positive or pos)
hourlydt[,mean_pm25_pos := mean(pm25[pm25>=0], na.rm=T), by = stn]
hourlydt[,std_pm25_pos := sd(pm25[pm25>=0], na.rm=T), by = stn]

hourlydt[, mean_daily_std_pm25_pos := mean(daily_std[pm25>=0], na.rm =T), by = stn]
hourlydt[, max_daily_std_pm25_pos := max(daily_std[pm25>=0], na.rm =T), by = stn]
hourlydt[, mean_daily_range_pm25_pos := mean(daily_range[pm25>=0], na.rm =T), by = stn]
hourlydt[, max_daily_range_pm25_pos := max(daily_range[pm25>=0], na.rm =T), by = stn]

hourlydt[, mean_max_diff_avg_pos := mean(daily_max[pm25>=0]-daily_mean[pm25>=0], na.rm =T), by = stn]
hourlydt[, mean_max_diff_max_pos := max(daily_max[pm25>=0]-daily_mean[pm25>=0], na.rm =T), by = stn]

dailybest[, mean_pm25 := mean(pm25, na.rm =T), by = stn]
dailybest[, std_pm25 := sd(pm25, na.rm =T), by = stn]

# # only use 24 hour measures
# 
# dailybest[, mean_pm25_24 := mean(pm25[`Sample Duration` == "24 HOUR"], na.rm =T), by = stn]
# dailybest[, std_pm25_24 := sd(pm25[`Sample Duration` == "24 HOUR"], na.rm =T), by = stn]
# 
# hourlydt[, mean_pm25_24 := mean(pm25[day %in% dailybest$day[dailybest$`Sample Duration` == "24 HOUR"]], na.rm =T), by = stn]
# hourlydt[, std_pm25_24 := sd(pm25[day %in% dailybest$day[dailybest$`Sample Duration` == "24 HOUR"]], na.rm =T), by = stn]
# 
# # only use 24 hour measures and positive
# 
# dailybest[, mean_pm25_24_pos := mean(pm25[`Sample Duration` == "24 HOUR" & pm25>=0], na.rm =T), by = stn]
# dailybest[, std_pm25_24_pos := sd(pm25[`Sample Duration` == "24 HOUR" & pm25>=0], na.rm =T), by = stn]
# 
# 
# hourlydt[, mean_pm25_24_pos := mean(pm25[day %in% dailybest$day[dailybest$`Sample Duration` == "24 HOUR"] & pm25>=0], na.rm =T), by = stn]
# hourlydt[, std_pm25_24_pos := sd(pm25[day %in% dailybest$day[dailybest$`Sample Duration` == "24 HOUR"] & pm25>=0], na.rm =T), by = stn]


# unique by station id
# filter by positive values and 24 hour sample duration
DS_hourly <- unique(hourlydt[,.(Latitude, Longitude, mean_pm25, std_pm25, mean_pm25_pos, std_pm25_pos,mean_daily_std_pm25_pos,mean_daily_range_pm25_pos,max_daily_std_pm25_pos,max_daily_range_pm25_pos , mean_max_diff_avg_pos,mean_max_diff_max_pos,stn , N_stn,Location_Setting, `State Name`)])
DS_daily <- unique(dailybest[,.(Latitude, Longitude, mean_pm25, std_pm25, mean_pm25_pos, std_pm25_pos,  stn,N_stn,Location_Setting,`State Name`)])
#DS_daily <- DS_daily[DS_daily$stn %in% DS_hourly$stn  & !is.nan(DS_daily$mean_pm25_24_pos)]
#DS_hourly <- DS_hourly[DS_hourly$stn %in% DS_daily$stn[!is.nan(DS_daily$mean_pm25_24_pos)]]
DS_daily <- DS_daily[DS_daily$stn %in% DS_hourly$stn]
DS_hourly <- DS_hourly[DS_hourly$stn %in% DS_daily$stn]


ggplot() + 
  geom_point(aes(x=DS_hourly$mean_pm25_pos,y=DS_daily$mean_pm25_pos)) + 
  labs(x='Annual mean using hourly data',y='Annual mean using daily data')
ggsave(paste0(fig_dir,"Compare_annual_mean_hourly_vs_daily.png"), width = 6, height = 3.5)

ggplot() + 
  geom_point(aes(x=DS_hourly$mean_pm25_pos,y=DS_hourly$mean_daily_std_pm25_pos)) + 
  labs(x='Annual mean using hourly data',y='Average daily std using hourly data')
ggsave(paste0(fig_dir,"Compare_annual_mean_hourly_vs_std.png"), width = 6, height = 3.5)

ggplot() + 
  geom_point(aes(x=DS_hourly$mean_pm25_pos,y=DS_hourly$mean_daily_range_pm25_pos)) + 
  labs(x='Annual mean using hourly data',y='Average daily range using hourly data')
ggsave(paste0(fig_dir,"Compare_annual_mean_hourly_vs_range.png"), width = 6, height = 3.5)

ggplot() + 
  geom_point(aes(x=DS_daily$std_pm25_pos,y=DS_hourly$mean_daily_std_pm25_pos)) + 
  labs(x='Annual std using daily data',y='Avg daily std using hourly data')
ggsave(paste0(fig_dir,"Compare_annual_std_daily_vs_avg_daily_std_hourly.png"), width = 6, height = 3.5)

ggplot() + 
  geom_point(aes(x=DS_hourly$std_pm25_pos,y=DS_hourly$max_daily_std_pm25_pos)) + 
  labs(x='Annual std using hourly data',y='Max daily std using hourly data')
ggsave(paste0(fig_dir,"Compare_annual_std_hourly_vs_max_daily_std_hourly.png"), width = 6, height = 3.5)

# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # Stats for plotting                                ####
# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
# hourlydt[, N_stn := .N, by = stn] # n obs per station
# dailybest[, N_stn := .N, by = stn] # n obs per station
# 
# # Mean and STD Total num. of non-NA obs. per station
# hourlydt[, mean_pm25 := mean(pm25, na.rm =T), by = stn]
# hourlydt[, std_pm25 := sd(pm25, na.rm =T), by = stn]
# 
# dailybest[, mean_pm25 := mean(pm25, na.rm =T), by = stn]
# dailybest[, std_pm25 := sd(pm25, na.rm =T), by = stn]
# 
# 
# # unique by station id:
# DS_hourly <- unique(hourlydt[,.(Latitude, Longitude, mean_pm25, std_pm25, stn , N_stn,Location_Setting, `State Name`)])
# DS_daily <- unique(dailybest[,.(Latitude, Longitude, mean_pm25, std_pm25, stn,N_stn,Location_Setting,`State Name`)])
# DS_daily <- DS_daily[DS_daily$stn %in% DS_hourly$stn]
# DS_hourly <- DS_hourly[DS_hourly$stn %in% DS_daily$stn]

# plot with log10 scale
my_breaks_mean <- round(10^c(.4,.6,.8,1.0,1.2,1.4))

cat("Mean PM2.5 by site:\n")
ggplot() +
  geom_polygon(data = map_data("state"), 
               aes(x = long, y = lat, group = group), 
               fill = 'NA', color = "grey") + 
  geom_point(data = na.omit(unique(DS_hourly[,.(mean_pm25_pos, Latitude, Longitude, stn, N_stn)])),
             aes(x=Longitude, y=Latitude, color = mean_pm25_pos, size = N_stn),
             alpha = 1) + 
  # coord_map("albers", parameters = list(at0 = 45.5, lat1 = 29.5), expand = T) + 
  scale_size_area(max_size = 5) + 
  scale_colour_viridis_c() +
#  scale_colour_viridis_c(name = "mean_pm25", trans = "log10", breaks = my_breaks_mean, labels = my_breaks_mean) +
  labs(x = "Longitude", y = "Latitude", color= "Mean PM2.5", 
       size = "Number of\nObservations") +
  # scale_size("RMSE") + 
  theme_minimal() +
  ggtitle("Hourly data mean PM2.5 by station") +
  guides(color = guide_colourbar(order=1),
         size = guide_legend(order=2))
ggsave(paste0(fig_dir,"PM25_mean_hourly_2018_filter.png"), width = 6, height = 3.5)


cat("Mean PM2.5 by site:\n")
ggplot() +
  geom_polygon(data = map_data("state"), 
               aes(x = long, y = lat, group = group), 
               fill = 'NA', color = "grey") + 
  geom_point(data = na.omit(unique(DS_daily[,.(mean_pm25_pos, Latitude, Longitude, stn, N_stn)])),
             aes(x=Longitude, y=Latitude, color = mean_pm25_pos, size = N_stn),
             alpha = 1) + 
  # coord_map("albers", parameters = list(at0 = 45.5, lat1 = 29.5), expand = T) + 
  scale_size_area(max_size = 4) + 
#  scale_colour_viridis_c(name = "mean_pm25", trans = "log10", breaks = my_breaks_mean, labels = my_breaks_mean) +
  scale_colour_viridis_c() +
  
  labs(x = "Longitude", y = "Latitude", color= "Mean PM2.5", 
       size = "Number of\nObservations") +
  theme_minimal() +
  ggtitle("Daily data mean PM2.5 by station") +
  guides(color = guide_colourbar(order=1),
         size = guide_legend(order=2))
  #theme(legend.position ='right') 

ggsave(paste0(fig_dir,"PM25_mean_daily_2018_filter.png"), width = 6, height = 3.5)

# plot with log10 scale
my_breaks_std <- round(10^c(.2,.4,.6,.8,1.0,1.2,1.4,1.6))

cat("STD PM2.5 by site:\n")
ggplot() +
  geom_polygon(data = map_data("state"), 
               aes(x = long, y = lat, group = group), 
               fill = 'NA', color = "grey") + 
  geom_point(data = na.omit(unique(DS_hourly[,.(std_pm25_pos, Latitude, Longitude, stn, N_stn)])),
             aes(x=Longitude, y=Latitude, color = std_pm25_pos, size = N_stn),
             alpha = 1) +
  #scale_colour_viridis_c(option = "magma", name = "std_pm25", trans = "log10", breaks = my_breaks_std, labels = my_breaks_std) +
  scale_colour_viridis_c() +
  # coord_map("albers", parameters = list(at0 = 45.5, lat1 = 29.5), expand = T) + 
  scale_size_area(max_size = 5) + 
  labs(x = "Longitude", y = "Latitude", color= "STD PM2.5", 
       size = "Number of\nObservations") +
  theme_minimal() +
  ggtitle("Hourly data standard deviation PM2.5 by station") +
  guides(color = guide_colourbar(order=1),
         size = guide_legend(order=2))
ggsave(paste0(fig_dir,"PM25_STD_hourly_2018_filter.png"), width = 6, height = 3.5)


cat("STD PM2.5 by site:\n")
ggplot() +
  geom_polygon(data = map_data("state"), 
               aes(x = long, y = lat, group = group), 
               fill = 'NA', color = "grey") + 
  geom_point(data = na.omit(unique(DS_daily[,.(std_pm25_pos, Latitude, Longitude, stn, N_stn)])),
             aes(x=Longitude, y=Latitude, color = std_pm25_pos, size = N_stn),
             alpha = 1) + 
 # scale_colour_viridis_c(option = "magma", name = "std_pm25", trans = "log10", breaks = my_breaks_std, labels = my_breaks_std) +
  scale_colour_viridis_c() +
# coord_map("albers", parameters = list(at0 = 45.5, lat1 = 29.5), expand = T) + 
  scale_size_area(max_size = 4) + 
  labs(x = "Longitude", y = "Latitude", color= "STD PM2.5", 
       size = "Number of\nObservations") +
  theme_minimal() +
  ggtitle("Daily data standard deviation PM2.5 by station") +
  guides(color = guide_colourbar(order=1),
         size = guide_legend(order=2))
ggsave(paste0(fig_dir,"PM25_STD_daily_2018_filter.png"), width = 6, height = 3.5)

ggplot() +
  geom_polygon(data = map_data("state"), 
               aes(x = long, y = lat, group = group), 
               fill = 'NA', color = "grey") + 
  geom_point(data = na.omit(unique(DS_hourly[,.(mean_daily_std_pm25_pos, Latitude, Longitude, stn, N_stn)])),
             aes(x=Longitude, y=Latitude, color = mean_daily_std_pm25_pos, size = N_stn),
             alpha = 1) + 
  # coord_map("albers", parameters = list(at0 = 45.5, lat1 = 29.5), expand = T) + 
  scale_size_area(max_size = 5) + 
  #scale_colour_viridis_c() +
  scale_colour_viridis_c(option="magma",name = "Avg daily std PM2.5", trans = "log10") +
  
  #  scale_colour_viridis_c(name = "mean_pm25", trans = "log10", breaks = my_breaks_mean, labels = my_breaks_mean) +
  labs(x = "Longitude", y = "Latitude", color= "Avg daily std PM2.5", 
       size = "Number of\nObservations") +
  # scale_size("RMSE") + 
  theme_minimal() +
  ggtitle("Hourly data mean PM2.5 by station") +
  guides(color = guide_colourbar(order=1),
         size = guide_legend(order=2))
ggsave(paste0(fig_dir,"PM25_avg_std_hourly_2018_filter.png"), width = 6, height = 3.5)

ggplot() +
  geom_polygon(data = map_data("state"), 
               aes(x = long, y = lat, group = group), 
               fill = 'NA', color = "grey") + 
  geom_point(data = na.omit(unique(DS_hourly[,.(mean_daily_range_pm25_pos, Latitude, Longitude, stn, N_stn)])),
             aes(x=Longitude, y=Latitude, color = mean_daily_range_pm25_pos, size = N_stn),
             alpha = 1) + 
  # coord_map("albers", parameters = list(at0 = 45.5, lat1 = 29.5), expand = T) + 
  scale_size_area(max_size = 5) + 
  #scale_colour_viridis_c() +
  scale_colour_viridis_c(option="magma",name = "Avg daily range PM2.5", trans = "log10") +
  labs(x = "Longitude", y = "Latitude", color= "Avg daily std PM2.5", 
       size = "Number of\nObservations") +
  # scale_size("RMSE") + 
  theme_minimal() +
  ggtitle("Hourly data mean PM2.5 by station") +
  guides(color = guide_colourbar(order=1),
         size = guide_legend(order=2))
ggsave(paste0(fig_dir,"PM25_avg_range_hourly_2018_filter.png"), width = 6, height = 3.5)


ggplot() +
  geom_polygon(data = map_data("state"), 
               aes(x = long, y = lat, group = group), 
               fill = 'NA', color = "grey") + 
  geom_point(data = na.omit(unique(DS_hourly[,.(max_daily_range_pm25_pos, Latitude, Longitude, stn, N_stn)])),
             aes(x=Longitude, y=Latitude, color = max_daily_range_pm25_pos, size = N_stn),
             alpha = 1) + 
  # coord_map("albers", parameters = list(at0 = 45.5, lat1 = 29.5), expand = T) + 
  scale_size_area(max_size = 5) + 
  #scale_colour_viridis_c() +
  scale_colour_viridis_c(option="magma",name = "Max daily range PM2.5", trans = "log10") +
  labs(x = "Longitude", y = "Latitude", color= "Max daily range PM2.5", 
       size = "Number of\nObservations") +
  # scale_size("RMSE") + 
  theme_minimal() +
  ggtitle("Hourly data mean PM2.5 by station") +
  guides(color = guide_colourbar(order=1),
         size = guide_legend(order=2))
ggsave(paste0(fig_dir,"PM25_max_range_hourly_2018_filter.png"), width = 6, height = 3.5)

ggplot() +
  geom_polygon(data = map_data("state"), 
               aes(x = long, y = lat, group = group), 
               fill = 'NA', color = "grey") + 
  geom_point(data = na.omit(unique(DS_hourly[,.(mean_daily_std_pm25_pos, Latitude, Longitude, stn, N_stn)])),
             aes(x=Longitude, y=Latitude, color = mean_daily_std_pm25_pos, size = N_stn),
             alpha = 1) + 
  # coord_map("albers", parameters = list(at0 = 45.5, lat1 = 29.5), expand = T) + 
  scale_size_area(max_size = 5) + 
  #scale_colour_viridis_c() +
  scale_colour_viridis_c(option="magma",name = "Avg daily std PM2.5", trans = "log10") +
  labs(x = "Longitude", y = "Latitude", color= "Avg daily std PM2.5", 
       size = "Number of\nObservations") +
  # scale_size("RMSE") + 
  theme_minimal() +
  ggtitle("Hourly data avg daily std PM2.5 by station") +
  guides(color = guide_colourbar(order=1),
         size = guide_legend(order=2))
ggsave(paste0(fig_dir,"PM25_avg_std_hourly_2018_filter.png"), width = 6, height = 3.5)


ggplot() +
  geom_polygon(data = map_data("state"), 
               aes(x = long, y = lat, group = group), 
               fill = 'NA', color = "grey") + 
  geom_point(data = na.omit(unique(DS_hourly[,.(max_daily_range_pm25_pos, Latitude, Longitude, stn, N_stn)])),
             aes(x=Longitude, y=Latitude, color = max_daily_range_pm25_pos, size = N_stn),
             alpha = 1) + 
  # coord_map("albers", parameters = list(at0 = 45.5, lat1 = 29.5), expand = T) + 
  scale_size_area(max_size = 5) + 
  #scale_colour_viridis_c() +
  scale_colour_viridis_c(option="magma",name = "Max daily range PM2.5", trans = "log10") +
  labs(x = "Longitude", y = "Latitude", color= "Max daily range PM2.5", 
       size = "Number of\nObservations") +
  # scale_size("RMSE") + 
  theme_minimal() +
  ggtitle("Hourly data: max daily range PM2.5 by station") +
  guides(color = guide_colourbar(order=1),
         size = guide_legend(order=2))
ggsave(paste0(fig_dir,"PM25_max_range_hourly_2018_filter.png"), width = 6, height = 3.5)


ggplot() +
  geom_polygon(data = map_data("state"), 
               aes(x = long, y = lat, group = group), 
               fill = 'NA', color = "grey") + 
  geom_point(data = na.omit(unique(DS_hourly[,.(mean_max_diff_avg_pos, Latitude, Longitude, stn, N_stn)])),
             aes(x=Longitude, y=Latitude, color = mean_max_diff_avg_pos, size = N_stn),
             alpha = 1) + 
  # coord_map("albers", parameters = list(at0 = 45.5, lat1 = 29.5), expand = T) + 
  scale_size_area(max_size = 5) + 
  #scale_colour_viridis_c() +
  scale_colour_viridis_c(option="magma",name = expression(paste(Delta,
    "PM2.5")), trans = "log10") +
  #scale_colour_viridis_c(option="magma",name = expression(paste(Delta, "PM2.5"))) +
  labs(x = "Longitude", y = "Latitude", 
       size = "Number of\nObservations") +
  # scale_size("RMSE") + 
  theme_minimal() +
  ggtitle("Avg difference between mean and max PM2.5") +
  guides(color = guide_colourbar(order=1),
         size = guide_legend(order=2))
ggsave(paste0(fig_dir,"PM25_avg_mean_max_diff_hourly_2018_filter_log_scale.png"), width = 6, height = 3.5)


ggplot() +
  geom_polygon(data = map_data("state"), 
               aes(x = long, y = lat, group = group), 
               fill = 'NA', color = "grey") + 
  geom_point(data = na.omit(unique(DS_hourly[,.(mean_max_diff_max_pos, Latitude, Longitude, stn, N_stn)])),
             aes(x=Longitude, y=Latitude, color = mean_max_diff_max_pos, size = N_stn),
             alpha = 1) + 
  # coord_map("albers", parameters = list(at0 = 45.5, lat1 = 29.5), expand = T) + 
  scale_size_area(max_size = 5) + 
  #scale_colour_viridis_c() +
  scale_colour_viridis_c(option="magma",name = "Difference PM2.5", trans = "log10") +
  labs(x = "Longitude", y = "Latitude", 
       size = "Number of\nObservations") +
  # scale_size("RMSE") + 
  theme_minimal() +
  ggtitle("Difference between mean and max PM2.5 by station") +
  guides(color = guide_colourbar(order=1),
         size = guide_legend(order=2))
ggsave(paste0(fig_dir,"PM25_max_mean_max_diff_hourly_2018_filter.png"), width = 6, height = 3.5)

