#!/usr/bin/env Rscript

# This script prepares daily PM measurements from EPA data, creating a candidate mod0 file for NEMIA.
# This combines previous code in import_PM_monitors.R and primary_PM_monitors.R

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Libraries                                         ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

suppressMessages(library(data.table))
suppressMessages(library(fst))
library(sf)
# devtools::install_github("rushgeo/nngeo") # use for parallel calculation with unprojected coords
library(nngeo)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Open Measurement Data                             ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# to unzip the files, we have to temporarily change working directory (calling out to unzip on linux, got weird)
storewd <- getwd()
# change working directory to where the data live, open as data.table
filepath <- "~/Documents/Research/MtSinai/Data/aqs_raw/hourly" # "data/aqs_raw/epa_daily"
setwd(filepath)
zipfiles <- list.files(pattern = "daily.*\\.zip")
dailydt <- rbindlist(lapply(zipfiles, function(x) fread(paste0("unzip -cq ", x))))

# restore the working directory
setwd(storewd)
rm(zipfiles, filepath)

# how big is the data
paste("The loaded PM measurement data table has dimensions", paste(dim(dailydt), collapse = ",")) # 9009658,29 for 1998-2017
# pryr::object_size(dailydt) #  1.8 GB

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Subset to NEMIA, Dates 2000-2015                  ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# clean the day
dailydt[, day := as.Date(`Date Local`)]

# nemiastates <- c("Maine", "New Hampshire", "Vermont", "Massachusetts", 
#                  "Rhode Island", "Connecticut", "New York", "New Jersey", 
#                  "Delaware", "Pennsylvania", "Maryland", "West Virginia", "Virginia", "District Of Columbia")
# dailydt <- dailydt[`State Name` %in% nemiastates & day >= as.Date("2017-12-31") & day < as.Date("2019-01-01"), ]
# paste("PM measurements table subset to NEMIA 2000-2015 has dimensions", paste(dim(dailydt), collapse = ",")) # 1243560,30

# look at 2018 all sites
dailydt <- dailydt[ day > as.Date("2017-12-31") & day < as.Date("2019-01-01"), ]
paste("PM measurements table subset to 2018 has dimensions", paste(dim(dailydt), collapse = ",")) # 1243560,30


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Variable construction                             ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
dailydt <- dailydt[`Sample Duration` %in% c("24 HOUR", "24-HR BLK AVG"),]
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
# Fixing Inconsistent Datums                        ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# general function for converting CRS
toEPSG <- function(x,y, from_epsg, to_epsg){ 
  fromPoint = st_sfc(st_point(c(x, y)), crs = from_epsg)
  toPoint = st_transform(fromPoint, crs = to_epsg)
  return(toPoint[[1]])}

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
# Get nearest AODIDs                                ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
# setkey(dailybest, stn, long_wgs84, lat_wgs84)
# unique_coords = unique(dailybest[, .(stn, long_wgs84, lat_wgs84)])
# unique_coords_sf = st_as_sf(unique_coords, coords = c("long_wgs84", "lat_wgs84"), crs = 4326)
# 
# # maiac grid
# maiac_sf = readRDS("data/intermediate/grid.Echo.06122017_sf.rds")
# 
# # nearest maiac grid points
# paste("Time to calculate nearest MAIAC grid centroids to EPA PM stations:")
# system.time(epa_nn <- st_nn(unique_coords_sf, maiac_sf, k = 1, returnDist = TRUE))
# 
# # copy nearest maiac info to station coordinate table
# unique_coords[, `:=`(maiac_rowid = unlist(epa_nn$nn), maiac_distance = epa_nn$dist[,1])]
# maiac_grid = as.data.table(maiac_sf)
# unique_coords[, aodid := maiac_grid[unique_coords$maiac_rowid, aodid]]
# 
# # join back to measurement table
# setkey(unique_coords, stn, long_wgs84, lat_wgs84)
# dailybest[unique_coords, c("aodid", "maiac_centroid_distance") := .(aodid, maiac_distance)]
# rm(unique_coords, unique_coords_sf, maiac_grid, maiac_sf)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Output                                            ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

output_file = "/Users/yshiga/Documents/Research/MtSinai/Data/monitors/epa_daily/dailybest_nemia.fst"
write_fst(dailybest, output_file, compress = 100)
paste("Wrote output file:", output_file)
paste0("Final table had dimensions ", paste0(dim(dailybest), collapse = ","),
      ", included dates from ", min(dailybest$day), " to ", max(dailybest$day), 
      ", and included ", dailybest[,uniqueN(stn)], " unique stations.")


