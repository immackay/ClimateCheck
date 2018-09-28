# Information can be found here: 
# ftp://client_climate@ftp.tor.ec.gc.ca/Pub/Get_More_Data_Plus_de_donnees/

# Station inventory is located here:
# ftp://ftp.tor.ec.gc.ca/Pub/Get_More_Data_Plus_de_donnees/Station%20Inventory%20EN.csv
# Sometimes acts "down" if navigated to directly, thus why there's no download function
# Save it as "stations.csv" in this folder



# Download climate data from @begin:@end
# @station is provided by Station.ID in stations.csv
# @timeframe is either 1 (hourly), 2 (daily), or 3 (monthly)
#
# wget is the "supported" utility but curl works fine
# and is more accessible on all systems

#' Base utility for Climate Bulk Data fetching
#'
#' @param station
#' @param begin
#' @param end
#' @param timeframe
#' @param method
#'
#' @references
#' \url{ftp://client_climate@ftp.tor.ec.gc.ca/Pub/Get_More_Data_Plus_de_donnees/Readme.txt}
#' \url{ftp://ftp.tor.ec.gc.ca/Pub/Get_More_Data_Plus_de_donnees/Station%20Inventory%20EN.csv}
#'
#' @export
#'
#' @examples
#' \donttest{
#' # Station 51423 is KAMLOOPS A
#' begin <- 2014
#' end <- 2017
#' df <- download.ClimateData(51423, begin, end, "hourly", "curl")
#' }
download.ClimateData <- function(station, begin, end, timeframe=c("hourly", "daily", "monthly"), method=c("curl", "wget")) {
  # check for nulls
  for (year in begin:end) {
    for (month in 1:12) {
      day <- 14 # arbitrary, will always download full month
      downloadUrl <- paste("http://climate.weather.gc.ca/climate_data/bulk_data_e.html?format=csv",
                           "&stationID=", station,
                           "&Year=", year,
                           "&Month=", month,
                           "&Day=", day,
                           "&timeframe=", timeframe,
                           "&submit=%20Download+Data",
                           sep="")
      sDir <- paste(".", "csv", station, year, sep="/")
      if (!dir.exists(sDir)) {
        dir.create(sDir, recursive = TRUE)
      }
      destUrl <- paste(sDir, "/", month, ".csv", sep="")
      if (file.exists(destUrl)) {
        print(paste("File already exists -", destUrl))
      } else {
        if (method=="curl") {
          download.file(downloadUrl, destUrl, method = "curl", quiet = TRUE, extra = "-L")
        } else {
          download.file(downloadUrl, destUrl, method = "wget", quiet = TRUE, extra = "--content-disposition")
        }
        print(paste("Successfully downloaded y", year, "m", month))
      }
    }
  }
}

# Download all data from @stations for the specified @timeframe
# See download.ClimateData for available timeframes
# See stations.csv (read.ClimateStationList) for available Station IDs
download.ClimateDataStations <- function(stations, timeframe) {
  
}

# Download all available data for the specified @timeframe
# See download.ClimateData for available timeframes
download.ClimateDataAll <- function(timeframe) {
  
}

# Compile all files for @station into year-by-year .csv files
compile.ClimateData <- function(station, begin, end, overwrite=F) {
  # check if file exists
  
  for (year in begin:end) {
    firstUrl <- paste(paste(".", "csv", station, year, sep="/"), "csv", sep=".")
    for (month in 1:12) {
      url <- paste(".", "csv", station, year, month, sep="/")
      url <- paste(url, ".csv", sep="")
      if (month == 1) {
        file.copy(url, firstUrl, overwrite=overwrite)
      } else {
        tempFile <- read.csv(paste(paste(".", "csv", station, year, month, sep="/"), "csv", sep="."), header=TRUE, skip=15)
        tempUrl <- paste(".", "csv", station, year, month, sep="/")
        tempUrl <- paste(url, "temp.csv", sep="")
        write.table(tempFile, file=tempUrl, sep=",", col.names=F, row.names=F)
        file.append(firstUrl, tempUrl)
        file.remove(tempUrl)
      }
    }
  }
}

# Read the station list
read.ClimateStationList <- function() {
  if (!file.exists("stations.csv"))
    stop("Download the newest Station Inventory! See Util.R")
  df <- read.csv("stations.csv", skip=3)
  df
}

# Read the header information for @station
read.ClimateHeader <- function(station) {
  baseUrl <- paste("./csv", station, sep="/")
  if (dir.exists(baseUrl)) {
    possibleUrl <- list.files(baseUrl, pattern=".csv")[1]
    # annoying workaround
    df <- data.frame(t(data.frame(read.csv(paste(baseUrl, possibleUrl, sep="/"), header=FALSE)[1:8,], row.names=1)))
    df
  } else {
    # download 1 dataset?
    stop("Station not found!")
  }
}

# Read the downloaded data for @station, @year
# Must run compile.ClimateData first
read.ClimateCSV <- function(station, year) {
  # check if file exists
  df <- read.csv(paste("./csv/", station, "/", year, ".csv", sep=""), header=TRUE, skip=15)
  dataFrame <- data.frame(df$Date.Time, 
                          format(as.POSIXct(df$Date), "%m-%d"), df$Temp...C.)
  colnames(dataFrame) <- c("Date", "MD", "TempC")
  lastKnown <- NA
  backProp <- 1
  for (checkTemp in seq_along(dataFrame$TempC)) {
    if (is.na(dataFrame$TempC[checkTemp])) {
      if (is.na(lastKnown)) {
        backProp <- backProp + 1
      } else {
        dataFrame$TempC[checkTemp] <- lastKnown
      }
    } else {
      if (is.na(lastKnown)) {
        dataFrame$TempC[1:backProp] <- dataFrame$TempC[checkTemp]
      }
      lastKnown <- dataFrame$TempC[checkTemp]
    }
  }
  dataFrame
}