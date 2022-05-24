## ----setup, include=FALSE----------------------------------------------------------------
#knitr::opts_chunk$set(echo = FALSE, warning=FALSE, include=FALSE)

library(tidyverse)
library(leaflet)
library(shiny)
library(jsonlite)
library(rnoaa)
library(DT)
library(shinycssloaders)
library(rsconnect)
library(reactable)
library(httr)


## ----------------------------------------------------------------------------------------
#Original stations dataset 
#setwd("/Users/tommysmale/classroom/capstone")
stations <- read.csv("data/ghcnd-stations.csv")
stations <- stations %>% drop_na(lng, lat)
#Read us_stations with state data 
us_stations <- read_csv("data/us_stations.csv", show_col_types = FALSE)
us_stations <- stations %>% subset(id %in% us_stations$id) %>% 
  add_column(state=us_stations$state, name=us_stations$name)
#Add counties 
counties <- read.csv("data/og_counties.csv")
us_stations <- us_stations %>% 
  subset(lat %in% counties$lat & lng %in% counties$lng) %>%
  add_column(county=counties$county) %>% 
  filter(!is.na(county))
states <- sort(unique(us_stations$state))


## ----------------------------------------------------------------------------------------
#For map testing
haversine <- function(loc1=NULL, loc2=NULL) { 
  if(is.null(loc1) || is.null(loc2)) {stop("haversine null")}
  #Expecting lat/lng format 
  #Return value is in miles 
  loc2 <- list(lat=loc2[1], lng=loc2[2])
  if(is.character(loc1) || is.character(loc2)) { 
    stop("please enter numeric value to harversine function")
  }
  if(loc1$lat > 90 || loc1$lat < -90) { 
    stop("harversine invalid lat coordinate (first arg)")
  }
  if(loc2$lat > 90 || loc2$lat < -90) { 
    stop("harversine invalid lat coordinate (second arg)")
  }
  if(loc1$lng > 180 || loc1$lng < -180) {
    stop("harversine invalid lng coordinate (first arg)")
  }
  if(loc2$lng > 180 || loc2$lat < -180)
    stop("harversine invalid lng coordinate (second arg)")
  loc1 <- lapply(loc1, '*', pi/180)
  loc2 <- lapply(loc2, '*', pi/180)
  dlat <- loc1$lat - loc2$lat
  dlong <- loc1$lng - loc2$lng
  a <- sin(dlat / 2)^2 + cos(loc1$lat) * cos(loc2$lat) * sin(dlong / 2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1-a)) #Ret angle between (x,y) using equator
  earth_radius <- 3963 #in miles
  distance <- earth_radius * c
  distance
}
#Returns dataframe of stations within specified radius for given stations
#Center is df so can be multiple stations 
#Radius is in miles
#Radius may be less than zero if user does not want to apply filter but just select stations
station_radius <- function(center=NULL, radius) { 
  if(is.null(center)) {stop("station_radius() NULL")}
  if(!is.data.frame(center)) stop("station_radius expecting df")
  if(radius < 0) return(NULL)
  center$lat <- center$lat * (pi/180)
  center$lng <- center$lng * (pi/180)
  lat_degree <- us_stations$lat * (pi/180)
  lng_degree <- us_stations$lng * (pi/180)
  neighbors <- data.frame()
  for(i in 1:nrow(center)) {
    dlat <- lat_degree - center$lat[i]
    dlong <- lng_degree - center$lng[i]
    a <- sin(dlat / 2)^2 + cos(center$lat[i]) * cos(lat_degree) * sin(dlong / 2)^2
    c <- 2 * atan2(sqrt(a), sqrt(1-a)) #Ret angle between (x,y) using equator
    earth_radius <- 3963 #in miles
    us_stations$distance <- earth_radius * c
    new_neighbors <- us_stations %>% filter(distance <= radius)
    duplicates <- new_neighbors$id %in% neighbors$id
    neighbors <- rbind(neighbors, new_neighbors[!duplicates, ])
    us_stations <- select(us_stations, -distance)
  }
  neighbors
}

## ----------------------------------------------------------------------------------------
#Functions related to google cloud API 
#Make sense of the JSON returned from google
parse_data <- function(data=NULL) {
  if(is.null(data)) {stop("parse_data NULL")}
  status <- data$status
  if (status == "OK") {
    #Currently only gets one location 
    location <- data$candidates[[1]]
    lng_lat <- location$geometry$location
    if(is.null(lng_lat)) { 
      return("No long/lat available")
    }else { 
      return(lng_lat)
    }
  } else {
    error_msg <- data$error_message
    if (!is.null(error_msg)) {
      print(error_msg)
    } else {
      if (status == "ZERO_RESULTS") {
        error_msg <- "No results found for search"
      } else if (status == "OVER_QUERY_LIMIT") {
        error_msg <- "Youre broke boi buy some more cloud credits"
      } else if (status == "REQUEST_DENIED") {
        error_msg <- "Missing key"
      } else if (status == "UNKNOWN_ERROR") {
        error_msg <- "Something went wrong"
      } else {
        error_msg <- "WHATWHATWHAAAAATT"
      }
    }
    return(error_msg)
  }
}
#Make sure URL is valid 
#Returns a dataframe with lat, lng columns
find_place <- function(search=NULL) { 
  if(is.null(search)) {stop("find_place NULL")}
  key <- read.delim(".google_key", "r",
                    header=FALSE, col.names="key")$key
  url <- ""
  debug <- FALSE
  if(debug) url <- 
    "https://raw.githubusercontent.com/thsmale/weather/main/data/jamestown.json"
  else { url <- paste(
    "https://maps.googleapis.com/maps/api/place/findplacefromtext/json?input=", 
               search, "&inputtype=textquery&fields=geometry&key=", key, sep="")
  }
  header <- curlGetHeaders(url)
  status <- attr(header, which="status")
  if(status == 200) { 
    fd <- url(url, method="libcurl", open = "r", blocking=FALSE, encoding="UTF-8")
    if(isOpen(fd)) { 
      data <- readLines(fd)
      data <- stream_in(textConnection(minify(data)))
      res <- parse_data(data)
      close(fd)
      unlink(fd)
      res <- cbind(res, name=search)
      return (res)
    }else { 
      print("Uhhh error opening data")
      return (-1)
    }
  } else { 
    print("Invalid search bro")
    return (-1)
  }
}
#Takes search input from user
#Returns appropriate coordinates 
get_coords <- function(input=NULL) {
  if(is.null(input)) {stop("get_coords NULL")}
  search <- trimws(input)
  search <- gsub(" ", "%20", search)
  return (find_place(search))
}


## ----------------------------------------------------------------------------------------
#NOAA functions 
noaa_token <- read.delim(".noaakey", header=FALSE, col.names="key")$key
options(noaakey = noaa_token)
#Takes in coordinates to return station id
get_station <- function(coords) { 
  if(is.null(coords)) stop("get_station coords is empty")
  station <- us_stations %>% 
    filter(lat==coords[1], lng==coords[2])
  return (station)
}
#Test for weather station data
#station_id is a vector of station_id's
#my station_id's are GHCND, there are other types that aren't implemented
#TODO: Response count > 25 
#Returns NULL if nothing successful 
#Only care about mindate, maxdate, name, id
#Return notification for missing data
get_datasets <- function(station_ids) { 
  url <- "https://www.ncdc.noaa.gov/cdo-web/api/v2/stations/"
  url <- "https://www.ncdc.noaa.gov/cdo-web/api/v2/datasets?"
  station_summary <- data.frame(station_id = character(), 
                                uid=character(), mindate=character(), 
                                maxdate=character(), name=character(), 
                                datacoverge=integer(), id=character())
  col_count <- ncol(station_summary)-1
  for(station_id in station_ids) {
    station <- station_id
    if(substr(station_id, 1, 5) != "GHCND")
      station <- paste("GHCND:", station, sep="")
    param <- paste("stationid=", station, sep = "")
    res <- GET(paste(url, param, sep = ""),
               add_headers(token = noaa_token),
               user_agent("tsmale@mail.csuchico.edu"))
    if (res$status_code == 200) {
      ss <- fromJSON(content(res, as = "text", encoding = 'UTF-8'))$results
      #Vunerability if API does not return the columns I specified
      if(!is.null(ss)) {
        if(ncol(ss) == col_count) {
          ss <- cbind(ss, "station_id"=station_id)
          station_summary <- rbind(station_summary, ss)
        }else{
          warning("API returned unexpected result", " ", station)
          next
        }
      }else {
        warning("API returned NULL results", " ", station)
        next
      }
    } else{
      warning("get_datasets bad ret code ", res$status_code, " ", station)
      next
    }
  }
  if(nrow(station_summary > 0)) {
    station_summary <- rename(station_summary, dataset_name=name)
    station_summary <- rename(station_summary, dataset_id=id)
    relevant <- station_summary %>% 
      select(mindate, maxdate, dataset_name, dataset_id, station_id)
    return(relevant)
  }else {
    return(NULL)
  }
}
#Get data using yr and stationid
fetch_noaa_data <- function(id, daterange) { 
  if(is.null(id) || is.null(range)) { 
    stop("fetch_noaa_data passed null")
  }
  if(substr(id, 1, 5) != "GHCND")
    id <- paste("GHCND:", id, sep="")
  cur_date <- str_split(daterange[1], pattern = "-")
  cur_yr <- cur_date[[1]][1]
  cur_month <- cur_date[[1]][2]
  cur_day <- cur_date[[1]][3]
  next_year <- as.integer(cur_yr) + 1
  next_year <- paste(next_year, cur_month, cur_day, sep="-")
  if(next_year > Sys.Date()) 
    stop("next year is not valid")
  out <- ncdc(datasetid='GHCND', stationid=id, 
              startdate = daterange[1], enddate = next_year)
  out$data
}
#Fetch data categories using curl 
url <- "https://www.ncdc.noaa.gov/cdo-web/api/v2/datacategories/?stationid="
header <- c("Access-Control-Allow-Headers: token\r\n", noaa_token)
header <- paste("token:", noaa_token, sep="")
fetch_categories <- function(id=NULL) {
  if(is.null(id)) {stop("fetch_categories NULL")}
  if(!is.character(id)) stop("Please enter a character id to fetch categories")
  if(substr(id, 1, 5) != 'GHCND')
    id <- paste('GHCND:', id, sep="")
  url <- paste(url, id, sep="")
  url_headers <- curlGetHeaders(url)
  header <- paste("token:", noaa_token, sep="")
  url(url, method="libcurl", open = "r", blocking=FALSE, encoding="UTF-8")
  fd <- url(
    url,
    method = "libcurl",
    open = "r",
    blocking = FALSE,
    encoding = "UTF-8",
    headers = header
  )
  if (isOpen(fd)) {
    data <- readLines(fd)
    data <- stream_in(textConnection(minify(data)))
    res <- parse_data(data)
    close(fd)
    unlink(fd)
  }else { 
    print("Unable to open fd")
  }
}
## ----------------------------------------------------------------------------------------
#Take in a station id and return df of data categories 
#TODO add error checking, change to curl_get_memory like rnoaa uses 
# Return null in case of error 
# Accept more parameters like date input etc 
# Pretty useless endpoint as far as I can tell so far
data_categories <- function(station_id=NULL) {
  if(is.null(station_id)) {stop("data_categories NULL")}
  if(station_id == "") {
    print("data_categories didn't receive station_id as required")
    station_id <- "GHCND:USC00048758" #for testing 
  }
  if(substr(station_id, 1, 5) != 'GHCND') 
    station_id <- paste('GHCND:', station_id, sep="")
  offset <- 0
  count <- 1
  data_cats <- data.frame()
  while(offset < count) {
    command <- paste("bash/categories.sh", station_id, offset, sep = " ")
    system(command = command)
    categories <- read_json("data/categories.json", simplifyVector = TRUE)
    offset <- offset + categories$metadata$resultset$limit
    count <- categories$metadata$resultset$count
    #add dataframe to categories use cbind 
    data_cats <- rbind(data_cats, categories$results)
  }
  if(is.null(data_cats)) {
    data_cats <- data.frame(name="None")
  }
  return (data_cats)
}
#NOAA API Request's Range must be 1 year 
#User's may want request's spanning multiple years
#Adj end date to be only 1 year greater than start date
#If end date is already same year or even less return NULL
adj_date <- function(daterange) {
  #Can only request one year at a time
  startdate = daterange[1]
  enddate = daterange[2]
  start_yr <- format(startdate, "%Y")
  end_yr <- format(enddate, "%Y") 
  if((as.integer(end_yr) - as.integer(start_yr)) > 1) {
    month <- format(startdate, "%m")
    day <- format(startdate, "%d")
    next_year <- as.integer(start_yr) + 1
    enddate <- paste(next_year, month, day, sep="-")
    #Prevent's 1 year in the future from being a non existent date
    if(enddate > Sys.Date()) 
      enddate <- Sys.Date()
  }
  return(c(startdate, enddate))
}
#Get actual weather data from NOAA 
#What if there is no stationid entered? 
#Causes error if incorrect date entered 
#Some of the datasetids: GHCND, 2GSOM, GSOY, NORMAL_ANN, NORMAL_DLY, NORMAL_MLY
get_data <- function(datasetid, 
                     daterange,
                     stationid,
                     update_progress) {
  #Colnames match those returned by noaa data 
  #Dataset is a col I added to make EDA better
  df <- data.frame(date=character(), datatype=character(), 
                   station=character(), value=double())#, 
                   #dataset_id=character())
  #It should be each data set and all the corresponding stations
  #Should verify station actually has data for datatype
  #Required params
  url <- "https://www.ncei.noaa.gov/cdo-web/api/v2/data?"
  if(substr(stationid, 1, 5) != 'GHCND') 
    stationid <- paste('GHCND:', stationid, sep="")
  dset_param <- paste("datasetid=", datasetid, sep="", collapse = "")
  url <- paste(url, dset_param, sep="")
  url <- paste(url, paste("&stationid=", stationid, sep=""), sep="")
  num_yrs <- 
    as.integer(format(daterange[1], '%Y')) - 
    as.integer(format(daterange[2], '%Y'))
  num_yrs <- num_yrs + 1
  #Get all the data for every year requested
  while(daterange[1] < daterange[2]) {
    adjdate <- adj_date(daterange)
    cur_url <- paste(url, paste("&startdate=", adjdate[1], sep=""), sep="")
    cur_url <- paste(cur_url, paste("&enddate=", adjdate[2], sep=""), sep = "")
    offset <- 0
    count <- 1
    #Get all the data possible within the year
    while(offset < count) {
      new_url <- paste(cur_url, paste("&offset=", offset, sep=""), sep="")
      print(new_url)
      res <- GET(new_url,
                 add_headers(token = noaa_token),
                 user_agent("tsmale@mail.csuchico.edu"))
      if (res$status_code == 200) {
        #print(content(res))
        noaa <- fromJSON(content(res, as = "text", encoding = 'UTF-8'))
        data <- noaa$results
        if(is.null(noaa$metadata)) {
          offset <- 0
          count <- -1
        }else {
          limit <- noaa$metadata$resultset$limit
          offset <- offset + limit
          count <- noaa$metadata$resultset$count
          update_progress(limit=limit, count=count, num_yrs)
        }
        #Often times NOAA request is successful but return's empty results
        if(is.null(data) || length(data) == 0) {
          count <- -1
          next
        }
        #Returned data from NOAA does not include the corresponding dsetid
        #We want this for our EDA
        #data <- cbind(data, dataset_id=dset)
        if(nrow(df) < 1) {
          df <- data
          next
        }
        if(ncol(df) == ncol(data)) {
          df <- rbind(df, data)
        }else {
          print("DF and Data rows DONUT match")
          print(df)
          print(data)
          count <- -1
        }
      }else {
        print("Bad req")
        print(content(res))
        count <- -1
      }
    }
    daterange[1] <- adjdate[2]
  }
  return(df)
}
#Input is files from FileInput UI
#2 columns required: lng, lat
#Name columns is optional
parse_upload_locations <- function(file_df, upload_id) {
  uploaded <- data.frame(id=character(),
                         lng=double(),
                         lat=double(),
                         name=character())
  for(i in 1:nrow(file_df)) {
    file <- read.csv(file_df$datapath, header=FALSE)
    #Check we were able to read it
    if(nrow(file) < 1)
      next
    if(ncol(file) < 2 || ncol(file) > 3) 
      next
    id <- character()
    lng <- double()
    lat <- double()
    name <- character()
    #Interpret lng/lat/name
    for(j in 1:ncol(file)) {
      x <- tryCatch({
        as.double(file[, j])
        },
        warning=function(cond) {
          #Can not interpet as double, must be name
          return(file[, j])
        }
      )
      if(typeof(x) == "character") {
        name <- x 
        next
      }
      
      if(x >= -90 && x <= 90) {
        lat <- x
        next
      }
      lng <- x
    }
    #Was unable to extract lng and lat
    if(length(lng) < 1 || length(lat) < 1)
      next
    #If no names provided set to empty string
    if(length(name) < 1) {
      name <- rep("", nrow(file))
    }
    rows <- nrow(file)-1
    id <- paste("upload", upload_id:rows, sep="")
    df <- data.frame(id, lng, lat, name)
    uploaded <- rbind(uploaded, df)
    upload_id <- upload_id + rows
  }
  return(uploaded)
}






