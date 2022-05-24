source("weather.R")
library(grid)
## ----------------------------------------------------------------------------------------
#Server
server <- function(input, output, session) { 
  #I can't wait to reduce the number of reactive vals
  search_id <- reactiveVal(value=0)
  upload_id <- reactiveVal(value=0)
  user_stations <- reactiveValues(df=NULL)
  selected_stations <- reactiveValues(df=NULL)
  station_summary <- reactiveValues(df=NULL)
  noaa_dataset <- reactiveValues(df=NULL)
  notification_ids <- character(0)
  out_plots <- reactiveVal(NULL)
  user_plots <- reactiveVal(list())
  #Map stations are all stations currently displayed on the map
  map_stations <- reactive({
    x <- NULL
    #Filter station by state
    if(!is.null(input$state_picker)) {
      x <- subset(us_stations, state %in% input$state_picker)
    }
    #Filter station by county
    #Add FIPS bc duplicating county names
    if(!is.null(input$county_picker)) {
      if(is.null(x)) {
        x <- subset(us_stations, county %in% input$county_picker)
      }else {
        #Filter by counties in respective states
        #So if you have 2 selected states, CA and AZ, filter by Butte, all the AZ stations remain
        filtered_counties_df <- x[x$county %in% input$county_picker, ]
        county_states <- unique(filtered_counties_df$state)
        non_county_states <- setdiff(input$state_picker, county_states)
        county_rows <- filtered_counties_df %>% 
          rownames()
        county_rows <- as.integer(county_rows)
        non_county_state_rows <- x[x$state %in% non_county_states, ] %>%
          rownames()
        non_county_state_rows <- as.integer(non_county_state_rows)
        x <- x %>% 
          filter(rownames(x) %in% union(non_county_state_rows, county_rows))
      }
    }
    #Show neighboring stations of selected stations within specified radius
    #ADVICE: Select stations with radius -1, then apply radius filter
    #-1 Disables filtering by radius
    if(!is.null(selected_stations$df) && nrow(selected_stations$df) > 0) {
      new_map_stations <- 
        station_radius(selected_stations$df, input$change_radius)
      if(!is.null(new_map_stations)) {
        x <- new_map_stations
      }
    }
    x
  })
  #Process search and update map
  #Searches are separate from NOAA data 
  observeEvent(input$search_button, {
    searched_coords <- c()
    if(input$search_input != "") 
      searched_coords <- get_coords(input$search_input)
    else {
      showNotification("Search query can't be empty")
      return("")
    }
    if(!is.null(searched_coords)) {
      id <- paste("search", as.character(search_id()), sep="")
      search_id(search_id()+1)
      name <- searched_coords$name
      lng <- as.double(searched_coords$lng)
      lat <- as.double(searched_coords$lat)
      if (is.null(user_stations$df)) {
        #First search need to init user_stations$df
        user_stations$df <- data.frame(id, name, lng, lat)
      } else {
        #Additional searches
        user_stations$df[nrow(user_stations$df) + 1, c('id', 'name')] <-
          c(id, name)
        user_stations$df[nrow(user_stations$df), c('lng', 'lat')] <-
          c(lng, lat)
      }
      leafletProxy("map") %>%
        addTiles() %>%
        addCircleMarkers(
          lng,
          lat,
          label = name,
          color = "red", 
          fillOpacity = .9, 
          weight = 17, 
          radius = 10, 
          layerId = id
        )
      showNotification("Successful search")
    } else {
      showNotification("Search failed")
    }
  })
  #User can upload data, we will process it and display it on map
  #Currently only accepts CSV and strict column input
  #Make more flexible
  observeEvent(input$upload_locations, {
    upload_df <- parse_upload_locations(input$upload_locations, upload_id())
    if(nrow(upload_df > 0)) {
      upload_id(upload_id()+nrow(upload_df))
      if (is.null(user_stations$df)) {
        #First search need to init user_stations$df
        user_stations$df <- upload_df
      } else {
        #Additional searches
        user_stations$df[nrow(user_stations$df) + 1, c('id', 'name')] <-
          c(upload_df$id, upload_df$name)
        user_stations$df[nrow(user_stations$df), c('lng', 'lat')] <-
          c(upload_df$lng, upload_df$lat)
      }
    }
  })
  #Update county select input according to states the user has selected 
  observe({
    updateSelectInput(session, "county_picker", 
                      choices = us_stations %>% 
                        filter(state %in% input$state_picker) %>% 
                        pull(county) %>% 
                        unique() %>% 
                        sort(), 
                      selected = input$county_picker)
  })
  #Initialize leaflet map
  output$map <- renderLeaflet({
    map <- leaflet(filter(us_stations, state == "California")) %>%
      addTiles() %>%
      addCircleMarkers(
        lng =  ~ lng,
        lat = ~ lat,
        radius = 1,
        label =  ~ name
      ) 
    map
  })
  #Update map to changes by user 
  observe({
    radius <- 1
    if(!is.null(input$map_zoom)) {
      if(input$map_zoom > 7) {
        radius <- 3
      }
      if(input$map_zoom >= 10) { 
        radius <- 4
      }
    }
    if(is.null(map_stations()) || nrow(map_stations()) < 1) {
      leafletProxy("map") %>% 
        clearMarkers()
    }else {
      leafletProxy("map", data=map_stations()) %>% 
        clearMarkers() %>% 
        addCircleMarkers(
          lng =  ~ lng,
          lat = ~ lat,
          radius = radius,
          label =  ~ name, 
          layerId = ~ id
        ) 
    }
    #Redraw search markers when zoom changes
    if(!is.null(user_stations$df) && nrow(user_stations$df) > 0) {
      leafletProxy("map") %>%
        addCircleMarkers(
          lng = user_stations$df$lng, 
          lat = user_stations$df$lat,
          label = user_stations$df$name,
          radius = radius, 
          color = "red",
          fillOpacity = 5, 
          weight = 15, 
          layerId = user_stations$df$id
        )
    }
    #Redraw size of markers depending on zoom level changing
    #This is also called everytime a user clicks or unclicks a station
    if (!is.null(selected_stations$df) && nrow(selected_stations$df) > 0) {
      #Want searched stations to be different color to differentiate from NOAA stations
      search_indx <- substr(selected_stations$df$id, 1, 6) == 'search'
      upload_indx <- substr(selected_stations$df$id, 1, 6) == 'upload'
      user_indx <- search_indx | upload_indx 
      user <- selected_stations$df[user_indx, ]
      noaa <- selected_stations$df[!user_indx, ]
      if(nrow(user) > 0) {
        leafletProxy("map") %>%
          removeMarker(user$id) %>%
          addAwesomeMarkers(
            lng = user$lng,
            lat = user$lat,
            label = user$name,
            icon = awesomeIcons(icon = "hand-pointer",
                                library = "fa",
                                markerColor="red",
                                ),
            layerId  = user$id
          )
      }
      if(nrow(noaa) > 0) {
        leafletProxy("map") %>%
          removeMarker(noaa$df$id) %>%
          addAwesomeMarkers(
            lng = noaa$lng,
            lat = noaa$lat,
            label = noaa$name,
            icon = awesomeIcons(icon = "hand-pointer",
                                library = "fa"),
            layerId  = noaa$id
          )
      }
    }
  })
  #When user clicks on a marker
  #Updates station table row, station dates, station categories 
  #Replace standard marker with selected marker
  #Everything is redrawn in function above 
  observeEvent(input$map_marker_click, {
    lat <- input$map_marker_click$lat
    lng <- input$map_marker_click$lng
    clicked_id <- input$map_marker_click$id
    if (is.null(id)) {
      warning("Marker missing id wtttff")
      return(NULL)
    }
    name <- ""
    if(substr(clicked_id, 1, 6) == "search" || 
       substr(clicked_id, 1, 6) == "upload") {
      name <- user_stations$df %>%
        filter(id == clicked_id)
    }else {
      name <- map_stations() %>%
        filter(id == clicked_id)
    }
    name <- name$name
    if (clicked_id %in% selected_stations$df$id) {
      #Unselect a station
      #Will turn back to blue circle
      selected_stations$df <-
        selected_stations$df[!selected_stations$df$id == clicked_id,]
    } else {
      if (is.null(selected_stations$df)) {
        #First marker to be clicked
        selected_stations$df <-
          data.frame(id = clicked_id, name, lng, lat)
      } else {
        #Or adding additional clicker
        selected_stations$df[nrow(selected_stations$df) + 1, 1:2] <-
          c(clicked_id, name)
        selected_stations$df[nrow(selected_stations$df), 3:4] <-
          c(lng, lat)
        #Most important thing to know about R is when you add a new row ro DF
        #If you add a vector of mixed types it will automatically convert it to one type
        #If you add a new value to df column of different type everything is automatically converted
      }
    }
  })
  #Station Names shown in Station summary
  #Displays names of stations shown on map
  output$station_names <- DT::renderDataTable({
    names <- map_stations() %>%
      select(name)
    DT::datatable(
      data=names,
      options=list(pageLength=5),
      selection='single', 
      rownames = FALSE
    )
  })
  #Set max value for number of station summaries user can fetch
  #Max value is number of stations shown on map
  observeEvent(input$station_names_rows_current, {
    rows <- input$station_names_rows_current
    range <- c(min(rows), max(rows))
    updateSliderInput(
      session,
      "fetch_num", 
      value = range,
      max = nrow(map_stations())
    )
  })
  #Fetch data in station summary
  #Fetch data for station name shown in adjacent panel
  observeEvent(input$get_station_datasets, {
    station_range <- input$fetch_num
    #If user searches for column in statoion table and click it
    #It will search it. User will have to disselect it to search range
    start <- 0
    end <- 0
    if(length(input$station_names_cell_clicked)) {
      start <- input$station_names_cell_clicked$row
      end <- start
    }else {
      start <- station_range[1]
      end <- station_range[2]
      if(start == 0 && end == 0) {
        showNotification("No stations selected", type='warning')
        return()
      }
      if(start == 0) {
        start <- 1
      }
    }
    #Get number of stations specified by fetch
    stations <- isolate(map_stations()[start:end, ])
    stations <- stations %>% 
      select(id, name)
    ss <- data.frame(station_id = character(), 
                                 uid=character(), mindate=character(), 
                                 maxdate=character(), name=character(), 
                                 datacoverge=integer(), id=character()) 
    #Fetch stations summaries and notify user of progress
    withProgress(message = "Fetching summaries...", {
      for(i in 1:nrow(stations)) {
        row <- stations[i, ]
        incProgress(1/nrow(stations), 
                    paste("Getting station", row$name))
        data <- get_datasets(row$id)
        if(is.null(data)) {
          showNotification(paste("No dataset information for", row$name), 
                           type="error")
        }else {
          ss <- rbind(ss, data)
        }
        i <- i + 1
      }
    })
    ss <- left_join(ss, 
                                 isolate(map_stations()), 
                                 by=c('station_id' = 'id'))
    if(is.null(station_summary$df))
      station_summary$df <- ss
    else 
      station_summary$df <- rbind(station_summary$df, ss)
    #Display station name, mindate, maxdate, data set name
    output$station_datasets <- DT::renderDataTable({
      table_data <- station_summary$df %>%
        select(name, mindate, maxdate, dataset_name)
      DT::datatable(
        data=table_data,
        options=list(pageLength=10), 
        rownames=FALSE,
        selection='none'
      )
    })
    #Init station data panel
    #Only display 5 selected at first
    max_size <- 5
    #Show dataset ID's from station summary
    dnames <- unique(station_summary$df$dataset_name)
    cutoff <- ifelse(length(dnames) > max_size, max_size, length(dnames))
    updateSelectInput(session,
                      "dataset_name",
                      choices = dnames,
                      selected = dnames[1:cutoff])
    #Update station id's
    snames <- unique(station_summary$df$name)
    cutoff <- ifelse(length(snames) > max_size, max_size, length(snames))
    updateSelectInput(
      session,
      "data_station_names", 
      choices = snames,
      selected = snames[1:cutoff]
    )
  })
  output$download_station_summary <- downloadHandler(
    filename = function() paste("noaa_station_summary.csv"),
    content = function(con) {
      if (!is.null(station_summary$df)) {
        write.csv(station_summary$df, con)
      }else 
        showNotification("No data to download", type="error")
    }
  )
  
  
  
  ######### STATION DATA ###########
  #TODO:  Update panel with corresponding values in station summary 
  #If x station has no global summary don't display it as option if user want's global summaries
  #Right now we are including stations in requests that may not have any data for tha dataset
  #Show corresponding stations for selected datasets 
  # observe({
  #   #Couldn't be observeEvent because it wouldnt detect last element changing
  #   if(is.null(station_summary$df)) {
  #     return()
  #   }
  #   in_dname <- isolate(input$dataset_name)
  #   in_sname <- isolate(input$data_station_names)
  #   
  #   sname_choices <- c()
  #   sname_selected <- c()
  #   dname_choices <- c()
  #   dname_selected <- c()
  #   if(is.null(in_dname)) {
  #     sname_choices <- unique(station_summary$df_name)
  #   }else {
  #     valid_station_names <- station_summary$df %>% 
  #       subset(dataset_name %in% in_dname) %>% 
  #       select(name)
  #     valid_station_names <- unique(valid_station_names$name)
  #     sname_selected <- intersect(valid_station_names, in_sname)
  #     if(is.null(selected)) 
  #       sname_selected <- in_dname
  #   }
  #   #Display appropiate datasets for selected stations 
  #   if(is.null(in_sname)) {
  #     dname_choices = unique(station_summary$df$dataset_name)
  #   }else {
  #     valid_datasets <- station_summary$df %>% 
  #       subset(name %in% in_sname) %>%
  #       select(dataset_name)
  #     valid_datasets <- unique(valid_datasets$dataset_name)
  #     dname_choices <- valid_datasets
  #     dname_selected <- intersection(valid_datasets, in_dname)
  #     if(is.null(dname_selected)) 
  #       dname_selected <- in_sname
  #   }
  # })
  #Set min and max for data sets and stations selected
  observe({
    if(is.null(station_summary$df))
      return()
    if(is.null(input$dataset_name) && 
       is.null(input$data_station_names)) {
      updateDateRangeInput(
        session,
        'daterange',
        start = Sys.Date(),
        end = Sys.Date(), 
        min = Sys.Date(), 
        max = Sys.Date()
      )
      return()
    }
    #Filter based off selected input
    df <- station_summary$df
    if(!is.null(input$dataset_name)) {
      df <- df %>%
        subset(dataset_name %in% input$dataset_name)
    }
    if(!is.null(input$data_station_names)) {
      df <- df %>% 
        subset(name %in% input$data_station_names)
    }
    #Daterange for Station Data
    #Depends on selected Dataset name and Station name
    #Display minimum and maximum date
    min <- order(df$mindate)[1]
    max <- order(df$maxdate)[length(df$maxdate)]
    updateDateRangeInput(
      session,
      "daterange",
      start = df$mindate[min],
      end = df$maxdate[max], 
      min = df$mindate[min],
      max = df$maxdate[max],
    )
  })
  #When user clicks on fetch data in station data section
  #Triggers a GET request to noaa server
  observeEvent(input$fetch_data, {
    if(is.null(station_summary$df)) {
      showNotification("Fetch station summary first to get data",
                       type="warning")
      return()
    }
    if(is.null(input$dataset_name)) {
      showNotification("Dataset ID is required", type="error")
      return()
    }
    if(is.null(input$data_station_names)) {
      showNotification("At least 1 station name must be selected",
                       type="error")
      return()
    }
    startdate = input$daterange[1]
    enddate = input$daterange[2]
    start_yr <- format(startdate, "%Y")
    end_yr <- format(enddate, "%Y") 
    if((as.integer(end_yr) - as.integer(start_yr)) > 15) {
      showNotification(
        "Cap is set at 15 years of data",
        type="error")
      return()
    }
    #Get data based off input in panel
    #Will only get data that is specified in station summary
    df <- station_summary$df %>% 
      subset(dataset_name %in% input$dataset_name) %>% 
      subset(name %in% input$data_station_names)
    if(is.null(df) || nrow(df) < 0) {
      print("DF IS EMPTYYY")
      return()
    }
    #Call back function to update progress 
    #dname = datasetname, sname = stationname 
    #Progress bar for updating user on progress 
    progress <- shiny::Progress$new() 
    on.exit(progress$close())
    update_progress <- function(limit, count, yrs) {
      progress$inc(amount = 1 / ((count/limit)*yrs)) 
    }
    data <- NULL
    for(i in 1:nrow(df)) {
      row <- df[i, ]
      progress$set(message="Fetching data", value=0,
                    detail = paste("Getting", row$dataset_name, 
                                  "for", row$name))
      fetched_data <- get_data(datasetid = row$dataset_id, 
                       daterange = input$daterange, 
                       stationid = row$station_id, 
                       update_progress)
      if(!is.null(fetched_data) && nrow(fetched_data) > 1) {
        fetched_data <- cbind(fetched_data, "dataset_id"=row$dataset_id)
        if(is.null(data))
          data <- fetched_data
        else
          data <- rbind(data, fetched_data)
      }
    }
    if(is.null(data) || nrow(data) < 1) {
      showNotification("Data request failed", type="error")
      return()
    }
    #Process data returned from fetch
    #Note: this code will change a lot when dropping station name requirement
    #I am changing the data!! 
    #Get station names cause they are more user friendly
    ids <- substr(data$station, 7, 1000000)
    names <- map_stations() %>% 
      subset(id %in% ids)
    if(nrow(names) < 1) {
      showNotification("Unknown StationID. Please try other station", 
                       type="error")
      return()
    }
    names$id <- paste('GHCND:', names$id, sep="")
    names <- rename(names, station=id, station_name=name)
    data <- merge(data, names, by='station')
    #Change date From %Y-%m-%dT%h:%m:s to yyyy/mm/dd
    data$date <- as.Date(data$date)
    #Append dataset name as well 
    dataset_names <- 
      station_summary$df %>% 
      subset(dataset_id %in% unique(data$dataset_id), 
             select=c(dataset_id, dataset_name))
    dataset_names <- dataset_names[!duplicated(dataset_names[,1]), ]
    data <- merge(data, dataset_names, by='dataset_id')
    if(is.null(noaa_dataset$df))
      noaa_dataset$df <- data
    else
      noaa_dataset$df <- rbind(noaa_dataset$df, data)
    #Update Table of Downloaded Data
    table_data <- noaa_dataset$df %>% 
      select(date, station_name, datatype, value, dataset_name)
    output$station_data <- DT::renderDataTable(
      DT::datatable(
        data=table_data,
        options=list(pageLength=10), 
        rownames=FALSE,
        selection='none'
      )
    )
    #Update EDA panel
    #User can use these parameters to make graphs in EDA section
    #At the start just display 5 selected
    max_size <- 5
    dnames <- unique(noaa_dataset$df$dataset_name)
    cutoff <- ifelse(length(dnames) > max_size, max_size, length(dnames))
    updateSelectInput(
      session,
      "eda_dataset_name",
      choices = dnames,
      selected = dnames[1:cutoff]
    )
    snames <- unique(noaa_dataset$df$station_name)
    cutoff <- ifelse(length(snames) > max_size, max_size, length(snames))
    updateSelectInput(
      session,
      "eda_station",
      choices = snames, 
      selected = snames[1:cutoff]
    )
    dtypes <- noaa_dataset$df %>% select(datatype)
    dtypes <- unique(dtypes$datatype)
    cutoff <- ifelse(length(dtypes) > max_size, max_size, length(dtypes))
    updateSelectInput(
      session,
      "datatypes", 
      choices = dtypes, 
      selected = dtypes[1:cutoff]
    )
    updateSelectInput(
      session,
      "functions", 
      selected = c("Scatter", "Density", "Histogram", "Boxplot")
    )
    #Radio button's are set to None when there is no data
    #Once we get data we will set them to group by dataset name & wrap datatype 
    updateRadioButtons(
      session, 
      "group",
      selected = "Dataset Name"
    )
    #Need to figure out way to set wrap by datatype
    #No promoise it will be called by the time plots have been updated from above update
  })
  #Download data retrieved from rnoaa 
  output$download_data <- downloadHandler(
    filename = function() paste("noaa_station_data.csv"),
    content = function(con) {
      if (!is.null(noaa_dataset$df)) {
        write.csv(noaa_dataset$df, con)
      }else 
        showNotification("No data to download", type="error")
    }
  )
  
  
  
  ########## EXPLORATORY DATA ANALYSIS ##############
  #User has own copy of downloaded data so they can manipulate it
  #The original does not change and is  available for reference
  #User can do data engineering on downloaded data from noaa
  #Filter stations available and datatypes available 
  #That is decided by what dataset's the user has selected
  user_noaa_set <- reactive({
    if(is.null(noaa_dataset$df)) 
      return(NULL)
    #First get data frame of data user has selected
    #User can set any of these to null so need to account for that
    df <- NULL
    if(!is.null(input$eda_dataset_name)) {
      df <- noaa_dataset$df %>%
        subset(dataset_name %in% input$eda_dataset_name)
    }else {
      #Need a dataset to have data
      return(NULL)
    }
    if(!is.null(input$eda_station)) {
      if(is.null(df)) {
        df <- noaa_dataset$df
      }
      df <- df %>% 
        subset(station_name %in% input$eda_station)
    }else {
      return(NULL)
    }
    if(!is.null(input$datatypes)) {
      if(is.null(df))
        df <- noaa_dataset$df
      df <- df %>% 
        subset(datatype %in% input$datatypes)
    }
    #Adjust x axis based off date range
    min <- order(df$date)[1]
    max <- order(df$date, decreasing=TRUE)[1]
    data_min <- as.Date(df$date[min])
    data_max <- df$date[max]
    daterange <- input$eda_daterange
    start <- input$eda_daterange[1]
    end <- input$eda_daterange[2]
    if(start >= data_min && start <= data_max) {
      #Adjust start date
      df <- df %>% 
        filter(date >= start)
    }
    if(end >= data_min && end <= data_max) {
      #Adjust end date
      df <- df %>% 
        filter(date <= end)
    }
    df
  })
  #Update EDA daterange min/max
  #Based off all selected input
  #Point of this is to avoid gaps in plot's
  observeEvent(user_noaa_set(), {
    #Data has fetched no data, set min max to null
    if(is.null(user_noaa_set())) {
      updateDateRangeInput(
        session, 
        "eda_daterange", 
        min=NULL, 
        max=NULL,
      )
      return()
    }
    #Cause eda_station is the names we need to convert to get station id's
    dates <- user_noaa_set()$date
    min <- order(dates)[1]
    max <- order(dates, decreasing=TRUE)[1]
    updateDateRangeInput(
      session, 
      "eda_daterange", 
      start=as.Date(dates[min]),
      end=as.Date(dates[max])
    )
  })
  #Render plot based on the parameters selected by the user
  #Use can specify multiple plots and we will make grid based off it
  #Possible functions c("Scatter", "Density", "Histogram", "Boxplot")
  observe({
    if(is.null(user_noaa_set())) {
      output$plot <- renderPlot({
        out_plots(grid.rect(gp=gpar(col="white")))
      })
      user_plots(list())
      return()
    }
    #Need to figure out what the group is 
    #Can be dataset name, station name, or datatype
    #print(user_noaa_set())
    user_group <- input$group
    if (user_group == "Dataset Name") {
      if (!is.null(isolate(input$eda_dataset_name))) {
        user_group <- "dataset_name"
      } else {
        showNotification("No datasets selected", type = "Error")
        return()
      }
    } else if (user_group == "Station Name") {
      if (!(is.null(isolate(input$eda_station)))) {
        user_group <- "station_name"
      } else {
        snowNotification("No stations selected", type = "Error")
        return()
      }
    } else if (user_group == "Datatype") {
      if (!is.null((isolate(input$datatypes)))) {
        user_group <- "datatype"
      } else {
        showNotification("No datatypes selected", type = "Error")
        return()
      }
    } else if (user_group == "None") {
      user_group <- NULL
    } else {
      print("Spelling error")
      return()
    }

    scat <- NULL
    box <- NULL
    hist <- NULL
    den <- NULL
    #TODO: Set aes in a better way w/out so many ifs
    if("Scatter" %in% input$functions) {
      if(is.null(user_group)) {
        scat <- 
          ggplot(user_noaa_set(), 
                 aes(x=date,
                     y=value)) +
          geom_point() + geom_line()
      }else {
        scat <- 
          ggplot(user_noaa_set(), 
                 aes(x=date,
                     y=value, 
                     group=!!sym(user_group), 
                     colour=!!sym(user_group))) +
          geom_point() + geom_line()
      }
    }
    if("Boxplot" %in% input$functions) {
      if(!is.null(user_group)) {
        box <-
          ggplot(user_noaa_set(),
                 aes(
                   x = user_group,
                   y = value,
                   group = !!sym(user_group),
                   colour = !!sym(user_group)
                 )) +
          geom_boxplot()
      }
    }
    if("Histogram" %in% input$functions) {
      if(is.null(user_group)) {
        hist <- 
          ggplot(user_noaa_set(), aes(x=value)) +
          geom_histogram()
      }else {
        if(is.null(user_group)) {
          hist <- 
            ggplot(user_noaa_set(), aes(x=value)) +
            geom_histogram()
        }else {
          hist <- 
            ggplot(user_noaa_set(), aes(x=value, fill=!!sym(user_group))) +
            geom_histogram()
        }
      }
    }
    if("Density" %in% input$functions) {
      if(is.null(user_group)) {
        den <- 
          ggplot(user_noaa_set(), aes(x=value)) + 
          geom_density()
      }else {
        den <- 
          ggplot(user_noaa_set(), aes(x=value, colour=!!sym(user_group))) + 
          geom_density()
      }
    }
    #Determine layout of the plots based off number of plots selected
    size <- 0
    plots <- list()
    if(!is.null(scat)) {
      size <- size + 1
      plots[[size]] <- scat
    }
    if(!is.null(box)) {
      size <- size + 1
      plots[[size]] <- box
    }
    if(!is.null(hist)) {
      size <- size + 1 
      plots[[size]] <- hist
    }
    if(!is.null(den)) {
      size <- size + 1
      plots[[size]] <- den
    }
    user_plots(plots)
  })
  #Apply facet
  #Most recently selected facet appended to end
  #Only permit one facet
  observeEvent(input$facet, {
    facet <- input$facet 
    if(facet == "None") {
      #Trigger an event to undo all of this
      og <- isolate(input$eda_dataset_name)
      updateSelectInput(session, "eda_dataset_name", selected = "")
      updateSelectInput(session, "eda_dataset_name", selected = og)
      return()
    }
    plots <- isolate(user_plots())
    if(length(plots) <= 0)
      return()
    #Apply facet
    f <- strsplit(facet, " ")[[1]]
    var <- ""
    if (f[2] == "Dataset")
      var <- "dataset_name"
    if (f[2] == "Station")
      var <- "station_name"
    if (f[2] == "Datatypes")
      var <- "datatype"
    if (f[1] == "Wrap") {
      for (i in 1:length(plots)) {
        plots[[i]] <- plots[[i]] + facet_wrap(vars(!!sym(var)))
      }
    } else {
      #Grid
      for (i in 1:length(plots)) {
        plots[[i]] <- plots[[i]] + facet_grid(vars(!!sym(var)))
      }
    }
    user_plots(plots)
  })
  #Update the plots!! Changed in above 2 functions
  observeEvent(user_plots(), {
    if(length(user_plots()) <= 0)
      return()
    output$plot <- renderPlot({
      out_plots(gridExtra::grid.arrange(grobs=user_plots()))
      out_plots()
    })
  })
  #Save the graph
  output$download_graph <- downloadHandler(
    filename = function() paste("graph.png"),
    content = function(con) {
      if (!is.null(out_plots())) {
        ggsave(con, out_plots())
      }else 
        showNotification("No data to download", type="error")
    }
  )
  #Download the graph data
  output$download_graph_data <- downloadHandler(
    filename = function() paste("graph_data.csv"),
    content = function(con) {
      if (!is.null(user_noaa_set())) {
        write.csv(user_noaa_set(), con)
      }else 
        showNotification("No data to download", type="error")
    }
  )
}




















