source("weather.R")
## ----------------------------------------------------------------------------------------
#User Interface 
ui <- fluidPage(
  titlePanel("NOAA Weather Collector"),
  #Lookup loactions
  fluidRow(column(
    9,
    textInput(
      inputId = "search_input",
      label = "Location lookup",
      value = "",
      placeholder = "Lookup an address, city, or place", 
      width = "100%"
    )
  ),
  column(
    1,
    #tags$head(tags$script(src = "message-handler.js")),
    div(
      style="margin-top:25px;",
      actionButton(
        inputId = "search_button",
        icon(name = "search-location")
      )
    )
  ),
  column(
    2,
    fileInput("upload_locations", 
              label = "Upload Locations",
              accept = '.csv', 
              multiple=TRUE),
  )), 
  #Select state or change radius
  sidebarLayout(position = "left",
                sidebarPanel(
                  fluidRow(
                    helpText("Select a state to display weather stations"),
                    selectInput(
                      "state_picker",
                      label = "States",
                      choices = states,
                      selected = "California",
                      multiple = TRUE
                    )
                  ),
                  fluidRow(
                    helpText("Filter by county"), 
                    selectInput(
                      "county_picker", 
                      label = "County", 
                      multiple = TRUE, 
                      choices = ""
                    )
                  ),
                  #Negative one disables filtering by radius
                  fluidRow(
                    helpText("Click on stations to view nearby stations"),
                    sliderInput(
                      "change_radius",
                      "Radius (miles):",
                      min = -1,
                      max = 200,
                      value = -1
                    )
                  )
                ),
                mainPanel(leafletOutput("map"))), 
  
  
  
  ################# SUMMARY STATIONS ###################
  #Display basic information about station
  h3("Station Summary"),
  sidebarLayout(position="left", 
                sidebarPanel(
                  h4("Station names"), 
                  helpText("Reflects stations displayed on map"),
                  br(),
                  DT::dataTableOutput("station_names"),
                  sliderInput(
                    "fetch_num",
                    "Fetch Station Range:",
                    min = 1,
                    max = 25,
                    value = c(1, 10)
                  ),
                  fluidRow(
                    actionButton("get_station_datasets", label="Fetch"),
                    downloadButton("download_station_summary", "Download")
                  ),
                ),
                mainPanel(
                  h4("Station datasets"), 
                  DT::dataTableOutput("station_datasets")
                )
  ), 
  
  
  
  ############ STATION DATA ###############
  h3("Station Data"),
  sidebarLayout(
    position = "left", 
    sidebarPanel(
      helpText("Required", style="color:red;"),
      selectInput(
        "dataset_name", 
        label="Dataset Name", 
        choices = "", 
        multiple=TRUE
      ), 
      #TODO: Drop requirement for station name
      helpText("Required", style="color:red;"),
      selectInput(
        "data_station_names", 
        label="Station Name",
        choices = "",
        multiple=TRUE
      ),
      helpText("Required", style="color:red;"),
      dateRangeInput(
        'daterange',
        label = 'Data range',
        format = "yyyy/mm/dd",
        startview = 'year', 
        start = Sys.Date(),
        end = Sys.Date(),
        min = Sys.Date(),
        max = Sys.Date()
      ),
      fluidRow(
        actionButton('fetch_data', label='Fetch'),
        downloadButton('download_data', label = "Download"), 
      ),
    ), 
    mainPanel(
      DT::dataTableOutput("station_data")
    )
  ), 
  
  
  
  h3("Exploratory Data Analysis"), 
  sidebarLayout(
    position = "left", 
    sidebarPanel(
      selectInput(
        "eda_dataset_name", 
        label="Dataset Name", 
        choices = "", 
        multiple = TRUE
      ), 
      selectInput(
        "eda_station", 
        label="Station Name",
        choices = "",
        multiple=TRUE
      ),
      selectInput(
        "datatypes", 
        label="Datatype", 
        choices = "", 
        multiple=TRUE
      ), 
      dateRangeInput(
        'eda_daterange',
        label = 'Data range',
        format = "yyyy/mm/dd",
        startview = 'year'
      ),
      selectInput(
        "functions", 
        label="Functions", 
        choices = c("Scatter", "Density", "Histogram", "Boxplot"), 
        multiple=TRUE
      ),
      radioButtons(
        "group",
        "Select Group", 
        choices = c(
          "None",
          "Dataset Name",
          "Station Name", 
          "Datatype"
        ), 
        selected = "None"
      ),
      radioButtons(
        "facet",
        "Select Facet",
        choices = c(
          "None",
          "Wrap Dataset Names",
          "Wrap Station Names",
          "Wrap Datatypes",
          "Grid Dataset Names",
          "Grid Station Names",
          "Grid Datatypes"
        ),
        selected = "None"
      ), 
      fluidRow(
        downloadButton("download_graph", "Download Graph"),
        downloadButton("download_graph_data", "Download Graph Data")
      ),
    ), 
    mainPanel(
      plotOutput("plot")
    )
  )
)