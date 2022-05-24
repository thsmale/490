# NOAA Weather Collector and Visualizer
* **Author**: Thomas Smale, github: [thsmale](https://github.com/thsmale)
* **Major**: Computer Science
* **Year**: 2022

# Type of project
A data engineering project for getting weather data from NOAA servers. NOAA is a government agency with over 1.4 billion rows of data.


# Purpose (including intended audience)
For anyone looking to get reliable climate data. The advantage of this application is it's ability to find NOAA stations. 


# Explanation of files

* `app.r` - lists all program dependencies and a template for hosting the project on shinyapps.io
 	- run() will run the application locally. First source('app.r') then call the run() function in the console.
    - Variables: <If data, explain key variables here>
* `ui.r` - the user interface of the application. The main sections are the map, station summaries, station data, and exploratory data analysis.
* `server.r` - Handles requests from the user interface 
	- map_stations: All NOAA stations currently displayed on the map
	- selected_stations: Circles on the map the user has clicked. Mostly used for filtering NOAA stations by radius
	- station_summary: Summaries of the stations the user has fetched. This is required for getting the dataset ID's for the station. NOAA has different dataset's like Daily Summary's or Yearly Summary's which vary in it's content. Dataset ID's are the one requirement in the NOAA API for fetching climate data. 
	- noaa_dataset: Weather data fetched from NOAA padded with variable's I think the user will find helpful like the station name, dataset name, and more. 
* `weather.r` - functions that server.r calls. Some of these functions make calls to the NOAA api and other's handle Google API requests. 

# Completion status 

<as applicable> Pending steps include: 

- Find NOAA stations using a map with many parameters to make it easier 
- Get weather data spanning multiple years and multiple stations with the click of a button

## Enhancements: 

- Interpolating data missing data based on data from nearby stations
- Better error feedback to user. 

# Can someone else work on this project? 
Yes!

# Public Display/dissemination
You will need a NOAA token to run the application locally (fast and easy) which one can get [here](https://www.ncdc.noaa.gov/cdo-web/token). To deploy it one will need a Google cloud token to use the google API and a shiny account.

# License
GPL V3
