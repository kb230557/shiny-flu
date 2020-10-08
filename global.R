
#Loading required packages
library("shiny")
library("dplyr")
library("ggplot2")
#library("Cairo")
library("sp")
library("leaflet")
library("MMWRweek")
library("htmltools")
library("DT")
library("ggthemes")
library("plotly")
library("tidyverse")
library("magrittr")
library("shinydashboard")
library("shinyWidgets")
library("shinythemes")
library("viridis")
library("RColorBrewer")

load("flu.Rdata")


season_name = "2020-21"

year_strains =  c("2020-21" = "2020-21",
                  "2019-20" = "2019-20 (Mixed Strain Predominant)",
                  "2018-19" = "2018-19 (H1N1 Predominant)",
                  "2017-18" = "2017-18 (H3N2 Predominant)",
                  "2016-17" = "2016-17 (H3N2 Predominant)",
                  "2015-16" = "2015-16 (H1N1 Predominant)"
                  )

#Function so correct week will be automatically updated in risk assessment and map slider - retired after slider format changed to dates
# getweek <- function (mydate) {
#   if (weekdays(mydate) %in% c("Friday","Saturday")) {
#      MMWRweek(mydate)[2] - 1 
#   }
#   else { 
#     MMWRweek(mydate)[2] - 2 
#   }
# }


