
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

load("flu.Rdata")

#Function so correct week will be automatically updated in risk assessment and map slider - retired after slider format changed to dates
# getweek <- function (mydate) {
#   if (weekdays(mydate) %in% c("Friday","Saturday")) {
#      MMWRweek(mydate)[2] - 1 
#   }
#   else { 
#     MMWRweek(mydate)[2] - 2 
#   }
# }


