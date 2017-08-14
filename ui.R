
library("shiny")
library("dplyr")
library("ggplot2")
library("Cairo")


ui <- fluidPage(
  
  ######### NOTE - Commands that access the www folder (including theme = and src = for images) do not work (something to do with folder permissions maybe?)
  
  #Brings in CSS to stylize application
  includeCSS("fluapp.css"),
  
  #==========================================ED DATA BY SEASON (UI)=============================================================#
  
  #Building the header ######### NOTE - to repeat on all pages
  fluidRow(class = "header",
           column(class = "headimg", 2, align = "center", img(src="http://cookcountypublichealth.org/files/images/CCDPH_logo-full.jpg", alt="CCDPH Logo")),
           column(class = "headtitle", 10, HTML('
                                                <h1 style="font-weight: 700; font-size: 40px">Weekly Influenza Surveillance Data <span id="beta">Beta</span></h1>
                                                '))
           ),
  
  #Building the intro title and data source description
  fluidRow(class = "EDhero",
           column(class = "EDheader", 4, h3(strong("Emergency Department Visits"), style = "padding-bottom: 10px; padding-top: 5px"), 
                  p("Emergency department data are extracted from the CCDPH syndromic surveillance system, ESSENCE. Forty-five hospital
                    emergency departments participate in ESSENCE, including all hospitals located in Suburban Cook County. The graphs 
                    display the proportion of emergency room visits that were for influenza-like illness(ILI) among Suburban Cook County residents. 
                    ILI is defined as fever plus cough or sore throat.", 
                    align = "justify", style = "padding-bottom: 10px"))
  ),
  
  #Using Shiny built-in layout for graphs and control panel
  sidebarLayout(
    
    #Building the control panel to select which seasons to display
    sidebarPanel(
      
      checkboxGroupInput(inputId = "seasonpick", #inputID used in server function to ensure user-selected data is displayed
                         label = "Display ED Data by Season for Suburban Cook County", 
                         choiceNames = list("2010-11 (Mixed Strain Predominant)", "2011-12 (H3N2 Predominant)","2012-13 (H3N2 Predominant)",
                                            "2013-14 (H1N1 Predominant)","2014-15 (H3N2 Predominant)","2015-16 (H1N1 Predominant)",
                                            "2016-17 (H3N2 Predominant)","2017-18"), 
                         choiceValues = list("2010-11", "2011-12","2012-13","2013-14","2014-15","2015-16","2016-17","2017-18"),
                         selected = "2017-18"),
      
      p("Include the 2017-2018 Baseline?*", style = "font-weight: bold"),
      
      checkboxInput(inputId = "baselinecheck", label = "2017-2018 Baseline"),
      
      tags$small("*The baseline value reflects the expected proportion of ED visits for ILI during 'non-influenza' weeks. Non-influenza weeks are defined as 
                 periods of two or more consecutive weeks in which each week accounted for less than 2% of the season's total number of specimens that 
                 tested positive for influenza. Data from the previous three influenza seasons are used to calculate the baseline value.",
                 style = "font-style: italic; ")
    ),#1st sidebarpanel closure
    
    #Creates the reference to the plot in the server function
    mainPanel(
      div(  #Mirroring code from hover code credit in server function
        style = "position:relative", #Mirroring code from code credit
        plotOutput("seasonplot", hover = hoverOpts("plot_hover_season", delay = 100, delayType = "debounce")), #Stores mouse data when user hovers over point
        uiOutput("hover_info_season"), #Displays values from server function to select data for hovered-over plot
        downloadButton('downloadseason', 'Download Image')
      )
    )#1st mainpanel closure
  ),#1st sidebarlayout closure
  
  #==========================================ED DATA BY AGE (UI)=============================================================#
  div(style = "padding-top: 50px; padding-bottom: 30px", 
  
  sidebarLayout(
    
    sidebarPanel(
      
      checkboxGroupInput(inputId = "agepick", 
                         label = "Display ED Data by Age for Suburban Cook County", 
                         choiceNames = list("0-4 year olds", "5-17 year olds","18-64 year olds", "65 years and older","All age groups"), 
                         choiceValues = list("0-4", "5-17","18-64","65+","All"),
                         selected = "All") 

    ),#2nd sidebarpanel closure
    
    mainPanel(
      div(  
        style = "position:relative", 
        plotOutput("ageplot", hover = hoverOpts("plot_hover_age", delay = 100, delayType = "debounce")),
        uiOutput("hover_info_age"), 
        downloadButton('downloadage', 'Download Image')
      )
    )#2nd mainpanel closure
    
  )#2nd sidebarlayout closure
  )#div closure
)#UI fluidpage closure
