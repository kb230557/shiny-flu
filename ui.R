
library("shiny")
library("dplyr")
library("ggplot2")
library("Cairo")


ui <- fluidPage(
  
  ######### NOTE - Commands that access the www folder (including theme = and src = for images) do not work (something to do with folder permissions maybe?)
  
  #Brings in CSS to stylize application
  includeCSS("fluapp.css"),
  
  #Building the header 
  fluidRow(class = "header",
           column(class = "headimg", 2, align = "center", img(src="http://cookcountypublichealth.org/files/images/CCDPH_logo-full.jpg", alt="CCDPH Logo")),
           column(class = "headtitle", 10, HTML('
                                                <h1 style="font-weight: 700; font-size: 40px">Weekly Influenza Surveillance Data <span id="beta">Beta</span></h1>
                                                '))
           ),
  
  navbarPage("Menu",
    tabPanel("Home",
      fluidRow(
        column(7, h4(strong("Cook County Department of Health Weekly Influenza Surveillance"), style = "padding-bottom: 10px; padding-top: 5px"),
              HTML('<p id="risk">As of <strong>Week 40</strong>, the risk of influenza in Suburban Cook
                    County is <strong>low</strong>.</p>'),
              p("The Cook County Department of Health collects and analyzes data on local influenza activity year-round. During periods when higher
                 influenza activity is expected (from MMWR Week 40 through MMWR Week 20), this information is compiled into a weekly surveillance
                 report that is distributed to our partners in the healthcare community, schools, community groups, and the public. This application
                 is a companion to our weekly surveillance report.", align = "justify", style = "padding-bottom: 10px"),
              p("Influenza surveillance data is collected from multiple sources including emergency department (ED) visits, visits to outpatient
                 sentinel healthcare providers, laboratory tests, intensive-care unit (ICU) hospitalizations, and deaths. The goal of influenza 
                 surveillance is to determine when and where influenza activity is occuring, what influenza viruses are circulating, and the amount of 
                 severe outcomes, such as hospitalizations and deaths, that can be attributed to influenza.", align = "justify", 
                 style = "padding-bottom: 10px"),
              tags$small("The Cook County Department of Public Health would like to thank all of our surveillance partners for their help in collecting
                          this information.",style = "font-style: italic")
        ),
        column(class = "homeimg", 5, img(src="https://phil.cdc.gov/PHIL_Images/11213/11213_lores.jpg", alt="Placeholder Image", #https://cdn.pixabay.com/photo/2015/03/30/13/23/woman-698980_960_720.jpg
                                         class = "center-block"))
      )
    ),#Home Tab Panel closure
    tabPanel("ED Data",
             
#==========================================ED DATA BY SEASON (UI)=============================================================#          
  
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
        ),#ED season sidebarpanel closure
    
    #Creates the reference to the plot in the server function
       mainPanel(
          div(  #Mirroring code from hover code credit in server function
            style = "position:relative", #Mirroring code from code credit
            plotOutput("seasonplot", hover = hoverOpts("plot_hover_season", delay = 100, delayType = "debounce")), #Stores mouse data when user hovers over point
            uiOutput("hover_info_season"), #Displays values from server function to select data for hovered-over plot
            downloadButton('downloadseason', 'Download Image')
          )
        )#ED season mainpanel closure
      ),#ED season sidebarlayout closure
  
  #==========================================ED DATA BY AGE (UI)=============================================================#
      div(style = "padding-top: 50px; padding-bottom: 30px", 
  
      sidebarLayout(
    
        sidebarPanel(
      
          checkboxGroupInput(inputId = "agepick", 
                         label = "Display ED Data by Age for Suburban Cook County", 
                         choiceNames = list("0-4 year olds", "5-17 year olds","18-64 year olds", "65 years and older","All age groups"), 
                         choiceValues = list("0-4", "5-17","18-64","65+","All"),
                         selected = "All") 

        ),#ED Age sidebarpanel closure
    
        mainPanel(
          div(  
            style = "position:relative", 
            plotOutput("ageplot", hover = hoverOpts("plot_hover_age", delay = 100, delayType = "debounce")),
            uiOutput("hover_info_age"), 
            downloadButton('downloadage', 'Download Image')
          )
       )#ED Age mainpanel closure
    
      )#ED Age sidebarlayout closure
      )#ED Age panel div closure

    ),#ED TabPanel closure
    
    tabPanel("Laboratory Data"
      
    )#Lab TabPabel closure
  )#Navbarpage closure

)#UI fluidpage closure
