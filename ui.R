

ui <- fluidPage(theme = shinytheme("flatly"),
  
  # NOTE - Functions to access the www folder (e.g. theme = and src = for images) were unsuccessful (folder permissions maybe?)
  
  #Bring in extra CSS to style application
  includeCSS("app.css"),
  
  #Add Google analytics global tracking code
  tags$head(HTML('<script async src="https://www.googletagmanager.com/gtag/js?id=UA-107917571-1"></script>')),
  tags$head(tags$script(HTML(" window.dataLayer = window.dataLayer || [];
                              function gtag(){dataLayer.push(arguments);}
                              gtag('js', new Date());
                              gtag('config', 'UA-107917571-1')"))),
  
  useShinydashboard(),
  tags$head(tags$style(HTML(".small-box {height: 120px}"))),
  
  
  #Building the header 
  fluidRow(class = "header",
           column(class = "headimg", 2, align = "center", img(class = "imggen", src="https://www.cookcountypublichealth.org/wp-content/uploads/2018/12/CookCountyLogo.png", alt="CCDPH Logo")), 
           column(class = "headtitle", 10, HTML('
                                                <h1 style="font-weight: 700; font-size: 40px">Weekly Influenza Surveillance Data</h1>
                                                '))
           ),
  #Starting nav bar
  navbarPage("Menu", id = "menu", windowTitle = "Cook County Flu Surveillance",
    tabPanel("Home",
             #Building Vertical strip of images on home page
             fluidRow(
               # column(2, style = "padding-right: 50px;",
               #        fluidRow(
               #          column(12, class = "homestrip", img(src="https://phil.cdc.gov/PHIL_Images/11213/11213_lores.jpg", class = "img-responsive imggen"))
               #        ),
               #        fluidRow(
               #          column(12, class = "homestrip", img(src="https://phil.cdc.gov/PHIL_Images/11213/11213_lores.jpg", class = "img-responsive imggen"))
               #        ),
               #        fluidRow(
               #          column(12, class = "homestrip", img(src="https://phil.cdc.gov/PHIL_Images/11213/11213_lores.jpg", class = "img-responsive imggen"))
               #        )
               # ),
               #Building home page text    
               
               #Value boxes
               column(width = 3, offset = 1, 
                      style='margin-bottom:30px;margin-top:10px;border-right:1px solid #EBEBEB; padding: 5px;',
                      
                      h4("For Week", paste0(week, ","), "ending", format(as.Date(end), "%b %d, %Y*:"), align = "center"),
                      
                      #risk level
                      valueBox(
                        str_to_title(risk_level), 
                        width = 12,
                        p("Risk Level for\nInfluenza", style = "font-size: 125%;"), 
                        icon = icon("exclamation"), 
                        color = ifelse(str_to_title(risk_level) == "Low", "olive",
                                       ifelse(str_to_title(risk_level) == "High", "red", "yellow")
                        )
                        
                      ),
                      
                      #ili
                      valueBox(
                        ili_num, 
                        width = 12,
                        p("Percent of ED Visits for Influenza-like Illness", style = "font-size: 125%;"), 
                        icon = icon("user-md"), color = "light-blue"
                        
                      ),
                      
                      #perc pos
                      valueBox(
                        lab_percent,
                        width = 12,
                        p("Percent of Lab Specimens Positive for Influenza", style = "font-size: 125%;"), 
                        icon = icon("vial"), color = "light-blue"
                        
                      ),
                      
                      #icu cases
                      valueBox(
                        icu_num_week, 
                        width = 12,
                        p("Number of Influenza ICU Cases", style = "font-size: 125%;"), 
                        icon = icon("hospital"), color = "light-blue"
                        
                      )
                      
               ),
               
               
               column(7, offset = 0, br(),
                      h4(strong("Cook County Department of Public Health Weekly Influenza Surveillance"), style = "padding-bottom: 10px; padding-top: 5px"), 
                      p(id="risk", "As of ",strong(paste("Week", week))," the risk of influenza in Suburban Cook
                            County is ", strong(paste0(toupper(risk_level), "."))),
                     #p(id="risk", "Weekly surveillance updates for the 2020-2021 influenza season have concluded. Updates for the 2021-2022 flu season will begin in October."),
                     p("The Cook County Department of Public Health collects and analyzes data on local influenza activity year-round. During periods when higher
                        influenza activity is expected (generally October to May), this information is compiled into a weekly surveillance
                       report that is distributed to our partners in the healthcare community, schools, community groups, and the public. This application
                       is a companion to our weekly surveillance report. Copies of those reports can be found",
                       a(href = "https://www.cookcountypublichealth.org/data-reports/communicable-disease-data-reports/", "here."), align = "justify", style = "padding-bottom: 10px"),
                     p("Influenza surveillance data are collected from multiple sources including emergency department (ED) visits, visits to outpatient
                       sentinel healthcare providers, laboratory tests, intensive-care unit (ICU) hospitalizations, and deaths. The goal of influenza
                       surveillance is to determine when and where influenza activity is occuring, what influenza viruses are circulating, and how severe the season is (as measured by hospitalizations and deaths).", align = "justify", style = "padding-bottom: 10px"),
                     p("It is important to note that influenza surveillance may be especially challenging during the 2020-21 season. COVID-19 and influenza present with similar symptoms. This means our typical indicators for influenza-like illness (ILI) may be less accurate in reflecting influenza activity than in previous years. Data should be interpreted with caution, especially when comparing this season to other years. In addition, it's too early to tell how behavioral changes resulting from the pandemic - including both social distancing measures and changes in health-care seeking behavior - will impact the influenza season. Surveillance data for COVID-19 is also available online ",a(href = "https://ccdphcd.shinyapps.io/covid19/", "here."), align = "justify", style = "padding-bottom: 10px"),
                     p("*Influenza surveillance data are typically aggregated by week. This app is updated on Fridays for the previous Sunday through Saturday. On all graphs, the week ending date is displayed. Ending dates are accurate for 2020 and 2021 but are approximations for all other years."),
                     p(id = "info", "For more information on influenza, please visit the Centers for Disease Control and Prevention at ",
                       a(href = "https://www.cdc.gov/flu/", "https://www.cdc.gov/flu/. "), "Information and recommendations for healthcare professionals
                       can be found ", a(href = "https://www.cdc.gov/flu/professionals/index.htm", "here.")),
                     tags$small("The Cook County Department of Public Health would like to thank all of our surveillance partners for their help in collecting
                                 this information.",style = "font-style: italic"),
                     br(), br(), br()
               ) #close home page info column
               
             ) #fluid Row closure
    ),#Home Tab Panel closure
    tabPanel("ED Data", id = "ED_Data", #ids added to potentially use in Google Analytics event tracking, may need modification for code to function
    
                      
#==========================================ED DATA BY SEASON (UI)=============================================================#          
  
  #Building the intro title and data source description
      fluidRow(
           column(4, h3(strong("Emergency Department Visits"), style = "padding-bottom: 10px; padding-top: 5px"), 
                  p("Emergency department data are extracted from the Illinois syndromic surveillance system, which is supported
                    by the CDC's National Syndromic Surveillace Program. All acute-care hospitals in Illinois report
                    emergency department data to this system. The graphs display the proportion of emergency room visits that were for influenza-like illness(ILI) among Suburban Cook County residents. 
                    ILI is defined as fever plus cough or sore throat. COVID-19-like illness is defined as fever plus cough or shortness of breath, or given a diagnosis of COVID-19.", 
                    align = "justify", style = "padding-bottom: 10px"))
      ),
  
  #Using Shiny built-in layout for graphs and control panel
      sidebarLayout(
    
    #Building the control panel to select which seasons to display
        sidebarPanel(
      
          checkboxGroupInput(inputId = "seasonpick", #inputID is used in server function to subset user-selected data 
                         label = "Display ED Data by Season for Suburban Cook County", 
                         choiceNames = c(as.character(year_strains), paste("COVID-like Illness,", season_name)),
                         choiceValues = c(names(year_strains), "COVID-like Illness"),
                         selected = c(season_name, "COVID-like Illness")),
      
          p("Include ILI Baseline?", tags$sup(HTML('&#167'), style = "font-weight: normal"), style = "font-weight: bold"),
      
          checkboxInput(inputId = "baselinecheck", label = "ILI Baseline", value = TRUE),
          
      #Adding footnotes  
          tags$div(tags$small("* Influenza surveillance data are aggregated by MMWR week. Most years have 52 weeks; however some have 53 weeks. 
                     This year is a 53-week year. Because the last week of the calendar year is epidemiologically important for flu transmission, we have 
                     graphed Week 53 of this season with Week 52 of the other flu seasons. Consequently, all data points prior to Week 52 
                     for these seasons have also been moved forward one week, i.e., Week 39 displayed as Week 40, and so on.",
                     style = "font-style: italic"), style = "padding-bottom: 10px"),
      
          tags$div(tags$small(HTML('&#167'), "The baseline value reflects the expected proportion of ED visits for ILI during 'non-influenza' weeks. Non-influenza weeks are defined as 
                 periods of two or more consecutive weeks in which each week accounted for less than 2% of the season's total number of specimens that 
                 tested positive for influenza. Data from the previous three influenza seasons are used to calculate the baseline value.",
                 style = "font-style: italic; "))
        ),#ED season sidebarpanel closure
    
    #Building main panel with plot and download functionality
       mainPanel(
          plotlyOutput("edyr_plotly")
        )#ED season mainpanel closure
      ),#ED season sidebarlayout closure
  

  #==========================================ED DATA BY AGE (UI)=============================================================#
      div(style = "padding-top: 50px; padding-bottom: 30px", 
      
      #See code comments from ED DATA BY SEASON section
      sidebarLayout(
    
        sidebarPanel(
      
          checkboxGroupInput(inputId = "agepick", 
                         label = "Display ED Data by Age for Suburban Cook County*", 
                         choiceNames = list("0-4 year olds", "5-17 year olds","18-44 year olds","45-64 year olds", "65 years and older","All age groups"), 
                         choiceValues = list("0-4", "5-17","18-44","45-64", "65+","All"),
                         selected = c("0-4", "5-17","18-44","45-64", "65+","All")),
          
          tags$small("*For the current influenza season", style = "font-style: italic; ")

        ),#ED Age sidebarpanel closure
    
        mainPanel(
          plotlyOutput("edage_plotly")
          
        )#ED Age mainpanel closure
    
       )#ED Age sidebarlayout closure
      )#ED Age panel div closure

    ),#ED TabPanel closure


#==========================================ED DATA MAP (UI)=============================================================# 
    tabPanel("ED Data by Zip Code", id = "ED_Map",
             
      #See code comments from ED DATA BY SEASON section
      sidebarLayout(
        
        sidebarPanel(width = 3,
          
           tags$head(tags$style("#EDmap{height:100vh !important;}")), #ensures map takes up as much vertical space as possible
          
           h3(strong("ED Data by Zip Code"), style = "padding-bottom: 10px"),
          
           p("Emergency department data are extracted from the Illinois syndromic surveillance system, which is supported
                    by the CDC's National Syndromic Surveillace Program. All acute-care hospitals in Illinois report
                    emergency department data to this system. The map
            displays the proportion of emergency room visits that were for influenza-like illness(ILI) by the patient's zip code of residence. 
            ILI is defined as fever plus cough or sore throat.", align = "justify", style = "padding-bottom: 10px"),
           
           sliderInput(inputId = "mapweek",
                      label = "Drag the slider to select the week of interest* or click play to see an animation of all weeks to date:",
                      min = (MMWRweek2Date(season, 35, 1) + 6), max = end, step = 7, ticks = FALSE, 
                      value = end, timeFormat = "%m-%d-%y",
                      animate = animationOptions(interval = 1200)),
          
           #checkboxInput(inputId = "hosploc", label = "Show hospital locations on map?"),
           
           tags$div(tags$small("* Week ending date is displayed.",
                               style = "font-style: italic")) #, style = "padding-bottom: 10px")
          
        ), #ED map sidebar panel closure
        
        mainPanel(    
      
          leafletOutput("EDmap"),
          br(), br()
    
        ) #ED Map main panel closure
      ) #ED Map sidebay layout closure
    ),#ED Map TabPanel closure
#==========================================LAB DATA (UI)=============================================================#
   
    tabPanel("Laboratory Data", id = "Lab",
             
      fluidRow(
        column( 4, h3(strong("Laboratory Specimen Data"), style = "padding-bottom: 10px; padding-top: 5px"), 
                             p("Laboratory testing data for influenza are submitted on a weekly basis from the following laboratories: 
                                Illinois Department of Public Health Sentinel Laboratories, NorthShore University Health System, 
                                Loyola University Medical Center, and ACL Laboratories. Laboratories submit aggregate data for all influenza tests 
                                performed; therefore, data contain results for individuals that reside outside of suburban Cook County. 
                                Tests include viral culture, RT-PCR, and rapid antigen tests.", align = "justify", style = "padding-bottom: 10px")#,
                            # p(em(paste0("Note: The total number of influenza tests performed remains significantly below average for this time of year (", current_total_tested, " total tests performed in Week ", week," compared to ", old_total_tested, " this time last year).")), align = "justify", style = "padding-bottom: 10px")
              ),
      ),
          
          #See code comments from ED DATA BY SEASON section   
          sidebarLayout(
               
             sidebarPanel(
                 
                 p("Display Laboratory Data by Influenza Strain", style = "font-weight: bold"),
               
                 radioButtons(inputId = "labbartype", 
                                    label = "Choose type of bar graph:", 
                                    choiceNames = list("Stacked ", "Side-by-Side"), 
                                    choiceValues = list("stack", "group"),
                                    selected = "stack"),
                                    #selected = "dodge"),
                 
                 checkboxGroupInput(inputId = "labbarstrain", 
                              label = "Select strains to display:", 
                              choices = list("A (H1N1)", "A (H3N2)", "A (Unknown Subtype)", "B"),
                              selected = list("A (H1N1)", "A (H3N2)", "A (Unknown Subtype)", "B"))
                              #selected = list("A (H1N1)", "A (H3N2)"))
                              
             ),#lab bar chart sidebarpanel closure
               
               
             mainPanel(
               
                   plotlyOutput("lab_strains_plotly")
                   
             )#lab bar chart mainpanel closure
          ),#lab bar chart sidebarlayout closure
      
      div(style = "padding-top: 50px; padding-bottom: 30px", 
        
          #See code comments from ED DATA BY SEASON section  
          sidebarLayout(
            
            sidebarPanel(
              
              checkboxGroupInput(inputId = "labpick", 
                                 label = "Display the Percent of Specimens Testing Positive for Influenza", 
                                 choices = names(year_strains),
                                 selected = season_name) 
              
            ),#lab line chart sidebarpanel closure
            
            mainPanel(
              
              plotlyOutput("lab_percent_plotly")
              
            )#lab line chart  mainpanel closure
            
          )#lab line chart sidebarlayout closure
      )#lab line chart  panel div closure     
    ),#Lab TabPabel closure


#==========================================ICU HOSP (UI)=============================================================#
tabPanel("ICU Hospitalizations", id = "ICU",
         
         fluidRow(
           column( 4, h3(strong("Influenza-associated ICU Hospitalizations"), style = "padding-bottom: 10px; padding-top: 5px"), 
                   p("Individuals hospitalized in an intensive care unit (ICU) with a positive laboratory test for influenza must
                     be reported to the local health department where the patient resides. The Cook County Department of Public Health
                     jurisdiction includes all of suburban Cook County, excluding Evanston, Skokie, Oak Park, and Stickney township.
                     Reported cases are aggregated by week of hospital admission.", align = "justify", style = "padding-bottom: 10px"))
                   ),
         
         #See code comments from ED DATA BY SEASON section
         sidebarLayout(
           
           sidebarPanel(
             
             checkboxGroupInput(inputId = "icuseason", 
                                label = "Display ICU Hospitalizations by Season:", 
                                choiceNames = as.character(year_strains), 
                                choiceValues = names(year_strains),
                                selected = season_name)
             
           ),#icu sidebarpanel closure
           
           mainPanel(
             
             uiOutput("icu_cases_ui")
             
           )#icu mainpanel closure
         )#icu sidebarlayout closure
      ),#icu tab closure


#==========================================PI DEATH (UI)=============================================================#

tabPanel("Mortality", id = "PI",
         
         fluidRow(
           column( 4, h3(strong("Pneumonia-Influenza-COVID-19 Mortality"), style = "padding-bottom: 10px; padding-top: 5px"), 
                   p("Mortality data are reported for all deaths that occur in Cook County, including Chicago. Deaths reported with pneumonia, influenza, and/or COVID-19 listed as the immediate cause of death or a contributing factor are considered associated with
                     pneumonia/influenza/COVID-19 (PIC).", align = "justify", style = "padding-bottom: 5px"))
                   ),
         
         #See code comments from ED DATA BY SEASON section
         sidebarLayout(
           
           sidebarPanel(
             
             p("The smoothed proportion of deaths associated with PIC (3-week running median) is displayed
               in the graph. Mortality data lag one week behind other flu surveillance indicators.", align = "justify", style = "padding-bottom: 5px"),
             
             p("Baseline and epidemic threshold values for PIC mortality are calculated based on the previous four
               years of data using a CDC robust regression model. PIC mortality exceeding the epidemic threshold indicates the
               observed proportion of deaths is significantly higher than would be expected for that time of year, in the absence
               of substantial COVID-19 or influenza-related mortality.", align = "justify", style = "padding-bottom: 10px") 
             
             ),#pi sidebarpanel closure 1
           
           mainPanel(
             
             plotlyOutput("pic_plot", height = 400)
             
            )#pi mainpanel closure 1
           ) #,#pi sidebarlayout closure 1

       #   div(style = "padding-top: 50px; padding-bottom: 30px",
       # 
       #       #See code comments from ED DATA BY SEASON section
       #       sidebarLayout(
       # 
       #         sidebarPanel(
       # 
       #          p("Select a season to view raw (unsmoothed) P/I mortality data in the table.", align = "justify", style = "padding-bottom: 10px"),
       # 
       #           selectInput("piyear","Season:", choices = list("2013-14","2014-15","2015-16","2016-17","2017-18", "2018-19"), selected = "2018-19")
       # 
       #         ),#pi sidebarpanel closure 2
       # 
       #         mainPanel(
       #           DT::dataTableOutput('pitable', width = '75%')
       #         )#pi mainpanel closure 2
       #       )#pi sidebarlayout closure 2
       # ) #pi div closure
    )#pi tab closure

  )#Navbarpage closure

#Adding GA event tracker to count tab hits and downloads -- NOT TRACKING, NEED MORE WORK HERE
# tags$script(HTML(
#   "
#   $(document).on('shiny:filedown', function(event) {
#   gtag('event', 'download', {'event.name': event.name,'href': href});
#   });
#   
#   $(document).on('click', '#ED_Data', function() {
#   gtag('event', 'tabclick', {'event.name': 'ED_Data'});
#   });
#   
#   $(document).on('click', '#ED_Map', function() {
#   gtag('event', 'tabclick', {'event.name': 'ED_Map'});
#   });
#   
#   $(document).on('click', '#Lab', function() {
#   gtag('event', 'tabclick', {'event.name': 'Lab'});
#   });
#   
#   $(document).on('click', '#ICU', function() {
#   gtag('event', 'tabclick', {'event.name': 'ICU'});
#   });
#   
#   "
# ))

)#UI fluidpage closure
