############## NOTE - Eventually need to break file into two scripts: server and UI. Two options for data transformation code: 1) break into a third script
# called global and Shiny will know to run it first before server and UI or 2) break into a third script unrelated to server and UI and set 
# a windows task scheduler to run it every morning at 4 or 5 AM



#Setting working directory, loading packages, and importing data
setwd("H:/Flu/Application")
lapply(list("shiny","readxl","dplyr","tidyr","stringr","ggplot2","Cairo","plotly"), require, character.only = TRUE)
flued <- read_excel('S:/Enhanced Surveillance/Flu Surveillance/2017_18/CCDPH_ILI_17-18Season.xlsx', skip=1)

#Removing empty rows and unnecessary columns from Excel
flued <- filter(flued, !is.na(flued[2]))
flued <- select(flued, -(starts_with("Sent")), -(starts_with("Kane")), -(starts_with("Winnebago")), -Month)
#Transposing data
flued <- flued[c(1,33,34,2:32,35:42)]
flued <- gather(flued, Region_Year, ED_ILI, 4:(ncol(flued)))
#Creating new variables from Excel column headers
flued$Region_Year <- str_replace(flued$Region_Year, "All Regions", "All")
flued <- separate(flued, Region_Year, c("Region","Season"), sep = " ")
#Removing spaces from column names
names(flued) <- gsub(" ", "_", names(flued))
#Creating month variable to correspond to MMWR week
flued <- mutate(flued, Month = case_when (flued$CDC_Week %in% c(52,1:4) ~ "Jan",
                                          flued$CDC_Week %in% (5:8) ~ "Feb",
                                          flued$CDC_Week %in% (9:12) ~ "Mar",
                                          flued$CDC_Week %in% (13:17) ~ "Apr",
                                          flued$CDC_Week %in% (18:21) ~ "May",
                                          flued$CDC_Week %in% (22:26) ~ "Jun",
                                          flued$CDC_Week %in% (27:30) ~ "Jul",
                                          flued$CDC_Week %in% (31:34) ~ "Aug",
                                          flued$CDC_Week %in% (35:38) ~ "Sep",
                                          flued$CDC_Week %in% (39:43) ~ "Oct",
                                          flued$CDC_Week %in% (44:47) ~ "Nov",
                                          flued$CDC_Week %in% (48:51) ~ "Feb",
                                          TRUE ~ "Unknown"
))
#Modifying display of Season variable 
flued$Season <- paste("20",flued$Season, sep="")
#Converting MMWR Week to factor and setting levels so x axis begins at the start of the flu season
flued$CDC_Week <- factor(flued$CDC_Week, levels = c(35:52, 1:34), ordered = TRUE)
#Selecting data to be displayed on the ED by Season graph
fluedyr <- filter(flued, !Season %in% c("2006-07","2007-08","2008-09","2009-10") & Region == "Sub-Cook")

#Start of the code to build the user interface
ui <- fluidPage(
  
######### NOTE - Commands that access the www folder (including theme = and src = for images) do not work (something to do with folder permissions maybe?)
      
      #Brings in CSS to stylize application
      includeCSS("fluapp.css"),
  

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
                                        emergency departments participate in ESSENCE. The graphs display the proportion of emergency room visits that
                                        were for influenza-like illness(ILI). ILI is defined as fever plus cough or sore throat.", 
                                        align = "justify", style = "padding-bottom: 10px"))
        ),
      
      #Using Shiny built-in layout for graphs and control panel
      sidebarLayout(
        
        #Building the control panel to select which seasons to display
        sidebarPanel(
          
          checkboxGroupInput(inputId = "seasonpick", #inputID used in server function to ensure user-selected data is displayed
                         label = "Display ED Data by Season for Suburban Cook County", 
                         #choices = c("2010-11", "2011-12","2012-13","2013-14","2014-15","2015-16","2016-17","2017-18") - Alternative code to use if choiceNames and choiceValues don't work (must match the way data appears in the df)
                         choiceNames = list("2010-11 (Mixed Strain Predominant)", "2011-12 (H3N2 Predominant)","2012-13 (H3N2 Predominant)",
                                            "2013-14 (H1N1 Predominant)","2014-15 (H3N2 Predominant)","2015-16 (H1N1 Predominant)",
                                            "2016-17 (H3N2 Predominant)","2017-18"), 
                         choiceValues = list("2010-11", "2011-12","2012-13","2013-14","2014-15","2015-16","2016-17","2017-18")),
          
          p("Include the 2017-2018 Baseline?*", style = "font-weight: bold"),
          
          checkboxInput(inputId = "baselinecheck", label = "2017-2018 Baseline"),
          
          tags$small("*The baseline value reflects the expected proportion of ED visits for ILI during 'non-influenza' weeks. Non-influenza weeks are defined as 
              periods of two or more consecutive weeks in which each week accounted for less than 2% of the season's total number of specimens that 
              tested positive for influenza. Data from the previous three influenza seasons are used to calculate the baseline value.",
              style = "font-style: italic; ")
       ),
      
        #Creates the reference to the plot in the server function
        mainPanel(
        plotOutput("seasonplot")
        )
      ))



#Start of code to build the graphs
server <- function(input, output) {
  
  #Improves graphics quality
  options(shiny.usecairo=T)
  
  #Selecting data to plot based on user selecter
  userdata = reactive({
    return(fluedyr[fluedyr$Season %in% input$seasonpick, ]) #inputID is from user interface - ensures user-selected data is displayed
  })
  
  #Selecting colors and line types to represent each season -- #### NOTE - Might want to play around more with these
  groupcolors <- c("2010-11" = "#2F6396", "2011-12" = "#B3AFED", "2012-13" = "#6E9DC9", "2013-14" = "#484199", 
                   "2014-15" = "#9BBFE2", "2015-16" = "#52779A", "2016-17" = "#7F79D0", "2017-18" = "#F33535")
  
  grouplines <- c("2010-11" = 5, "2011-12" = 1, "2012-13" = 4, "2013-14" = 3, 
                  "2014-15" = 1, "2015-16" = 5, "2016-17" = 4, "2017-18" = 1)
  
  #Creating plot of data from function above
  output$seasonplot <- renderPlot({
    

      ggplot(data = userdata(), aes(x = CDC_Week, y = ED_ILI, color = Season)) +
      geom_point(size = 3) + #######Consider size=4 paired with line size = 2 
      geom_line(aes(group = Season, linetype = Season), size = 1) +
      geom_hline(yintercept = ifelse(input$baselinecheck, 1.02, -.1), color = "black", linetype = "F1") +
      labs(title = "Proportion of ED Visits for ILI, Suburban Cook County", x = "MMWR Week", y = "% of Visits for ILI") +
      scale_color_manual(values = groupcolors) +
      scale_linetype_manual(values = grouplines) +
      scale_y_continuous(limits = c(0,6), expand = c(0,0)) +
      theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), legend.title = element_text(size = 14, face = "bold"), 
            legend.text = element_text(size = 12), axis.title = element_text(size = 14, face = "bold"))
  
    

  })
  

}

shinyApp(ui,server)



############################ TO DO: 

#X axis doesn't appear when only baseline us checked - add in permanent axis display?
#Add mouse over labels to plot
#Add download button
#Can improvements be made to coding the hline input?  Or x-axis displayed with turning into a factor?
#Add double axis with months




#Resources
#https://stackoverflow.com/questions/37343387/r-shiny-ggplot2-checkboxgroup-to-plot-specific-data
#https://stackoverflow.com/questions/20571306/multi-row-x-axis-labels-in-ggplot-line-chart

###Adjusting Color and LineType
# https://stackoverflow.com/questions/6075140/in-r-how-do-i-change-the-color-value-of-just-one-value-in-ggplot2s-scale-fill-b
# https://stackoverflow.com/questions/17180115/manually-setting-group-colors-for-ggplot2
# http://www.sthda.com/english/wiki/ggplot2-colors-how-to-change-colors-automatically-and-manually
# http://sape.inf.usi.ch/quick-reference/ggplot2/linetype
# http://ggplot2.tidyverse.org/reference/aes_linetype_size_shape.html


###General
# https://shiny.rstudio.com/reference/shiny/latest/
# https://www.rstudio.com/products/shiny/shiny-user-showcase/
# https://shiny.rstudio.com/articles/

###Adding Tooltips 
#http://www.77dev.com/2016/03/custom-interactive-csshtml-tooltips.html
# https://stackoverflow.com/questions/27965931/tooltip-when-you-mouseover-a-ggplot-on-shiny
# https://stackoverflow.com/questions/31538572/access-data-created-from-reactive-function-to-define-reactivevalues-in-shiny



