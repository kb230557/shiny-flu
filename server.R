
#Importing data
#load("flu.Rdata")

server <- function(input, output) {
  
  #Improves graphics quality (didn't end up needing)
  #options(shiny.usecairo=T)
  
  
  #==========================================ED DATA BY SEASON (SERVER)=============================================================#
  
  
  #Subsetting data to plot based on user selected seasons
  userdatayr = reactive({
    return(fluedyr[fluedyr$Season %in% input$seasonpick, ]) #inputID is from UI script - ensures user-selected data is displayed
  })
  
  #Attempted to merge season data with baseline data so hover option would work on baseline data but code didn't work, retained for possible further troubleshooting
  # hoverdata = reactive({
  #   if(input$baselinecheck) {
  #     rbind(userdata(),testdf)
  #   }
  #   else return(userdata())
  # })
  
  #Assigning colors - used W3Schools color picker (https://www.w3schools.com/colors/colors_picker.asp) to select hues based off blue in border and logo (#6e9dc9)
  #Selected variation on red, yellow, and orange hues with higher saturation for brightness 
  # groupcolorsyr <- c("2010-11" = "#6ec9b2", "2011-12" = "#6ec96e", "2012-13" = "#6e6ec9", "2013-14" = "#dcdc5b",
  #                    "2014-15" = "#c96eb2", "2015-16" = "#e69c51", "2016-17" = "#6E9DC9", "2017-18" = "#4cebeb", "2018-19" = "#1f77b4",
  #                    "2019-20" = "#d62728",
  #                    "Average H1N1 Seasons" = "#808080", "Average H3N2 Seasons" = "#D3D3D3", "Average All Seasons" = "#101010")
  # 
  # 
  # #Assigning line type (used vector assignment for easier adjustment in the future if needed)
  # grouplinesyr <- c("2010-11" = 1, "2011-12" = 1, "2012-13" = 1, "2013-14" = 1, 
  #                   "2014-15" = 1, "2015-16" = 1, "2016-17" = 1, "2017-18" = 1, "2018-19" = 1,
  #                   "Average H1N1 Seasons" = 5, "Average H3N2 Seasons" = 5, "Average All Seasons" = 5)
  
  
  #Creating plot of user-selected data to use in the download image function 
  edyrplot <- reactive({
    
    p <- ggplot(data = userdatayr(), aes(x = Week_Start, y = ED_ILI, color = Season)) +
      geom_point(size = 3) +  
      geom_line(aes(group = Season, linetype = Season), size = 1) +
      geom_hline(yintercept = ifelse(input$baselinecheck, baseline_ess, -.1), color = "black", linetype = "F1") +
      labs(title = "Proportion of ED Visits for ILI, Suburban Cook County\n", x = "MMWR Week Starting Date", y = "% of Visits for ILI") +
      scale_y_continuous(limits = c(0,9), expand = c(0,0)) +
      scale_x_discrete(drop = F) +
      scale_linetype_manual(values = rep("solid", length(input$seasonpick))) +
      theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), legend.title = element_text(size = 14, face = "bold"), 
            legend.text = element_text(size = 12), axis.title = element_text(size = 14, face = "bold"), axis.text = element_text(size = 12),
            axis.text.x = element_text(angle = 70, hjust = 1), panel.grid = element_blank(), panel.background = element_blank(), axis.line = element_line())
    
    if(length(input$seasonpick) > 4){
      
      p + 
        facet_wrap(~Season, ncol = 2) +
        scale_color_manual(values = rep("#1f77b4", length(input$seasonpick))) 
        
        
    } else {
      
      p + 
        scale_color_manual(values = tableau_color_pal('Classic 10')(length(input$seasonpick))) 
      
    }
    
  })
  
  
  #Creating plot of user-selected data to display on the app 
  output$seasonplot <- renderPlot({
    
    edyrplot()
    
  })
  
  #Creating download image functionality
  output$downloadseason <- downloadHandler(
    filename = "ED_Data_by_Season.png",
    content = function(edyrfile){
      ggsave(edyrfile, plot = edyrplot(), device = "png", height = 3, width = 10, unit = "in")
    }
  )
  
  #Generating tooltip data for hovered-over points #######CODE CREDIT: http://www.77dev.com/2016/03/custom-interactive-csshtml-tooltips.html
  output$hover_info_season <- renderUI({
    
    if(!is.null(input$seasonpick)) { #Line added to avoid error caused by geom_hline in plot when no values are selected
      
      hover <- input$plot_hover_season
      point <- nearPoints(userdatayr(), hover, threshold = 5, maxpoints = 1, addDist = TRUE)
      if (nrow(point) == 0) return(NULL)
      
      # calculate point position INSIDE the image as percent of total dimensions
      # from left (horizontal) and from top (vertical)
      left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
      top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
      
      # calculate distance from left and bottom side of the picture in pixels
      left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
      top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
      
      # create style property fot tooltip
      # background color is set so tooltip is a bit transparent
      # z-index is set so we are sure are tooltip will be on top
      style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                      "left:", left_px + 2, "px; top:", top_px + 2, "px;")
      
      # actual tooltip created as wellPanel
      wellPanel(
        style = style,
        p(HTML(paste0("<b> Season: </b>", point$Season, "<br/>",
                      "<b> MMWR Week: </b>", point$CDC_Week, "<br/>",
                      "<b> % of ED Visits for ILI: </b>", point$ED_ILI, "<br/>")))
      )
      
    }
  })
  
  
  #==========================================ED DATA BY AGE (SERVER)=============================================================#
 
  #See code comments above
  userdataage = reactive({
    return(fluedage[fluedage$Age_Group %in% input$agepick, ]) 
  })
  
  groupcolorsage <- c("0-4" = "#1f77b4", "5-17" = "#ff7f0e", "18-44" = "#2ca02c", "45-64" = "#d62728", "65+" = "#9467bd", "All" = "#979CA1")
  
  grouplinesage <- c("0-4" = 1, "5-17" = 1, "18-44" = 1, "45-64" = 1, "65+" = 1, "All" = 5)

  #Plot for download handler
  edageplot <- reactive({
    
    ggplot(data = userdataage(), aes(x = Week_Start, y = ED_ILI, color = Age_Group)) +
      geom_point(size = 3) + 
      geom_line(aes(group = Age_Group, linetype = Age_Group), size = 1) +
      labs(title = "Proportion of ED Visits for ILI by Age Group, Suburban Cook County\n", x = "MMWR Week Starting Date", y = "% of Visits for ILI") +
      scale_color_manual(values = groupcolorsage, name = "Age Group") +
      scale_linetype_manual(values = grouplinesage, name = "Age Group") +
      scale_y_continuous(limits = c(0,25), expand = c(0,0)) +
      scale_x_discrete(drop = F)+
      theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), legend.title = element_text(size = 14, face = "bold"), 
            legend.text = element_text(size = 12), axis.title = element_text(size = 14, face = "bold"), axis.text = element_text(size = 12),
            axis.text.x = element_text(angle = 70, hjust = 1), panel.grid = element_blank(), panel.background = element_blank(), axis.line = element_line())
    
  })
  
  #Plot for app display
  output$ageplot <- renderPlot({
    
    edageplot()
    
  })
  
  output$downloadage <- downloadHandler(
    filename = "ED_Data_by_Age.png",
    content = function(edagefile){
      ggsave(edagefile, plot = edageplot(), device = "png", height = 3, width = 10, unit = "in")
    }
  )
  
  output$hover_info_age <- renderUI({
      
      hover <- input$plot_hover_age
      point <- nearPoints(userdataage(), hover, threshold = 5, maxpoints = 1, addDist = TRUE)
      if (nrow(point) == 0) return(NULL)
      
      left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
      top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
      
      left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
      top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
      
      style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                      "left:", left_px + 2, "px; top:", top_px + 2, "px;")
      wellPanel(
        style = style,
        p(HTML(paste0("<b> Age Group: </b>", point$Age_Group, "<br/>",
                      "<b> MMWR Week: </b>", point$Week_Number, "<br/>",
                      "<b> % of ED Visits for ILI: </b>", point$ED_ILI, "<br/>")))
      )
  })
   
  
  #==========================================ED MAP DATA (SERVER)=============================================================#
  
  
  #Calculating MMWR week number from sliderinput selected date then translating to matching column position in zips data file
  #tempweek <- reactive ({
  #  return(ifelse(MMWRweek(input$mapweek)[1,1] == 2019, MMWRweek(input$mapweek)[1,2] - 24, MMWRweek(input$mapweek)[1,2] + 28))
  #})
  
  tempweek <- reactive({
    grep(paste0("^Week_", MMWRweek(as.Date(input$mapweek, format("%m-%d-%y")))[1,2], "$"), colnames(zips@data))
  })
  
  
  #Subsetting data to select only spatial data and selected week for ED values
  mapdata <- reactive ({
    temp <- zips[,c(1, tempweek())]   #Subset spatial data plus selected week based on column position
    names(temp) <- gsub("_.*","",names(temp))   #Renaming selected week to non-specific "Week" for use in later functions
    return(temp)                                #Returning data frame for use in global environment
  })
  
  
  #Creating labels for map based on selected week
  labels <- reactive ({
    sprintf("<strong>%s</strong><br/>%g %%", zips$ZCTA5CE10, mapdata()$Week) %>% lapply(htmltools::HTML)
  })
  
  #Generating the base map so it doesn't need to be redrawn with each change 
  output$EDmap <- renderLeaflet({
    
    leaflet(zips) %>% addProviderTiles(providers$CartoDB.Positron) %>% setView(lng = -87.86, lat = 41.8, zoom = 10) 
    
    #NOTE: Placing all polygon layers in an observer functions results in the map intializing with no layer. However, intializing with Week 35 polygons results in 
    #Week 35 flashing between each week on animation. Still need a solution here...Empty polygon borders?
    #
    # %>% addPolygons(
    #   fillColor = ~pal(zips$Week_35),
    #   weight = 2,
    #   opacity = 1,
    #   color = "white",
    #   dashArray = "3",
    #   fillOpacity = 0.7,
    #   highlight = highlightOptions(weight = 4, color = "white", dashArray = "1", fillOpacity = 0.7, bringToFront = TRUE),
    #   label = labs,
    #   labelOptions = labelOptions(
    #     style = list("font-weight" = "normal", padding = "3px 8px"),
    #     textsize = "15px", direction = "auto")) %>%
    #   addLegend("topright", pal = pal, values = ~zips$Week_35,
    #             title = "% of ED Visits for ILI",
    #             labFormat = labelFormat(suffix = " %"),
    #             opacity = 1)
    
  })
  
  #Creating the rest of the map in observer functions so it will be re-drawn as options change
  #observe ({
    observeEvent({
      input$mapweek
      input$menu == "ED Data by Zip Code"
    },{
    
    #Creating color palette
    bins <- c(0,1,2,4,6,8,10,Inf)
    pal <- colorBin("Blues", bins = bins, na.color=NA)
    
    labs <- labels()
    
    leafletProxy("EDmap", data = mapdata()) %>% clearShapes() %>% clearControls() %>% #clearing data from previous draw of map then adding new data
      addPolygons(
        fillColor = ~pal(mapdata()$Week),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(weight = 4, color = "white", dashArray = "1", fillOpacity = 0.7, bringToFront = TRUE),
        label = labs,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px", direction = "auto")) %>%
      addLegend("topright", pal = pal, values = ~mapdata()$Week,
                title = "% of ED Visits for ILI",
                labFormat = labelFormat(suffix = " %"),
                opacity = 1)
    
  })
   
  observe ({
    
    #Creating hospital icon
    hospam <- makeAwesomeIcon(icon = "plus", library = "glyphicon", markerColor = "red", iconColor = "white")
    
    #Creating hospital layer 
    proxy <- leafletProxy("EDmap") %>% addAwesomeMarkers(data = hospitals, icon = hospam, group = "hosps", #assign a layer to a group
                                      label = ~as.character(CFNAME), labelOptions = labelOptions(
                                      style = list("font-weight" = "normal", padding = "3px 8px"),
                                      textsize = "14px", direction = "auto"))   
    
    #Turn layer on and off based on whether hospital box on UI is checked
    if (input$hosploc)  {
      proxy %>% showGroup("hosps")
    }
    else (proxy %>% hideGroup("hosps"))
    
  })
  
  
  #==========================================LAB DATA (SERVER)=============================================================#
  
  #See code comments from ED DATA BY SEASON section
  
  
  #========BAR PLOT=======#
  groupcolorslab <- c("A (H1N1)" = "#1f77b4", "A (H3N2)" = "#ff7f0e", "A (Unknown Subtype)" = "#2ca02c", "B" = "#d62728")
  
  userdatalabbar = reactive({
      return(labcount[(labcount$Subtype %in% input$labbarstrain) & (labcount$Season == "2019-20"), ]) 
  })
  
  #NOTE: Explored hover functionality for bar plots but was unsuccessful, work on a solution at later date 
  #Until hover option built in, duplicated plots as seen above are not necessary)
  
  #Plot for download handler
  labbplot <- reactive({
    
    ggplot(userdatalabbar(), aes(x = Week_Start, y = Count, fill = Subtype)) +
      geom_col(position = input$labbartype) +
      labs(title = "Number of Laboratory Specimens Positive for Influenza by Strain", x = "MMWR Week Starting Date", y = "Count") +
      scale_fill_manual(values = groupcolorslab, name = "Strain") +
      scale_y_continuous(expand = c(0,0)) +
      theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), legend.title = element_text(size = 14, face = "bold"), 
            legend.text = element_text(size = 12), axis.title = element_text(size = 14, face = "bold"), axis.text = element_text(size = 12),
            axis.text.x = element_text(angle = 70, hjust = 1), panel.grid = element_blank(), panel.background = element_blank(), axis.line = element_line())
    
    
  })
  
  #Plot for app display
  output$labbarplot <- renderPlot({
    
   labbplot()
    
  })
  
  output$downloadlabbar <- downloadHandler(
    filename = "Lab_Data_by_Strain.png",
    content = function(labbarfile){
      ggsave(labbarfile, plot = labbplot(), device = "png", height = 3, width = 10, unit = "in")
    }
  )
  
  #========LINE PLOT=======#
  groupcolorsperpos <- c("2019-20" = "#d62728", "2018-19"= "#91D1FF", "2017-18" = "#619FCC", "2016-17" = "#335E7C")

  userdatalabline = reactive({
    return(unique(labcount[labcount$Season %in% input$labpick,1:4])) 
  })
  


  #Plot for download handler
  lablplot <- reactive({
    
    ggplot(data = userdatalabline(), aes(x = Week_Start, y = Percent_Pos, color = Season)) +
      geom_point(size = 3) + 
      geom_line(aes(group = Season), size = 1) +
      labs(title = "Percent of Lab Specimens Positive for Influenza\n", x = "MMWR Week Starting Date", y = "% of Positive Specimens") +
      scale_color_manual(values = groupcolorsperpos, name = "Season") +
      scale_y_continuous(limits = c(0,45), expand = c(0,0)) +
      theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), legend.title = element_text(size = 14, face = "bold"), 
            legend.text = element_text(size = 12), axis.title = element_text(size = 14, face = "bold"), axis.text = element_text(size = 12),
            axis.text.x = element_text(angle = 70, hjust = 1), panel.grid = element_blank(), panel.background = element_blank(), axis.line = element_line())
    
  })
  
  #Plot for app display
  output$lablineplot <- renderPlot({
    
    lablplot()
    
  })
  
  output$downloadlabline <- downloadHandler(
    filename = "Lab_Data_by_Season.png",
    content = function(lablinefile){
      ggsave(lablinefile, plot = lablplot(), device = "png", height = 3, width = 10, unit = "in")
    }
  )
  
  output$hover_info_labline <- renderUI({
    
    hover <- input$plot_hover_labline
    point <- nearPoints(userdatalabline(), hover, threshold = 5, maxpoints = 1, addDist = TRUE)
    if (nrow(point) == 0) return(NULL)
    
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px + 2, "px; top:", top_px + 2, "px;")
    wellPanel(
      style = style,
      p(HTML(paste0("<b> Season: </b>", point$Season, "<br/>",
                    "<b> MMWR Week: </b>", point$Week, "<br/>",
                    "<b> % Positive: </b>", round(point$Percent_Pos, 2), "<br/>")))
    )
  })
  
  
  #==========================================ICU HOSP (SERVER)=============================================================# 

  #See code comments from ED DATA BY SEASON
  #groupcolorsicu <- c("2019-20" = "#C96E85", "2018-19"= "#376895", "2017-18" = "#6E9DC9", "2016-17" = "#b5cde3")
  groupcolorsicu <- c("2019-20" = "#d62728", "2018-19"= "#91D1FF", "2017-18" = "#619FCC", "2016-17" = "#335E7C")
  
  userdataicu = reactive({
    return(icu[(icu$Season %in% input$icuseason), ]) 
  })
  
  #See note in lab bar plot section on hover functionality
  
  #Plot for download handler
  icuprintplot <- reactive({
    
    ggplot(userdataicu(), aes(x = Week_Start, y = Count, fill = Season)) +
      geom_col(position = "dodge") +
      labs(title = "Number of Influenza-associated ICU Hospitalizations\n", x = "MMWR Week Starting Date", y = "Count") +
      scale_fill_manual(values = groupcolorsicu, name = "Season") +
      scale_y_continuous(expand = c(0,0)) +
      scale_x_discrete(drop = FALSE) +
      theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), legend.title = element_text(size = 14, face = "bold"), 
            legend.text = element_text(size = 12), axis.title = element_text(size = 14, face = "bold"), axis.text = element_text(size = 12),
            axis.text.x = element_text(angle = 70, hjust = 1), panel.grid = element_blank(), panel.background = element_blank(), axis.line = element_line())
    
    
  })
  
  #Plot for app display
  output$icuplot <- renderPlot({
    
    icuprintplot()
    
  })
  
  output$downloadicu <- downloadHandler(
    filename = "Flu_ICU_by_Season.png",
    content = function(icufile){
      ggsave(icufile, plot = icuprintplot(), device = "png", height = 3, width = 10, unit = "in")
    }
  )
  
  
  #==========================================PI DEATH (SERVER)=============================================================#   
 
  #========LINE PLOT=======#
  
  #Subsetting just smoothed data to use in plot
  pism <- pi[pi$Value_Type != "Actual", ]
  
  colorpi <- c("Epidemic Threshold" = "#1f77b4", "Baseline" = "#1f77b4", "PI Death (Smoothed)" = "#d62728")
  
  linepi <- c("Epidemic Threshold" = 1, "Baseline" = 3, "PI Death (Smoothed)" = 1)
  
  
  #Plot for download handler
  piprintplot <- reactive({
    
    ggplot(data = pism, aes(x = date_ish, y = Percent, color = Value_Type)) +
      geom_line(aes(group = Value_Type, linetype = Value_Type), size = 1) +
      labs(title = "Proportion of Deaths Associated with Pneumonia or Influenza\n", x = "Date", y = "% of Deaths due to Pneumonia/Flu") +
      scale_color_manual(values = colorpi, name = "") +
      scale_linetype_manual(values = linepi, name = "") +
      scale_y_continuous(limits = c(2,11), expand = c(0,0)) +
      scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y") +
      theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), legend.title = element_text(size = 14, face = "bold"), 
            legend.text = element_text(size = 12), axis.title = element_text(size = 14, face = "bold"), axis.text = element_text(size = 14),
            panel.grid = element_blank(), panel.background = element_blank(), axis.line = element_line(), axis.text.x = element_text(angle = 70, hjust = 1),
            strip.text.x = element_text(size = 12, face = "bold")) +
      facet_grid(. ~ Season, scales = "free_x")
    
    
  })
  
  
  #Plot for app display
  output$piplot <- renderPlot({
    
    piprintplot()
    
    
  })
  
  output$downloadpi <- downloadHandler(
    filename = "PI_Mort_Smooth.png",
    content = function(pifile){
      ggsave(pifile, plot = piprintplot(), device = "png", height = 5, width = 10, unit = "in")
    }
  )
  
  output$hover_info_pi <- renderUI({
    
    hover <- input$plot_hover_pi
    point <- nearPoints(pism, hover, threshold = 5, maxpoints = 1, addDist = TRUE)
    if (nrow(point) == 0) return(NULL)
    
    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    # calculate distance from left and bottom side of the picture in pixels
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    # create style property fot tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px + 2, "px; top:", top_px + 2, "px;")
    
    # actual tooltip created as wellPanel
    wellPanel(
      style = style,
      p(HTML(paste0("<b> Value: </b>", point$Value_Type, "<br/>",
                    "<b> Proportion: </b>", point$Percent, "<br/>")))
    )
  })
  
  #========DATA TABLE=======#

  # #Subsetting data from user inputs
  # piselect = reactive({
  #   return(pi[pi$Season == input$piyear & pi$Value_Type == "Actual", c(2,1,4)]) 
  # })
  # 
  # #Creating data table
  # output$pitable <- DT::renderDataTable({ 
  #   
  #   piselect() 
  #   
  #   })
  
  
}#Server function closure


