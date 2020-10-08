
#Importing data
#load("flu.Rdata")

server <- function(input, output) {
  
  #Improves graphics quality (didn't end up needing)
  #options(shiny.usecairo=T)
  
  
  #color pal
  seasons_color_pal = reactive({
    
    seasons = unique(labcount$Season) %>% sort()
    pal = c(brewer.pal(11, "RdBu")[7:11], brewer.pal(11, "RdBu")[2], brewer.pal(9, "Oranges")[6], "black")
    names(pal) = c(seasons, "COVID-like Illness", "All")

    return(pal)
    
  })
  
  #==========================================ED DATA BY SEASON (SERVER)=============================================================#
  
  
  #Subsetting data to plot based on user selected seasons
  userdatayr = reactive({

    fluedyr %>%
      filter(Season %in% input$seasonpick) %>%
      group_by(Season) %>%
      complete(Week_Start) %>%
      mutate(ED_ILI = round(ED_ILI, 2))
    
  })
  
  #plotly
  output$edyr_plotly = renderPlotly({
    
    #Set color pal
    #pal = tableau_color_pal()(length(unique(fluedyr$Season)))
    #names(pal) = unique(fluedyr$Season)
    pal = seasons_color_pal()
    
    #Set axis mins and maxs
    minx = min(userdatayr()$Week_Start)
    maxx = max(userdatayr()$Week_Start)
    miny = 0
    maxy = ceiling(max(userdatayr()$ED_ILI, na.rm = T))
    
    #Legend title annotation
    legendtitle = list(yref='paper',xref="paper",y=1.05,x=1.09, text="<b>Season</b>",showarrow=F)
    
    #plot
    plot = plot_ly(data = userdatayr(), x = ~Week_Start, y = ~ED_ILI, color = ~Season, 
            colors = pal, hovertemplate = paste('%{y}'),
            line = list(width = 3),
            type = "scatter", mode = "lines") %>%
      layout(hovermode = "compare",
             xaxis = list(title = "Week", showgrid = F, range = c(minx, maxx), showline = T),
             yaxis = list(title = "% of Visits", showgrid = F, range = c(miny, maxy), showline = T),
             title = "Proportion of ED Visits for ILI, Suburban Cook County",
             annotations=legendtitle,
             margin = list(r=25,l=50,t=50,b=0),
             showlegend = T
             
             )
    
    #add baseline check if selected
    if(input$baselinecheck){
      plot %<>%
        add_segments(x = minx, xend = maxx, y = baseline_ess, yend = baseline_ess, type = "line",
                     color = "baseline", line = list(color = "black", dash = "dot", width = 1), 
                     name = "ILI Baseline")
    }
    
    #plot
    plot

    
  })
  
  
  
  #==========================================ED DATA BY AGE (SERVER)=============================================================#
 
  #Filter data to selected
  userdataage = reactive({

    fluedage %>%
      filter(Age_Group %in% input$agepick) %>%
      group_by(Age_Group) %>%
      complete(Week_Start) %>%
      mutate(ED_ILI = round(ED_ILI, 2)) %>%
      ungroup() %>%
      mutate(Age_Group = factor(Age_Group, 
                                levels = c("0-4", "5-17", "18-44", "45-64", "65+", "All")
                                )
             )
  })
  
  
  
  output$edage_plotly = renderPlotly({
    
    #Set color pallette and line types
    pal = c(tableau_color_pal(palette = "Tableau 20")(5), "#7f7f7f")
    names(pal) = unique(fluedage$Age_Group)
    #pal = c("0-4" = "#1f77b4", "5-17" = "#ff7f0e", "18-44" = "#2ca02c", "45-64" = "#d62728", "65+" = "#9467bd", "All" = "#979CA1") #old pal
    
    
    lines = c(rep("solid",5), "dash")
    names(lines) = unique(fluedage$Age_Group)
    
    
    #Set min and max axis values
    minx = min(userdataage()$Week_Start)
    maxx = max(userdataage()$Week_Start)
    miny = 0
    maxy = ceiling(max(userdataage()$ED_ILI, na.rm = T))
    
    #Legend Titile
    legendtitle = list(yref='paper',xref="paper",y=1.05,x=1.11, text="<b>Age Group</b>",showarrow=F)
    
    #plot
    plot_ly(data = userdataage(), x = ~Week_Start, y = ~ED_ILI, color = ~Age_Group, 
                   colors = pal, hovertemplate = paste('%{y}'), linetype = ~Age_Group,
                   linetypes = lines,
                   line = list(width = 3),
                   type = "scatter", mode = "lines") %>%
      layout(hovermode = "compare",
             xaxis = list(title = "Week", showgrid = F, range = c(minx, maxx), showline = T),
             yaxis = list(title = "% of Visits for ILI", showgrid = F, range = c(miny, maxy), showline = T),
             title = "Proportion of ED Visits for ILI by Age Group, Suburban Cook County",
             annotations=legendtitle,
             margin = list(r=0,l=50,t=50,b=0),
             showlegend = T
             
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
    sprintf("<strong>%s</strong><br/>%g %%", zips$ZCTA5CE10, round(mapdata()$Week, 2)) %>% lapply(htmltools::HTML)
  })
  
  #Generating the base map so it doesn't need to be redrawn with each change 
  output$EDmap <- renderLeaflet({
    
    leaflet(zips, options = leafletOptions(minZoom = 8, zoomSnap = .25)) %>% addProviderTiles(providers$CartoDB.Positron) %>% setView(lng = -87.86, lat = 41.8, zoom = 9.75) 
    
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
    pal <- colorBin("RdYlBu", bins = bins, na.color=NA, reverse = T)
    
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
    # if (input$hosploc)  {
    #   proxy %>% showGroup("hosps")
    # }
    # else (proxy %>% hideGroup("hosps"))
    
    proxy %>% hideGroup("hosps") #delete if put hosp locations option back in
    
  })
  
  
  #==========================================LAB DATA (SERVER)=============================================================#
  
  #========BAR PLOT=======#
  
  #subset data
  userdatalabbar = reactive({

    labcount %>%
      filter(Subtype %in% input$labbarstrain,
             Season == season_name
             ) %>%
      group_by(Subtype) %>%
      complete(Week_Start) %>%
      ungroup() %>%
      mutate(Subtype = factor(Subtype, levels = c("A (H1N1)", "A (H3N2)", "A (Unknown Subtype)", "B"), ordered = T))
    
  })
  
  #bar chart
  output$lab_strains_plotly = renderPlotly({
    
    #set color pal
    pal = c("A (H1N1)" = "#1f77b4", "A (H3N2)" = "#ff7f0e", 
            "A (Unknown Subtype)" = "#2ca02c", "B" = "#d62728")
    
    #set min and max axis values
    minx = min(userdatalabbar()$Week_Start)
    maxx = max(userdatalabbar()$Week_Start)
    miny = 0
    maxy = userdatalabbar() %>%
      group_by(Week_Start) %>%
      summarise(count = sum(Count)) %>%
      pull(count) %>%
      max(na.rm = T) %>%
      add(1) %>%
      max(., 5)
    
    #Legend Titile
    legendtitle = list(yref='paper',xref="paper",y=1.05,x=1.097, text="<b>Strain</b>",showarrow=F)
    
    #plot
    plot_ly(userdatalabbar(), x=~Week_Start, y=~Count, type = "bar", color = ~Subtype,
            colors = pal, hovertemplate = paste('%{y}')) %>%
      layout(barmode = input$labbartype,
             hovermode = "compare",
             annotations = legendtitle,
             xaxis = list(title = "Week", showgrid = F, showline = T, range = c(minx, maxx)),
             yaxis = list(title = "Count", showgrid = F, showline = T, range = c(miny, maxy)),
             title = "Number of Laboratory Specimens Positive for Influenza by Strain",
             margin = list(t = 50),
             showlegend = T
             )

    
    
  })
  
  
  
  #========LINE PLOT=======#
  groupcolorsperpos <- c("2019-20" = "#d62728", "2018-19"= "#91D1FF", "2017-18" = "#619FCC", "2016-17" = "#335E7C")

  userdatalabline = reactive({
    labcount %>%
      filter(Season %in% input$labpick) %>%
      select(Season, Week_Start, Percent_Pos) %>%
      unique() %>%
      group_by(Season) %>%
      complete(Week_Start)
  })
  
  
  #plotly
  output$lab_percent_plotly = renderPlotly({
    
    #Set color pal
    #pal = tableau_color_pal(direction = -1)(length(unique(labcount$Season)))
    #pal = c(viridis(5, end = 0.85, direction = -1), "red")
    #names(pal) = unique(labcount$Season)
 
    pal = seasons_color_pal()

    #Set axis mins and maxs
    minx = min(userdatalabline()$Week_Start)
    maxx = max(userdatalabline()$Week_Start)
    miny = 0
    maxy = ceiling(max(userdatalabline()$Percent_Pos, na.rm = T)) + 2
    
    #Legend title annotation
    legendtitle = list(yref='paper',xref="paper",y=1.05,x=1.08, text="<b>Season</b>",showarrow=F)
    
    #plot
    plot_ly(data = userdatalabline(), x = ~Week_Start, y = ~Percent_Pos, color = ~Season, 
                   colors = pal, hovertemplate = paste('%{y}'),
                   line = list(width = 3),
                   type = "scatter", mode = "lines") %>%
      layout(hovermode = "compare",
             xaxis = list(title = "Week", showgrid = F, range = c(minx, maxx), showline = T),
             yaxis = list(title = "% of Positive Specimens", showgrid = F, range = c(miny, maxy), showline = T),
             title = "Percent of Lab Specimens Positive for Influenza",
             annotations=legendtitle,
             margin = list(r=25,l=50,t=50,b=0),
             showlegend = T
             
      )
    
  })
  


  
  
  #==========================================ICU HOSP (SERVER)=============================================================# 
  #number of rows for plot
  plot_rows = reactive({
    ceiling(length(input$icuseason)/2)
    
  }) 
  
  
  #cases by year bar/line plots
  output$icu_cases_plot <- renderPlotly({
    #cases_plot
    
    plot_years = input$icuseason
    plot_data = final_icu %>%
      filter(season %in% plot_years) %>%
      rename(Week = weekStarts,
             Cases = n
             ) %>%
      group_by(season) %>%
      complete(Week, fill = list(n = NA)) %>%
      mutate(total = sum(Cases, na.rm = T),
             label = paste0(season, " (Total = ", total, ")")
             )
    
    #plot all other years a single blue color and current year as red
    colors = seasons_color_pal()[plot_years]
    colors[names(colors) != season_name] = seasons_color_pal()[4]
    
    p<- ggplot(data=plot_data, aes(x=Week, y=Cases,group=season, fill =season)) + #, fill =season -- add if want dif colors
      geom_bar(stat = "identity")+
      #scale_fill_tableau()+
      scale_fill_manual(values = colors) +
      expand_limits(y = 4) +
      facet_wrap(~label, nrow = plot_rows()) +
      theme(axis.text.x = element_text(angle = 90,color=rep(c("black","transparent"),11),
                                       size = 8),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            #panel.grid.minor.y = element_blank(),
            #panel.grid.major.y = element_blank(),
            panel.background = element_rect(fill = "white", colour = "grey50"),
            panel.grid.major = element_line(colour = "grey80"),
            plot.margin = margin(t=10,r=10,b=10,l=50),
            panel.spacing = unit(0.5, "lines")
      ) +
      xlab("") +
      ylab("Influenza ICU Cases") 
    
    ggplotly(p,  tooltip=c("x", "y")) %>%
      layout(showlegend = FALSE,
             hovermode = "compare") 
    
  })
  
  output$icu_cases_ui <- renderUI({
    plotlyOutput("icu_cases_plot", height = (plot_rows()*250))
  })
  
  
  #==========================================PI DEATH (SERVER)=============================================================#   
 
  #========LINE PLOT=======#
  
  
  output$pic_plot = renderPlotly({
    
    #Subsetting just smoothed data to use in plot
    pism <- pic_clean %>%
      filter(Value_Type != "Actual") %>%
      mutate(Percent = round(Percent,2)) %>%
      rename(Date = `date_ish`) %>%
      mutate(Date = Date + 6)
    
    
    colorpi <- c("Epidemic Threshold" = "#4e79a7", "Baseline" = "#4e79a7", "PI Death (Smoothed)" = "#d62728")
    linepi <- c("Epidemic Threshold" = 1, "Baseline" = 3, "PI Death (Smoothed)" = 1)
    maxp = max(pism$Percent, na.rm = T) %>% add(1) %>% ceiling()
    
    pip = ggplot(data = pism, aes(x = Date, y = Percent, color = Value_Type, 
                                  text = paste0("</br>",
                                                format(Date, "%b %d, %Y"), "</br>",
                                                Value_Type, ": ", Percent, "%")
    )) +
      geom_line(aes(group = Value_Type, linetype = Value_Type), size = 1) +
      labs(title = "Proportion of Deaths Associated\n with Pneumonia, Influenza, or COVID-19\n", x = "", y = "% of Deaths due to\nPneumonia/Flu/COVID") +
      scale_color_manual(values = colorpi, name = "") +
      scale_linetype_manual(values = linepi, name = "") +
      scale_y_continuous(limits = c(2,maxp), expand = c(0,0)) +
      scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y") +
      theme(plot.title = element_text( hjust = 0.5),
            panel.grid = element_blank(), 
            panel.background = element_blank(),
            axis.line = element_line(),
            axis.text.x = element_text(angle = 70, hjust = 1)

      ) +
      facet_grid(. ~ Season, scales = "free_x")
    
    ggplotly(pip, tooltip = c("text")) %>%
      layout(hovermode = "compare",
             margin = list(r=100,l=100,t=125,b=50)
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


