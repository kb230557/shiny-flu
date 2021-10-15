library(shiny)

ui <- fluidPage(
  column(8, offset = 2,
  br(), br(), br(),
  h3("This site has moved.", align = "center"),br(), 
  h3("For the Cook County Department of Public Health Weekly Influenza Surveillance App, please go here:", 
    a(href = "https://ccdphcd.shinyapps.io/influenza/", "https://ccdphcd.shinyapps.io/influenza/"),
    align = "center"
    )
  )
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)