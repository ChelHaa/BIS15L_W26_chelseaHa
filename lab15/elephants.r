library(tidyverse)
library(janitor)
library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  
  dashboardHeader(title="Distribution of Elephant Age and Height by Sex"),
  
  dashboardSidebar(
    
    selectInput("x",
                "Select Varibale",
                choices=c("age", "height"),
                selected="age"),
    selectInput("y",
                "Select Sex",
                choices=c("M","F"),
                selected="M")
  ),
  
  
  dashboardBody(
    plotOutput("plot", width="600px", height="500px")
  )
)

server <- function(input, output, session) {
  
  output$plot <- renderPlot({
    elephants %>% 
      filter(sex==input$y) %>%
      ggplot(aes(x=sex, y=.data[[input$x]], fill=sex))+
      geom_boxplot(alpha=0.75)+
      labs(title="Distribution of Elephant Age and Height by Sex")+
      theme_minimal()
  })
  
}
shinyApp(ui, server)
