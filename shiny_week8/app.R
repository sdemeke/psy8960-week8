library(shiny)
library(tidyverse)

ui <- fluidPage(
  titlePanel("PSY8960 Week 8 App"),
  sidebarLayout(
    sidebarPanel(
    
      selectInput("gender","Select Gender",choices = c("Male","Female","All"), selected = "All"),
      selectInput("see_se","See Shaded Error Band",choices = c("Display Error Band"="TRUE", "Suppress Error Band"="FALSE"), selected = "Display Error Band" ),
      selectInput("beforeAug17","See Participants Completed Before August 1, 2017", choices = c("Yes","No"),selected="Yes"),
              
              ),
  
   mainPanel(
       plotOutput("plot"),

     
   )
  )
)

"6.	Allows the user to select whether to include or exclude participants that completed the assessment before August 1, 2017. Include by default."

server <- function(input, output) {
  
  week8_shinydata <-  read_rds("./week8_shinydata.rds") #setwd here..?
  
    output$plot <- renderPlot({ 
    week8_shinydata %>% 
      filter(if (input$gender!="All") gender==input$gender else TRUE) %>%  # filter(gender==input$gender) %>% 
      filter(if (input$beforeAug17 == "No") timeEnd >= ymd_hms("2017-08-01",truncated = 3) else TRUE) %>% 
      ggplot(aes(x=mn_q1q6,y=mn_q8q10)) +
      geom_point() +
      geom_smooth(method = "lm", formula = 'y ~ x', color="purple", se = as.logical(input$see_se)) + #need to label one as true other as false..done:)
      scale_x_continuous("Mean Scores on Q1-Q6") +
      scale_y_continuous("Mean Scores on Q8-Q10")
  })
}

shinyApp(ui = ui, server = server)


