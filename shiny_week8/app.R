
#Shiny app for PSY8960 Week 8

library(shiny)
library(tidyverse) 

ui <- fluidPage(

    # Application title
     titlePanel("PSY8960 Week 8 Shiny App - Demeke"),

    # Sidebar with a series of selection inputs  
    sidebarLayout(
        sidebarPanel(
          #first selection input takes in user choice for gender. Default all cases shown
          selectInput("gender","Select Gender",choices = c("Male","Female","All"), selected = "All"),
          #second selection input asks whether user wants to see shaded error band around regression line
          #labels are also recoded as "TRUE" and "FALSE" for easier use within renderPlot in server, see below. Defauls band is shown
          selectInput("see_se","See Shaded Error Band",choices = c("Display Error Band"="TRUE", "Suppress Error Band"="FALSE"), selected = "Display Error Band" ),
          #final selection input asks whether user wants to restrict cases completed after set date. Default all cases shown
          selectInput("beforeAug17","See Participants Completed Before August 1, 2017", choices = c("Yes","No"),selected="Yes"),
          
        ),

        # Show the generated scatterplot based on user inputs or with default settings
        mainPanel(
           plotOutput("scatterPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  #read in saved RDS for plot
  week8_shinydata <-  read_rds("./week8_shinydata.rds") #check wd
  
  #save POSITCX variable of Aug 1 2017 date
  checkAug17 <- ymd_hms("2017-08-01",truncated = 3)
  
  
    output$scatterPlot <- renderPlot({
     
       week8_shinydata %>% 
        #filter data based on selected gender. Default/'All' choice results in everything selected
        filter(if (input$gender!="All") gender==input$gender else TRUE) %>%  
        #filter data based on Aug2017 selection. Default/'No' choice results in everything selected
        filter(if (input$beforeAug17 == "No") timeEnd >= checkAug17 else TRUE) %>% 
        ggplot(aes(x=mn_q1q6,y=mn_q8q10)) +
        geom_point() +
        #se argument in geom_smooth() takes in coerced logical value of input$see_se which are recoded as TRUE and FALSE
        geom_smooth(method = "lm", formula = 'y ~ x', color="purple", se = as.logical(input$see_se)) + 
        scale_x_continuous("Mean Scores on Q1-Q6") +
        scale_y_continuous("Mean Scores on Q8-Q10")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)


