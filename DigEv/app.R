#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#



library(shiny)
library(ggplot2)
library(dplyr)
library(readr)



# Define UI for application that draws a histogram
ui <- navbarPage("DIG Trial Shiny Application",

    tabPanel("About the Dig Trial",
             fluidPage(
               h2("About the Dig Trial Introduction"), 
                  p("This Trial Explores the DIG or Digoxin Trial Dataset"),
                    p("Use the tabs above to:"),
              tags$ul(tags$li("Explore distributions of individual variables"),
               tags$li("Investigate the relationships between the variables with interactive points"),
               tags$li("Filter by baseline characteristics")
               ))
             ),
    tabPanel("scatterplot",
             fluidPage(h2("AGE vs HR scatterplot"),
                       plotOutput("scatterplot")
                       )
             )
    )
                    


server <- function(input,output,session) {output$scatterplot <- renderPlot({
  dig <- read.csv("DIG.csv")
 ggplot(dig, aes(x = AGE, y = HR)) + 
    geom_point(alpha = 0.5) +
    geom_smooth(method = "loess") +
    labs(
      title = "AGE vs HR",
      x = "AGE",
      y = "HR") +
    theme_light()})
}



# Run the application 
shinyApp(ui = ui,server = server)
