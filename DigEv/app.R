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
                            tags$li("Explore distributions of individual variables"),
                            tags$li("Investigate the relationships between the variables with interactive points"),
                            tags$li("Filter by baseline characteristics")
                          ),
                          h3("Notes on Data"),
                          tags$li("Abbreviations and units of measurement for each variable was taken by the DIG code book"),
                          tags$li("Variables of Significance includes AGE SEX, BMI etc.")
                          
                 ),
                 
                 
                 
                          
                          
                 tabPanel("Sex vs Treatment",
                          fluidPage(
                            h2("SEX vs TRTMT Bar Chart"),
                            plotOutput("Result")
                          )
                 ))


    
   
    server <- function(input, output, session) {
      output$Result <- renderPlot({
      
          ggplot(dig, aes(x = factor(TRTMT), fill = SEX)) +
          geom_bar(position = "dodge", width = 0.5) +
          labs(title = "Distribution of SEX by Treatment",
           x = "Treatment", y = "Count") +
          theme_light() +
          scale_fill_manual(values = c("goldenrod3", "grey4","steelblue", "firebrick"))
  })
}
    


# Run the application 
shinyApp(ui = ui,server = server)

