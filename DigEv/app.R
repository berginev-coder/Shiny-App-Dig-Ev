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

dig <- read.csv(file.path(getwd(), "DIG.csv"))

dig <- dig %>%
  select(ID, TRTMT, AGE, SEX, BMI, KLEVEL, CREAT, DIABP, SYSBP,
         HYPERTEN, CVD, WHF, DIG, HOSP, HOSPDAYS, DEATH, DEATHDAY)

numeric_vars <- names(dig)[sapply(dig, is.numeric)]
factor_vars  <- names(dig)[sapply(dig, function(x) is.factor(x) || is.character(x))]

# Define UI for application that draws a histogram
ui <- navbarPage("DIG Trial Shiny Application",
                 
                 tabPanel("About the Dig Trial",
                          fluidPage(
                            h2("About the Dig Trial Introduction"), 
                            p("The Digitalis Investigation Group Trial (DIG trial) was a landmark study testing digoxin in heart failure. It found that digoxin did not reduce overall mortality but did significantly lower hospitalizations for worsening heart failure"),
                            p("This app will allow you to explore different variables attached to this trial and explore their relationships and significanct"),
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
                            plotOutput("Result"),
                            h2("Notes on Sex related to this trial"),
                            tags$li("Note there were significantly more males recruited to this trial then females"),
                            tags$li("This could")
                          )
                 ),
                 
                 tabPanel(
                   "Single variable",
                   sidebarLayout(
                     sidebarPanel(
                       selectInput(
                         "single_var",
                         "Choose a numeric variable:",
                         choices = numeric_vars,
                         selected = numeric_vars[1]
                       ),
                       sliderInput(
                         "bins",
                         "Number of bins:",
                         min = 10,
                         max = 80,
                         value = 30
                       ),
                       checkboxInput("log_y", "Log-scale y-axis", FALSE)
                     ),
                     mainPanel(
                       h3(textOutput("single_title")),
                       plotOutput("single_hist"),
                       h4("Summary statistics"),
                       tableOutput("single_summary")
                     )
                   )
                 )



    
   
    server <- function(input, output, session) {
      output$Result <- renderPlot({
      
        dig$SEX <- factor(dig$SEX,
                          levels = c(1,2),
                          labels = c("Male", "Female"))
        
          ggplot(dig, aes(x = factor(TRTMT), fill = SEX)) +
          geom_bar(position = "dodge", width = 0.5) +
          labs(title = "Distribution of SEX by Treatment",
           x = "Treatment", y = "Count") +
          theme_light() +
          scale_fill_manual(values = c("goldenrod3", "grey4"))
  })
    }
  
    
  


# Run the application 
shinyApp(ui = ui,server = server)

