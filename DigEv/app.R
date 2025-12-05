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
library(tableone)

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
                            p("This app will allow you to explore different variables attached to this trial and explore their relationships and significance"),
                            p("Use the tabs above to:"),
                            tags$li("Explore distributions of individual variables"),
                            tags$li("Investigate the relationships between the variables with interactive points"),
                            tags$li("Investigate by means of P-value if each variable was significant or not.")
                          ),
                          h3("Notes on Data"),
                          tags$li("Abbreviations and units of measurement for each variable was taken by the DIG code book"),
                          tags$li("Variables of Significance includes AGE SEX, BMI etc.")
                          
                 ),
                
                  tabPanel("Age vs Treatment",
                          fluidPage(
                            h2("Placebo vs Treatment distributed by Sex"),
                            plotOutput("Result_Age"),
                            h2("Notes on Age related to this trial"),
                            tags$li("Ages between the Placebo and Treatment groups were roughly the same with a slighly and not significantly higher average age in the  "),
                            tags$li("The average age of patients in This trial were in their 60s")
                            
                          )),
                 
                          
                          
                 tabPanel("Sex vs Treatment",
                          fluidPage(
                            h2("Placebo vs Treatment distributed by Sex"),
                            plotOutput("Result_Sex"),
                            h2("Notes on Sex related to this trial"),
                            tags$li("There were significantly more males recruited to this trial then females"),
                            tags$li("Box Stratification would be useful in evening out the SEX distriubtion for more accurate results")
                            
                          )
                 ),
                 
                 tabPanel("Mortality vs Treatment",
                          fluidPage(
                            h2("Mortality by Treatment Group"),
                            plotOutput("Result_Death"),
                            h2("Notes on Mortality related to this trial"),
                            tags$li("This plot shows the proportion of patients who died in each treatment arm."),
                            tags$li("The DIG trial found no significant difference in overall mortality between placebo and digoxin.")
                          )
                 ),
                 tabPanel("Table of Baseline Characteristics",
                          fluidPage(
                            h2("Baseline Table of variables by Treatment Group"),
                            p("This Table summarizes baseline demographics and clinical characteristics stratified by treatment arm, Essentially Showing P-values for each."),
                            tags$li("Look at the P value for ecah baseline factor and compare to see if was significant in the treatment group compared to the control"),
                            tags$li("If the P-Value is <0.05 it is considered Significant in this Trial"),
                            tableOutput("Table1")
                          )
                 ),
                 
                 
                 tabPanel("Summary of the DIG Trial",
                   
                   fluidPage(h2("The Digitalis Investigation Group (DIG) Trial was a large, randomized study 
       designed to assess whether digoxin improves outcomes in patients with chronic 
       heart failure and reduced ejection fraction."),
                   
                   h3("Purpose"),
                   p("To determine whether digoxin reduced overall mortality and hospitalizations 
       for worsening heart failure compared with placebo."),
                   
                   h3("Key Findings"),
                   tags$ul(
                     tags$li("Digoxin did not reduce all-cause mortality."),
                     tags$li("Digoxin significantly reduced hospitalizations due to worsening heart failure."),
                     tags$li("Overall, digoxin improved symptoms but did not extend survival.")
                   ),
                   
                   h3("Conclusion"),
                   p("Digoxin remains useful for reducing heart failure symptoms and preventing hospital 
       admissions, but it does not prolong life. Its role today is selective and often used 
       when symptoms persist despite standard therapy.")
                 )
))

                 
              


    
   
    server <- function(input, output, session) {
      output$Result_Age<- renderPlot({
      
        
        ggplot(dig, aes(x = factor(TRTMT), y = AGE, fill = factor(TRTMT))) +
          geom_boxplot(width = 0.6) +
          labs(title = "Age Distribution by Treatment Group",
               x = "Treatment", y = "Age") +
          scale_fill_manual(values = c("skyblue", "red")) +
          theme_light() })
      
        output$Result_Sex <- renderPlot({
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
    
    output$Result_Death <- renderPlot({
      dig_plot <- dig
      dig_plot$DEATH <- factor(dig_plot$DEATH,
                               levels = c(0, 1),
                               labels = c("Alive", "Dead"))
      
      ggplot(dig_plot, aes(x = factor(TRTMT), fill = DEATH)) +
        geom_bar(position = "fill") +
        labs(title = "Proportion of Mortality by Treatment Group",
             x = "Treatment",
             y = "Proportion",
             fill = "Status") +
        scale_y_continuous(labels = scales::percent_format()) +
        theme_light()
    })
    output$Table1 <- renderTable({
      
      vars <- c("AGE", "SEX", "BMI", "KLEVEL", "CREAT", "DIABP", "SYSBP",
                 "CVD", "WHF", "DIG", "HOSP", "HOSPDAYS")
      
      factorVars <- c("SEX", "HYPERTEN", "CVD", "WHF", "DIG")
      
      
      
      tab1 <- CreateTableOne(
        vars       = vars,
        strata     = "TRTMT",
        data       = dig,
        factorVars = factorVars,
        includeNA  = TRUE
      )
      tab_mat <- print(tab1, printToggle = FALSE)
      
      df <- as.data.frame(tab_mat)
      df <- cbind(Variable = rownames(df), df)
      rownames(df) <- NULL
      
      print(df)
      
    })
    }
      
      
    
    
    

    
    
  
    
  


# Run the application 
shinyApp(ui = ui,server = server)

