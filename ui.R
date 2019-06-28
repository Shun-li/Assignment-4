library(shiny)
library(DT)

# Define UI for application that draws a histogram
ui<- shinyUI(fluidPage(
  
  # Application title
  titlePanel(h2("Assignment 4")),
  h3("Shun Li, 65322005"),
  tabsetPanel(
    tabPanel("Data",
             verbatimTextOutput("DataSummary"),
             plotOutput("BoxPlots"),
             DT::dataTableOutput("Table")
    ),
    tabPanel("Split",
             sliderInput("Split", "Train proportion", min = 0, max=1, value = 0.8),
             verbatimTextOutput("SplitSummary")
    ),
    tabPanel("GLMnet Model",
             tags$h3("Best tuning parameters:"),
             tableOutput("GlmModelSummary1"),
             hr(),
             plotOutput("GlmModelPlots"),
             verbatimTextOutput("GlmModelSummary2")
    ),
    tabPanel("PLS Model",
             tags$h3("Best tuning parameters:"),
             tableOutput("PlsModelSummary1"),
             hr(),
             plotOutput("PlsModelPlots"),
             verbatimTextOutput("PlsModelSummary2")
    ),
    tabPanel("ANN Model",
             tags$h3("Best tuning parameters:"),
             tableOutput("AnnModelSummary1"),
             hr(),
             plotOutput("AnnModelPlots"),
             verbatimTextOutput("AnnModelSummary2")
    ), 
    
    tabPanel("Cubist Model",
             tags$h3("Best tuning parameters:"),
             tableOutput("CubistModelSummary1"),
             hr(),
             plotOutput("CubistModelPlots"),
             verbatimTextOutput("CubistModelSummary2")
    ), 
    
    tabPanel("Elasticnet Model",
             tags$h3("Best tuning parameters:"),
             tableOutput("ElasticnetModelSummary1"),
             hr(),
             plotOutput("ElasticnetModelPlots"),
             verbatimTextOutput("ElasticnetModelSummary2")
    ), 

   tabPanel("Robust Linear Model",
          tags$h3("Best tuning parameters:"),
          tableOutput("RobustLinearModelSummary1"),
          hr(),
          plotOutput("RobustLinearModelPlots"),
          verbatimTextOutput("RobustLinearModelSummary2")
   ), 
 
    tabPanel("Model Selection",
             tags$h3("Cross validation results:"),
             checkboxInput("Notch", "Show notch", value = FALSE),
             plotOutput("SelectionBoxPlot"),
             radioButtons("Choice", "Model choice", choices = c("GLMnet", "PLS", "ANN","Cubist","Elasticnet","RobustLinear"), selected = "PLS")
    ),
    tabPanel("Performance",
             htmlOutput("Title"),
             verbatimTextOutput("TestSummary"),
             plotOutput("TestPlot")
    )
  )
))
