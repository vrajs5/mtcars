library(shiny)
library(markdown)

shinyUI(fluidPage(

  # Application title
  titlePanel("Analysis of mtcars Dataset"),
  
  sidebarLayout(
    sidebarPanel(
      # Creating UI from server for Exploratory Tab
      conditionalPanel(condition="input.conditionedPanels==1",
                       uiOutput('chartMenu')),     # Generating chart option from output
      
      conditionalPanel(condition="input.conditionedPanels==1",
                       uiOutput('controlForX')),   # Generating x axis option from output
    
      conditionalPanel(condition="input.conditionedPanels==1",
                       uiOutput('controlForGroup')),# Generating grouping option from output
      
      # Creating UI in client only for Modelling Tab
      conditionalPanel(condition="input.conditionedPanels==2",
                       selectInput(inputId = 'inputVar', multiple = TRUE,
                                   choices = c('disp', 'hp', 'drat', 'wt', 'qsec', 
                                               'cyl', 'vs', 'am', 'gear', 'carb'),
                                   label = 'Select Input Variable For Model',
                                   selected = 'wt'))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        # Tab for user help
        tabPanel("How to Use", includeMarkdown('about.md')),
        # Tab for exploratory analysis
        tabPanel("Exploratory Analysis", plotOutput("explPlot"),value=1),	
        # Tab for modelling
        tabPanel("Model Fitting", dataTableOutput("modelling"),value=2)
        , id = "conditionedPanels"
      )
    )
  )
))
