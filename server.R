library(shiny)
library(ggplot2)
library(markdown)

shinyServer(function(input, output) {
  
  # Creating description frame for mtcars variables
  # which will be used to add proper labels in graphs
  getDescription = reactive({
    read.table(text = 'code,description
mpg,Miles/(US) gallon
cyl,Number of cylinders
disp,Displacement (cu.in.)
hp,Gross horsepower
drat,Rear axle ratio
wt,Weight (lb/1000)
qsec,1/4 mile time
vs,V/S
am,Transmission
gear,Number of forward gears
carb,Number of carburetors',sep=',',header =TRUE)
  })
  
  # Generating chart type menu
  output$chartMenu <- renderUI({
    selectInput(inputId = 'chartType', label = 'Select type of chart',
                choices = c('line chart', 'box plot'))
  })
  
  # Generating x axis menu
  output$controlForX <- renderUI({
    if(length(input$chartType) == 0) return
    
    # mpg cyl disp  hp drat    wt  qsec vs am gear carb
    continuousType = c('disp', 'hp', 'drat', 'wt', 'qsec')
    discritType = c('cyl', 'vs', 'am', 'gear', 'carb')
    
    # If chart type is line chart then we need x axis
    # as continous variable
    if(input$chartType == 'line chart'){
      selectInput(inputId = 'xAxis', label = 'Select X-Axis Input', 
                  choices = continuousType)
    }
    # If chart type is boxplot then we need x axis
    # as discrit variable
    else if(input$chartType %in% c('bar chart','box plot')){
      selectInput(inputId = 'xAxis', label = 'Select X-Axis Input',
                  choices = discritType)      
    }
  })
  
  # Generating grouping menu
  output$controlForGroup <- renderUI({
    if(length(input$xAxis) == 0) return
    
    discritType = c('cyl', 'vs', 'am', 'gear', 'carb')
   
    # Getting variable which is used in x-axis
    selectedX = input$xAxis
    
    # Only for line chart we will populate group option
    # all discrit variable except selected as x axis will
    # be populated in dropdown
    if(input$chartType == c('line chart')){
      selectInput(inputId = 'groupBy', label = 'Select Categorical Variable (Optional)', 
                  choices = c('(none)', discritType[ !(discritType == selectedX)]),
                  selected = '')
    }
  })
  
  # Generating exploratory tab ouput
  output$explPlot <- renderPlot({
    if(length(input$chartType) == 0) return
    if(length(input$xAxis) == 0) return
    
    reading = transform(mtcars, 
                        cyl = as.factor(cyl),
                        vs = as.factor(vs),
                        am = as.factor(am),
                        gear = as.factor(gear),
                        carb = as.factor(carb))
    
    # Outcome is fixed
    yAxis = 'mpg'
    
    # Getting description frame
    label = getDescription()
    
    # Generating initial frame for ggplot
    chartVar = ggplot(data = reading) 
    
    # Populate description for x and y
    x = label[label$code == input$xAxis,]$description
    y = label[label$code == yAxis,]$description
    
    # Populate line chart
    if(input$chartType == 'line chart'){
      # If grouping variable is selected 
      if(input$groupBy != '(none)'){
        grp = label[label$code == input$groupBy,]$description
        
        # Generate final graph
        chartVar = chartVar + 
          geom_line(aes_string(x = input$xAxis, y = yAxis, 
                               color = input$groupBy)) +
          labs(x = label[label$code == input$xAxis,]$description,
               y = label[label$code == yAxis,]$description,
               title = paste(y,'v/s',x,'grouped by',grp))
      }
      else{
        
        # Generate final graph
        chartVar = chartVar  +
          geom_line(aes_string(x = input$xAxis, y = yAxis), 
                    color = 'green') +
          labs(x = label[label$code == input$xAxis,]$description,
               y = label[label$code == yAxis,]$description,
               title = paste(y,'v/s',x))
      }
    }
    # Populate box plot
    else if(input$chartType == 'box plot'){
      # Generate final graph
      chartVar = chartVar +
        geom_boxplot(aes_string(x = input$xAxis, 
                                fill = input$xAxis,  
                                y = yAxis)) +
        labs(x = label[label$code == input$xAxis,]$description,
             y = label[label$code == yAxis,]$description,
             title = paste(y,'v/s',x))
    }
    
    # Finally printing graph
    print(chartVar) 
  })
  
  # Generating modelling tab ouput
  output$modelling <- renderDataTable({
    reading = transform(mtcars, 
                        cyl = as.factor(cyl),
                        vs = as.factor(vs),
                        am = as.factor(am),
                        gear = as.factor(gear),
                        carb = as.factor(carb))
    
    # Creating formula
    inVar = paste(input$inputVar,collapse = '+')
    
    # If nothing is selected then creating model with 
    # all variable
    if(length(input$inputVar)==0){
      forml = formula('mpg~.')
    }
    # Attaching selected input variable
    else{
      forml = formula(paste('mpg~',inVar))
    }
    
    # Create model
    fit <- lm(forml, mtcars)
    
    # Populate final output
    data.frame(Model = rownames(mtcars),
               Actual_Value = mtcars$mpg,
               Fitted_Value = round(fit$fitted.values,1))
  })
})
