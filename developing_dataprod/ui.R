library(shiny)

shinyUI(pageWithSidebar(
    headerPanel("Find your Car !!!"),
    sidebarPanel(
        sliderInput('mpg', 'Min. mpg', value = 12, min = 10.4, max = 33.9, step = 0.5),
        sliderInput('cyl', 'Min.number of cyl', value = 5, min = 4, max = 8, step = 1),
        sliderInput('disp', 'Min. displacement', value = 150, min = 71.1, max = 472, step = 1),
        sliderInput('hp', 'Min. hp', value = 140, min = 52, max = 335, step = 10),
        sliderInput('am', 'Type of transmission (0=auto,1=manual)', value = 0,min = 0, max = 1, step = 1),
        submitButton('Find Car'),
        
        
        checkboxGroupInput("Par1", "Select field to Compare mpg performance:",
                           c("No of cylinder(cyl)" = "2",
                             "Displacement(disp)" = "3",
                             "HorsePower(hp)" = "4",
                             "Transmission type(am)" = "9")),
        submitButton('Plot Comparison')
    ),
    mainPanel(
        h3('List of Cars Available:'),
        dataTableOutput('carlist'),
        h3('The Comparison Plot'),
        #verbatimTextOutput("oPar1"),
        plotOutput('compplot')
    )
))