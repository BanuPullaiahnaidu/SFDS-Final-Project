library(class)
library(shiny)
library(caret)
library(ggplot2)
library(dplyr)
library(magrittr)
library(tidyverse)
library(readr)
library(plotly)

ui <- fluidPage(
  # App title ----
  titlePanel("House Prices - Advanced Regression Techniques!"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a file ----
  fileInput("file1", "Choose the Training CSV file", multiple = FALSE, accept = c(".csv", ".txt")),
#  fileInput("file2", "Choose the Test CSV file", multiple = FALSE, accept = c(".csv", ".txt")),
  
 
  selectizeInput(
    inputId = "ExpVar",
    label = "Select an Explanatory Variable",
    choices = c(),
    multiple = FALSE,
    selected = "GrLivArea",
    options = list(maxItems = 1)
  ),
 
  selectizeInput(inputId = "ResVar", label = "Select a Response Variable", choices = c(),
               multiple = FALSE, selected = "SalePrice",options = list(maxItems = 1)),
  
  checkboxInput("SepNeighborhood", "Look at Neighborhoods separately?")
    ),
  
  # Main panel for displaying outputs ----
    tabsetPanel(
      tabPanel("Data Review", 
               # content for the first tab goes here
               tableOutput("headTrain")
               #tableOutput("headTest"),
               #tableOutput("Selection"),
     ),
      tabPanel("Plots", 
               # content for the second tab goes here
               fluidRow(
                 column(6, plotOutput(outputId = "Plot11")),
                 column(6, plotOutput(outputId = "Plot12"))
              ),
               fluidRow(
               column(6,plotOutput(outputId = "Plot21")),
               column(6,plotOutput(outputId = "Plot22"))
               
              )
            )
   )
  
),
)



server <- function(input, output, session) {
  
  HPA <- reactive({
    req(input$file1)
     
    ext <- tools::file_ext(input$file1$name)
    switch(ext,
           csv = vroom::vroom(input$file1$datapath, delim = ","),
           txt = vroom::vroom(input$file1$datapath, delim = "\t"),
           validate("Invalid file; Please upload a .csv or .txt file")
    )
    inFile <- input$file1
    df <- read_csv(inFile$datapath, col_types = cols())

    updateSelectInput(session, "ExpVar", choices = colnames(df), selected = "GrLivArea")
    updateSelectInput(session, "ResVar", choices = "SalePrice")

    # Check if each column has only integer values and convert to integer if true
    #df <- purrr::map_df(df, ~ if (all(is.na(.x) | is.numeric(.x)) && all(floor(.x) == .x)) {as.integer(.x)} else {.x})
    return(df)
   
  })


  
  output$headTrain <- renderTable({
    req(HPA())
    head(HPA(), 5) 
    #as.data.frame(as.list(head(HPA(), 5)))
    })


  output$Plot11 <- renderPlot({
       {HPA() %>%  filter(Neighborhood == 'NAmes'| Neighborhood == 'Edwards' | Neighborhood == 'BrkSide') %>% ggplot(aes(x = GrLivArea, y = SalePrice)) + geom_point(aes(color = Neighborhood)) + geom_smooth(method = "lm") + theme(legend.position = "right")  + ggtitle("Home Price Analysis by Neighborhood: Sale Price v. Gross Living Area")}
  
  })
  
  output$Plot12 <- renderPlot({
    if(input$SepNeighborhood == TRUE)
    {HPA() %>%  filter(Neighborhood == 'NAmes') %>% ggplot(aes(x = GrLivArea, y = SalePrice)) + geom_point(aes(color = Neighborhood)) + geom_smooth(method = "lm")  + theme(legend.position = "right")  + ggtitle("Home Price Analysis for NAmes Neighborhood: Sale Price v. Gross Living Area")}
    
  })
  
  output$Plot21 <- renderPlot({
    if(input$SepNeighborhood == TRUE)
    {HPA() %>%  filter(Neighborhood == 'Edwards') %>% ggplot(aes(x = GrLivArea, y = SalePrice)) + geom_point(aes(color = Neighborhood)) + geom_smooth(method = "lm")  + theme(legend.position = "right")  + ggtitle("Home Price Analysis for Edwards Neighborhood: Sale Price v. Gross Living Area")}
    
    
  })
  
  output$Plot22 <- renderPlot({
    if(input$SepNeighborhood == TRUE)
    {HPA() %>%  filter(Neighborhood == 'BrkSide')  %>% ggplot(aes(x = GrLivArea, y = SalePrice)) + geom_point(aes(color = Neighborhood)) + geom_smooth(method = "lm") + theme(legend.position = "right")  + ggtitle("Home Price Analysis for BrookkSide Neighborhood: Sale Price v. Gross Living Area")}
    
    
  })

  }

shinyApp(ui, server)