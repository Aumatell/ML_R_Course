

library(shiny)
library(shinythemes)
library(DT)
# 
ui <- fluidPage(theme = shinytheme("cyborg"),
  navbarPage(
  theme = "Cyborg",  # <--- To use a theme, uncomment this
  "Machine learning app name",
  tabPanel("DATA UPLOAD",
   
   # App title ----
  titlePanel(h1("UPLOAD DATA")),
  
   ################################################################
   #UPLOAD FILES
   ################################################################
   
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(h3("UPLOAD CONFIGURATION"),
      
      # Input: Select a file ----
      fileInput("file1", "Choose CSV File",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Checkbox if file has header ----
      checkboxInput("header", "Header", TRUE),
      
      # Input: Select separator ----
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
      
      # Input: Select quotes ----
      radioButtons("quote", "Quote",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = '"'),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Select number of rows to display ----
      radioButtons("disp", "Display",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "head")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(h3("DATA PREVIEW"),
      
      # Output: Data file ----
      tableOutput("contents"),
      
      )) # mainPanel
  
  ), # Navbar 1, tabPanel
  
  
  ###############################################################
  # DATA DESCRIPTION
  ###############################################################
  tabPanel("DATA DESCRIPTION", 
    sidebarPanel(h3("SELECT THE VARIABLE TO EXPLORE"),
      # Input: Select quotes ----
      uiOutput("variables")
      
  ), #sidebar close
  
  mainPanel(h3("This panel is intentionally left blank"),
            tableOutput("statistics"),
            plotOutput("plot")
            
            
            ) #mainpanel close
  
  ),
  
  
  ###############################################################
  # DATA PREPROCESSING
  ###############################################################
  tabPanel("DATA PREPROCESSINB",  
           sidebarPanel(h3("This panel is intentionally left blank")
           ),
           mainPanel(h3("This panel is intentionally left blank"))
           
  ),
  ###############################################################
  # MACHINE LEARNING MODELS CONFIGURATION
  ###############################################################
  tabPanel("MACHINE LEARNING MODELS CONFIGURATION",  
           sidebarPanel(h3("This panel is intentionally left blank")
           ),
           mainPanel(h3("This panel is intentionally left blank"))
           
  ),
  ###############################################################
  # MODEL SELECTION
  ###############################################################
  tabPanel("MODEL SELECTION",  
           sidebarPanel(h3("This panel is intentionally left blank")
           ),
           mainPanel(h3("This panel is intentionally left blank"))
           
  ),
  ###############################################################
  # PREDICTIONS
  ###############################################################
  tabPanel("PREDICTIONS",  
           sidebarPanel(h3("This panel is intentionally left blank")
           ),
           mainPanel(h3("This panel is intentionally left blank"))
           
  ),
  ###############################################################
  # DOWNLOAD MODEL
  ###############################################################
  tabPanel("DOWNLOAD MODEL",  
           sidebarPanel(h3("This panel is intentionally left blank")
           ),
           mainPanel(h3("This panel is intentionally left blank"))
           
  )
  ) # navbarPage)
  )

# Define server 
server <- function(input, output) {
  
  ################################################################
  # UPLOAD FILES
  ################################################################
  
    ## UPLOAD FILES
    input_data <- reactive({
      df<-read.csv(input$file1$datapath,
      header = input$header,
      sep = input$sep,
      quote = input$quote)
      return(df)
    })
      
    output$variables <- renderUI({
      radioButtons("dynamic", "NULL", choiceNames = colnames(input_data()), 
                          choiceValues =  seq(1,length(colnames(input_data())),by = 1))
      
    })
    
  ##   Table preview
    
   output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.

    if(input$disp == "head") {
      return(head(input_data()))
    }
    else {
      return(input_data())
    }
    
  })
   
   # Histogram( to delete)
  output$statistics <- renderPrint({
    dataf<-input_data()
    if (class(dataf[,as.numeric(input$dynamic)]) == "numeric" ){
      return(summary(dataf[,as.numeric(input$dynamic)]))
  }else{
      return(table(as.factor(dataf[,as.numeric(input$dynamic)])))
    }
    
  })
  
  output$plot <- renderPlot({
    dataf<-input_data()
    if (class(dataf[,as.numeric(input$dynamic)]) == "numeric" ){
      return(hist(dataf[,as.numeric(input$dynamic)]))
    }else{
      return(boxplot(table(as.factor(dataf[,as.numeric(input$dynamic)]))))
    }
  })
  ###############################################################
  # DATA DESCRIPTION
  ###############################################################

  
}

# Run the application 
shinyApp(ui = ui, server = server)

