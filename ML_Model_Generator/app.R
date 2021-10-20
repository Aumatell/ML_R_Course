

library(shiny)
library(shinythemes)
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
      tableOutput("contents")
      )) # mainPanel
  
  ), # Navbar 1, tabPanel
  
  
  ###############################################################
  # DATA DESCRIPTION
  ###############################################################
  tabPanel("DATA DESCRIPTION", 
           sidebarPanel(h3("This panel is intentionally left blank")
  ),
  mainPanel(h3("This panel is intentionally left blank"))
  
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
  ) # navbarPage
  )
 

# Define server l
server <- function(input, output) {
   
  ## UPLOAD FILES
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    input_data <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
    
  })
  output$plot <- renderPlot({
    hist(as.data.frame(input_data)[,6])
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

