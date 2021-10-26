

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
      
      radioButtons("transp", "Transposition",
                   choices = c("No" = 'False',
                               "Yes" = "True"),
                   selected = 'False'),
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
      uiOutput("variables"),
      tags$hr(),
      radioButtons("NumOrFac", "Num", 
                   choiceNames = c("NUMERICAL", "CATEGORICAL"), 
                   choiceValues = c("Num","Fac"))
      
  ), #sidebar close
  
  mainPanel(h3("This panel is intentionally left blank"),
            tableOutput("statistics"),
            tags$hr(),
            plotOutput("plot"),
            plotOutput("plot2")
            
            
            ) #mainpanel close
  
  ),
  
  
  ###############################################################
  # DATA PREPROCESSING
  ###############################################################
  tabPanel("DATA PREPROCESSINB",  
           sidebarPanel(h3("This panel is intentionally left blank"),
                        radioButtons("Trimout", "TRIMMING OF OUTLIERS", 
                                     choiceNames = c("YES", "NO"), 
                                     choiceValues = c("True","False"),
                                     selected = "True"),
                        tags$hr(),
                        radioButtons("Imputations", "IMPUTATION", 
                                     choiceNames = c("ZEROS", "MEAN", "SEGMENTED"), 
                                     choiceValues = c("Zeros","Mean", "Segmented"),
                                     selected = "Mean"),
                        tags$hr(),
                        radioButtons("Seasonality", "SEASONALITY", 
                                     choiceNames = c("YES", "NO"), 
                                     choiceValues = c("True","False"),
                                     selected = "False")
                        
           ), #sidebar pannel end
           mainPanel(h3("This panel is intentionally left blank"),
                     uiOutput("selection"),
                     tags$hr(),
                     uiOutput("response")
                     ) # main panel end
           
  ), # tabpanel end
  ###############################################################
  # BIVARIATE ANALYSIS
  ###############################################################
  tabPanel("BIVARIATE ANALYSIS",  
           sidebarPanel(h3("This panel is intentionally left blank"),
                        uiOutput("variables1"),
                        uiOutput("variables2"),
                        tags$hr(),
                        radioButtons("bivariate", "SELECT MODIFICATION IF NEEDED", 
                                     choiceNames = c("ZEROS", "MEAN", "SEASONALITY"), 
                                     choiceValues = c("Zeros", "Mean", "Seasonality")),
                        actionButton("Modify_variable", "RUN FOR THE SELECTED VARIABLE")
                        
           ),
           mainPanel(h3("This panel is intentionally left blank"))
           
  ),
  ###############################################################
  # MACHINE LEARNING MODELS CONFIGURATION
  ###############################################################
  tabPanel("MACHINE LEARNING MODELS CONFIGURATION",  
           sidebarPanel(h3("This panel is intentionally left blank"),
                        uiOutput("variables3"),
                        uiOutput("conditions")
                        
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
      ## INPUTS DEPENDENTS ON THE DATA FRAME
    output$variables <- renderUI({
      radioButtons("dynamic", "NULL", choiceNames = colnames(input_data()), 
                          choiceValues =  seq(1,length(colnames(input_data())),by = 1))
    })
    
    output$selection <- renderUI({
      checkboxGroupInput("inputvariables", "VARIABLES SELECTED TO TRAIN THE MODEL", choiceNames = colnames(input_data()), 
                   choiceValues = colnames(input_data()) )
    })
    
    output$response <- renderUI({
      selectInput("responsevariables", "RESPONSE VARIABLE SELECTED TO TRAIN THE MODEL", colnames(input_data()))
    })
    
    output$variables1 <- renderUI({
      selectInput("var1", "VARIABLE 1", colnames(input_data()))
    })
    
    output$variables2 <- renderUI({
      selectInput("var2", "VARIABLE 2", colnames(input_data()[!colnames(input_data()) %in% input$var1]))
    })
    output$variables3 <- renderUI({
      selectInput("var3", "VARIABLE 3", c("REG","CLAS"))
    })
    
    
    
    
    
    output$conditions <- renderUI({
      
      for (i in input$var3){
        
        if (i == ""){
          conditionalPanel(condition = "input.var3")
        }  
      
        
        
        
      }
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
  ###############################################################
  # DATA DESCRIPTION
  ###############################################################
   
  output$statistics <- renderPrint({
    dataf<-input_data()
    if (input$NumOrFac == "Num" ){
      dataf[,as.numeric(input$dynamic)] <- as.numeric(dataf[,as.numeric(input$dynamic)])
      as.table(summary(dataf[,as.numeric(input$dynamic)]))
  }else{
      dataf[,as.numeric(input$dynamic)] <- as.factor(dataf[,as.numeric(input$dynamic)])
      as.table(table(as.factor(dataf[,as.numeric(input$dynamic)])))
    }
    
  })
  
  output$plot <- renderPlot({
    dataf<-input_data()
    if (input$NumOrFac == "Num"){
      par(2,1)
      dataf[,as.numeric(input$dynamic)] <- as.numeric(dataf[,as.numeric(input$dynamic)])
      hist(dataf[,as.numeric(input$dynamic)], breaks = length(unique(dataf[,as.numeric(input$dynamic)])))
      output$plot2 <- renderPlot({
        boxplot(dataf[,as.numeric(input$dynamic)])
      })
    }else{
      dataf[,as.numeric(input$dynamic)] <- as.factor(dataf[,as.numeric(input$dynamic)])
      Table <-table(dataf[,as.numeric(input$dynamic)])
      
      plot(Table)
    }
  })


  
}

# Run the application 
shinyApp(ui = ui, server = server)

