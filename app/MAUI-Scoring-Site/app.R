library(shiny)
library(tidyverse)
library(ggplot2)

# Define UI for data upload app ----
ui <- fluidPage(
    
    # App title ----
    titlePanel("Uploading Files"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            
            # Input: Select a file ----
            fileInput("file1", "Choose CSV File",
                      multiple = FALSE,
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
        mainPanel(
            
            # Output: Data file ----
            tableOutput("raw"),
            tableOutput("freq_table"),
            tableOutput("mass_table"),
            tableOutput("scores")
        )
        
    )
)

# Define server logic to read selected file ----
server <- function(input, output) {
    
    df <- setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("participant", "jot"))
    freq <- data.frame(matrix(ncol = 2, nrow = 0))
    mass <- data.frame(matrix(ncol = 6, nrow = 0))
    
    output$raw <- renderTable({
        
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.
        
        req(input$file1)
        
        # when reading semicolon separated files,
        # having a comma separator causes `read.csv` to error
        tryCatch(
            {
                df <<- read.csv(input$file1$datapath,
                               header = input$header,
                               sep = input$sep,
                               quote = input$quote)
                
                names(df) <<- c('participant', 'jot')
                
                df$jot <<- str_replace_all(df$jot, "[^[:alnum:]]", " ") %>% #gets rid of non alphanumerics
                    tolower() #' turns everything to lowercase
                
            },
            error = function(e) {
                # return a safeError if a parsing error occurs
                stop(safeError(e))
            }
        )
        
        if(input$disp == "head") {
            return(head(df))
        }
        else {
            return(df)
        }
        
    })
    
    output$freq_table <- renderTable({
        
        req(input$file1)
        freq <<- df %>%
            group_by(jot) %>%
            summarise(frequency = n()) %>%
            rename(response = jot)
  
        
        if(input$disp == "head") {
            return(head(freq))
        }
        else {
            return(freq)
        }
        
    })
    
    output$mass_table <- renderTable({
        
        req(input$file1)
        
        mass <- freq %>%
            group_by(frequency) %>%
            summarise(count = n()) %>%
            arrange(desc(frequency)) %>%
            mutate(mass = frequency*count) %>%
            mutate(cum_mass = cumsum(mass)) %>%
            mutate(MAUI = (cum_mass - mass/2)/max(cum_mass),
                   #UI = 1 - frequency/n,
                   norm_rank = (rank(cum_mass) - .5)/nrow(.))
        
        
        if(input$disp == "head") {
            return(head(mass))
        }
        else {
            return(mass)
        }
        
    })
    
}

# Create Shiny app ----
shinyApp(ui, server)