#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)

source("stocks.R")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Portfolio Analysis Tool"),

        sidebarLayout(
            sidebarPanel(
                helpText("Upload a .csv of your monthly returns. The format of the data needs to be such that each column is a stock and each row is
                         a closing value, with the earliest dates being at the top of the .csv file. Sample data below:"),
                downloadButton("sample_data", "Sample Data"),
                br(),
                br(),
                fileInput("upload", "Upload a file"),
                actionButton("minRiskPortfolio", label = "Get Minimum Risk Portfolio"),
                numericInput("num", label = h3("Get Portfolio For Expected Value"), value = .01, step=.01),
                actionButton("portfolioFromReturn", label = "Calculate")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           h2("Efficient Frontier"),
           h6("(File Uploaded Needed To Display)"),
           plotlyOutput("frontier"),
           h2("Portfolio"),
           textInput("portfolioName", "Portfolio Name", "New Portfolio"),
           actionButton("addPortfolio", label = "Add To Plot"),
           br(),
           br(),
           tabsetPanel(type = "tabs",
                       tabPanel("Plot", plotlyOutput("portfolio_hist")),
                       tabPanel("Summary", verbatimTextOutput("summary")),
                       tabPanel("Table", DT::dataTableOutput("portfolio_table")),
                       tabPanel("Stock Data", DT::dataTableOutput("stock_data"))
           ),
        )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {

    output$sample_data <- downloadHandler(
        filename = function() {
            paste0("sample.csv")
        },
        content = function(file) {
            write.csv(read.csv("stonks_cut.csv"), file, fileEncoding = "UTF-8", row.names = FALSE)
        }
    )
    
    data <- reactive({
        req(input$upload)
        read.csv(input$upload$datapath)
    })
    
    frontier <- reactive({
        req(input$upload)
        g <- plot_frontier(data())
        
        if(length(portfolio_list()) > 0) {
            for(i in 1:length(portfolio_list())) {
                portfolio <- portfolio_list()[[i]]
                
                sd <- get_portfolio_variance(portfolio, data())^.5
                expected <- get_portfolio_return(portfolio, data())
                name <- names(portfolio_list()[i])
                print(name)
                
                g <- g %>% add_trace(x = sd, y = expected, name = name, mode = 'markers')    
            }
        }
        
        g
    })
    
    portfolio <- reactiveVal()
    
    portfolio_list <- reactiveVal(list())
    
    observeEvent(input$portfolioFromReturn, {
        req(portfolio())
        portfolio(portfolio_from_return(data(), input$num))
    })
    
    observeEvent(input$minRiskPortfolio, {
        req(data())
        portfolio(get_min_risk(data()))
    })
    
    observeEvent(input$addPortfolio, {
        req(portfolio())
        portfolios <- length(portfolio_list())
        new_portfolio_list <- portfolio_list()
        new_portfolio_list[[portfolios + 1]] <- portfolio()
        names(new_portfolio_list)[portfolios + 1] <- input$portfolioName
        portfolio_list(new_portfolio_list)
    })
    
    output$frontier <- renderPlotly({
        frontier()
    })
    
    output$summary <- renderText({
        sd <- get_portfolio_variance(portfolio(), data())^.5
        expected <- get_portfolio_return(portfolio(), data())
        
        paste("Expected Return: ", expected, "\n", "Risk (Standard Deviation): ", sd, sep="")
    })
    
    output$portfolio_hist <- renderPlotly({
        if(!is.null(portfolio())) {
            plot_portfolio(portfolio())
        }
    })
    
    output$portfolio_table <- renderDataTable({
        portfolio()
    })
    
    output$stock_data <- renderDataTable({
        data()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
