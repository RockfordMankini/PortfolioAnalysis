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
            tabsetPanel(type="tabs",
                        tabPanel("Setup", 
                                 helpText("Upload a .csv of your monthly returns. The format of the data needs 
                                    to be such that each column is a stock and each row is
                                    a closing value, with the earliest dates being at the top of the .csv file. Sample data below:"),
                                 downloadButton("sample_data", "Sample Data"),
                                 br(),
                                 br(),
                                 fileInput("stock_data", "Upload Stock Data"),
                                 helpText("If you want to analyze the portfolio using the single index model, upload a market index
                                              such as the S&P 500 (in the same format as the previous data) below. Sample Index Data below:"),
                                 downloadButton("sample_data_index", "Sample Data (S&P 500)"),
                                 br(),
                                 br(),
                                 fileInput("index_data", "Upload Index Data (Optional)")),
                        
                        tabPanel("Model Selection", 
                                 selectInput("method", label="Select a model", 
                                             choices=c("Historical Covariance"="historical"))),
                        tabPanel("Multi-Group Model",
                                 helpText("This requires that the uploaded stock data is formatted such that stocks
                                              in the same industry are in adjacent columns. Breaks also must be input so that the model
                                              can tell which stocks belong to which industry. For example, if you have 3 industries for 3 stocks
                                              each, the breaks would be input as '3, 6, 9'."), 
                                 br(),
                                 textInput("industry_breaks", "Breaks For Industries", "3, 6, 9")),
                        
                        tabPanel("Find Portfolios", 
                                 actionButton("minRiskPortfolio", label = "Get Minimum Risk Portfolio"),
                                 numericInput("num", label = h3("Get Portfolio For Expected Value"), value = .01, step=.01),
                                 actionButton("portfolioFromReturn", label = "Calculate"), 
                                 br(), 
                                 numericInput("rf", label = h3("Add Risk Free Asset"), value = 0, step=.001),
                                 textInput("rfName", "RF Name", "RF"),
                                 actionButton("optimumPortfolio", label = "Find Optimum Portfolio"),
                                 actionButton("clearPortfolios", label="Clear Portfolios")))),
        
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
    
    output$sample_data_index <- downloadHandler(
        filename = function() {
            paste0("sample_index.csv")
        },
        content = function(file) {
            write.csv(read.csv("index.csv"), file, fileEncoding = "UTF-8", row.names = FALSE)
        }
    )
    
    data <- reactive({
        req(input$stock_data)
        read.csv(input$stock_data$datapath)
    })
    
    frontier <- reactive({
        req(input$stock_data)
        g <- plot_frontier(data(), input$method)
        
        if(length(portfolio_list()) > 0) {
            for(i in 1:length(portfolio_list())) {
                portfolio <- portfolio_list()[[i]]
                
                rf <- portfolio_rf_list()[i]
                
                sd <- get_portfolio_variance(portfolio, data(), input$method)^.5
                expected <- get_portfolio_return(portfolio, data())
                name <- names(portfolio_list()[i])
                
                print(rf)
                
                name_rf <- names(rf)
                
                g <- g %>% add_trace(x = sd, y = expected, name = name, mode = 'markers') 
                
                print(rf)
                
                if(rf > 0) {
                    g <- g %>% add_trace(x = 0, y = rf, name = name_rf, mode='markers')
                    
                    slope <- (rf - expected)/(0-sd)
                    new_x <- sd + .25
                    new_y <- new_x * slope + rf
                    
                    g <- g %>% add_segments(x = new_x, y = new_y, xend = 0, yend = rf, showlegend=FALSE)
                    
                }
            }
        }
        
        g
    })
    
    portfolio <- reactiveVal()
    
    portfolio_rf <- reactiveVal()
    
    portfolio_list <- reactiveVal(list())
    
    portfolio_rf_list <- reactiveVal(numeric())
    
    observeEvent(input$portfolioFromReturn, {
        req(data())
        portfolio(portfolio_from_return(data(), input$num, input$method))
        portfolio_rf(0)
    })
    
    observeEvent(input$minRiskPortfolio, {
        req(data())
        portfolio(get_min_risk(data(), input$method))
        portfolio_rf(0)
    })
    
    observeEvent(input$optimumPortfolio, {
        req(data())
        
        print(breaks)
        
        rf <- input$rf
        
        if(rf > 0) {
            portfolio(get_optimum_portfolio(data(), rf, input$method))
            portfolio_rf(rf)
        }
        
    })
    
    observeEvent(input$addPortfolio, {
        req(portfolio())
        
        portfolios <- length(portfolio_list())
        new_portfolio_list <- portfolio_list()
        new_portfolio_list[[portfolios + 1]] <- portfolio()
        names(new_portfolio_list)[portfolios + 1] <- input$portfolioName
        portfolio_list(new_portfolio_list)
        
        new_rfs <- portfolio_rf_list()
        new_rfs <- c(new_rfs, portfolio_rf())
        names(new_rfs)[length(new_rfs)] <- input$rfName
        portfolio_rf_list(new_rfs)
        
    })
    
    observeEvent(input$clearPortfolios, {
        portfolio_list(list())
        portfolio_rf_list(numeric())
    })
    
    output$frontier <- renderPlotly({
        frontier()
    })
    
    output$summary <- renderText({
        req(portfolio())
        sd <- get_portfolio_variance(portfolio(), data(), input$method)^.5
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
