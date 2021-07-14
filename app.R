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
library(plotly)
library(stringr)

source("stocks_new.R")

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
                                             choices=c("Historical Covariance"="historical",
                                                       "Single Index Model"="SIM",
                                                       "Constant Correlation Model" = "CC",
                                                       "Multi-Group Model" = "MGM")),
                                 selectInput("short", "Short Sales Allowed? (Non Historical-Covariance Models Only, No Efficient Frontier Will Be Drawn)",
                                             c("Yes" = TRUE,
                                               "No" = FALSE))),
                        
                        tabPanel("Multi-Group Model",
                                 helpText("This requires that the uploaded stock data is formatted such that stocks
                                              in the same industry are in adjacent columns. Breaks also must be input so that the model
                                              can tell which stocks belong to which industry. For example, if you have 3 industries for 3 stocks
                                              each, the breaks would be input as '3, 6, 9'."), 
                                 br(),
                                 textInput("industry_breaks", "Breaks For Industries", "3, 6, 9")),
                        
                        # tabPanel("Constant Correlation Model",
                        #          helpText("This requires that the uploaded stock data is formatted such that stocks
                        #                       in the same industry are in adjacent columns. Breaks also must be input so that the model
                        #                       can tell which stocks belong to which industry. For example, if you have 3 industries for 3 stocks
                        #                       each, the breaks would be input as '3, 6, 9'."), 
                        #          br(),
                        #          textInput("industry_breaks", "Breaks For Industries", "3, 6, 9")),
                        
                        # tabPanel("Single Index Model",
                        #          helpText("Upload the index in setup and decide if short sales should be allowed."), 
                        #          br(),
                        #          selectInput("SIM_short", "Short Sales Allowed?",
                        #                      c("Yes" = TRUE,
                        #                        "No" = FALSE))),
                        
                        
                        tabPanel("Find Portfolios",
                                #numericInput("num", label = h3("Get Portfolio For Expected Value"), value = .01, step=.01),
                                #actionButton("portfolioFromReturn", label = "Calculate"), 
                                #br(), 
                                numericInput("rf", label = h3("Add Risk Free Asset"), value = 0, step=.001),
                                textInput("rfName", "RF Name", "RF"),
                                actionButton("optimumPortfolio", label = "Find Optimum Portfolio"),
                                #actionButton("minRiskPortfolio", label = "Get Minimum Risk Portfolio"),
                                actionButton("clearPortfolios", label="Clear Portfolios")))),
        
        # Show a plot of the generated distribution
        mainPanel(
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
            h2("Efficient Frontier"),
            h6("(Plot Needs to be Added to Display)"),
            plotlyOutput("frontier")
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
        
        req(length(portfolio_list()) > 0)
        #g <- plot_frontier(portfolio())
        
        g <- plot_ly()
        
        x <- list(
            title = "Standard Deviation (Risk)"
        )
        y <- list(
            title = "Expected Return"
        )
        
        g <- g %>% layout(xaxis = x, yaxis = y)
        
        if(length(portfolio_list()) > 0) {
            for(i in 1:length(portfolio_list())) {
                portfolio <- portfolio_list()[[i]]

                rf <- portfolio$rf

                sd <- portfolio$port_var^.5
                expected <- portfolio$port_return
                name <- portfolio$name

                if(portfolio$shorts_allowed == TRUE) {
                    df <- plot_frontier(portfolio)
                    
                    g <- g %>% add_trace(x = df$sdeff, y= df$y1,  type='scatter', mode = 'lines', name=paste("Expected Returns From:", name))
                }
                
                g <- g %>% add_trace(x = sd, y = expected, name = name, mode = 'markers')
                
                if(!is.na(rf)) {
                    
                    name_rf <- portfolio$rf_name
                    g <- g %>% add_trace(x = 0, y = rf, name = name_rf, mode='markers', type='scatter')

                    slope <- (rf - expected)/(0-sd)
                    new_x <- sd + .25
                    new_y <- new_x * slope + rf

                    g <- g %>% add_segments(x = new_x, y = new_y, xend = 0, yend = rf, name=paste("Capital Allocation Line:", name))

                }
            }
        }

        g %>% layout(xaxis = list(range = c(0, .2)), yaxis = list(range=c(-.2, .2)))
    })
    
    portfolio <- reactiveVal()
    
    portfolio_list <- reactiveVal(list())
    
    observeEvent(input$portfolioFromReturn, {
        req(data())
        portfolio(build_portfolio(stocks=data(), E=input$num, method=input$method, name=input$portfolioName))
    })
    
    observeEvent(input$minRiskPortfolio, {
        req(data())
        portfolio(build_portfolio(stocks=data(), method=input$method, name=input$portfolioName))
    })
    
    observeEvent(input$optimumPortfolio, {
        req(data())
        
        rf <- input$rf
        shorts_allowed <- as.logical(input$short)
        
        print(shorts_allowed)
        
        if(input$method == "SIM" & rf > 0) {
            index <- read.csv(input$index_data$datapath)
            portfolio(build_portfolio(stocks=data(), rf=input$rf, rf_name=input$rfName, method=input$method, 
                                      name=input$portfolioName, index=index, shorts_allowed = shorts_allowed))
        }
        
        else if(input$method == "CC" & rf > 0) {
            portfolio(build_portfolio(stocks=data(), rf=input$rf, rf_name=input$rfName, method=input$method, 
                                      name=input$portfolioName, shorts_allowed = shorts_allowed))
        }

        else if(input$method == "historical" & rf > 0) {
            portfolio(build_portfolio(stocks=data(), rf=input$rf, rf_name=input$rfName, method=input$method, 
                                      name=input$portfolioName, shorts_allowed = TRUE))
        }
        
        else if(input$method == "MGM" & rf > 0) {
            breaks <- str_split(input$industry_breaks, ",")[[1]] %>% str_trim() %>% as.numeric()
            
            portfolio(build_portfolio(stocks=data(), rf=input$rf, rf_name=input$rfName, method=input$method, 
                                      name=input$portfolioName, shorts_allowed = TRUE, breaks=breaks))
        }
        
    })
    
    observeEvent(input$addPortfolio, {
        req(portfolio())
        

            
        portfolios <- length(portfolio_list())
        new_portfolio_list <- portfolio_list()
        new_portfolio_list[[portfolios + 1]] <- portfolio()
        names(new_portfolio_list)[portfolios + 1] <- input$portfolioName
        portfolio_list(new_portfolio_list)
        
        print(portfolio_list())
        
        
        
    })
    
    observeEvent(input$clearPortfolios, {
        portfolio_list(list())
    })
    
    output$frontier <- renderPlotly({
        frontier()
    })
    
    output$summary <- renderText({
        req(portfolio())
        sd <- portfolio()$port_var^.5
        expected <- portfolio()$port_return
        
        paste("Expected Return: ", expected, "\n", "Risk (Standard Deviation): ", sd, sep="")
    })
    
    output$portfolio_hist <- renderPlotly({
        if(!is.null(portfolio())) {
            plot_portfolio(portfolio())
        }
    })
    
    output$portfolio_table <- renderDataTable({
        portfolio()$weights
    })
    
    output$stock_data <- renderDataTable({
        data()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
