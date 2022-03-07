library(fpp3)
library(readr)
library(shinyWidgets)
library(shiny)
#shinyWidgetsGallery()

# Read zipped data
stocks <- read_csv("nyse_stocks.csv.zip")

# Convert to `tsibble()`
stocks$date <- as.Date(stocks$date)
stocks <- tsibble(stocks, index = date, key = symbol)

# 1 stock
selected_stock <- "AAPL"

stocks %>%
  filter(symbol == selected_stock) %>%
  autoplot(open) +
  labs(title = selected_stock)

# Multiple stocks
selected_stocks <- c("GOOG", "AAPL")

stocks %>%
  filter(symbol %in% selected_stocks) %>%
  autoplot(open)

#Create Year Range Vector
Year_Range <- c("2010/1/1","2011/1/1","2012/1/1","2013/1/1","2014/1/1","2015/1/1","2016/1/1","2017/1/1")

?awesomeRadio()


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Group Assignment 2 - Stocks!"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
     selectizeInput(inputId="stockselect",
                     label="Select some stocks!",
                     choices=unique(stocks$symbol),
                     multiple=TRUE),
     awesomeRadio(
       inputId = "yvar",
       label = "Select a variable to study:", 
       choices = c("open","volume"),
       selected = "open"
     ),
     sliderTextInput(
       inputId = "Date",
       label = "Choose a Date Range:", 
       choices = Year_Range,
       selected = Year_Range[c(1, 2)]
     )
    ),
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("timePlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$timePlot <- renderPlot({
    # draw the time plot with selected symbols and y var
    selected_stocks <- input$stockselect
    stocks %>%  
      filter(symbol %in% selected_stocks) %>%
      select(symbol,input$yvar) %>% 
      autoplot() + scale_x_date(limits = as.Date(c(input$Date[1],input$Date[2])))

    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
