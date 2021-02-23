p <- c('BatchGetSymbols', 'Quandl', 'BETS', 'dplyr','splitstackshape','compare',
       'GetDFPData', 'GetTDData', 'quantmod','TTR','tidyquant','Metrics','tibble',
       'lubridate','stringr','foreach','glmnet','Hmisc','caret','xgboost','e1071',
       'kernlab','elasticnet','Cubist','lplyr','tidyr','reshape2','purrr','tseries',
       'lmtest','ggplot2','forecast','shiny','urca','gridExtra','cowplot')

install.packages(p)
lapply(p, require, character.only = TRUE)

# set tickers
df.ibov <- GetIbovStocks()
tickers <- paste0(df.ibov$tickers, '.SA')

# define UI
ui <- fluidPage(
  # App title
  titlePanel('Cointegration Stocks Exercise'),
  sidebarLayout(
    sidebarPanel(
      dateRangeInput('date', h5('date range'), start = '2020-01-01'),
      selectInput('selecty', h6('Y variable'),
                  choices = tickers, selected = tickers[1]),
      selectInput('selectx', h6('X variable'),
                  choices = tickers, selected = tickers[2])),

    mainPanel(
      plotOutput(outputId = 'plot'),
      plotOutput(outputId = 'plot1'),
      verbatimTextOutput(outputId = 'text'),
        )
      )
    )


# define server logic
server <- function(input, output) {

  data <- reactive({
    temp.data <- BatchGetSymbols(ticker = c(input$selecty, input$selectx),
                                 first.date = input$date[1],
                                 last.date = input$date[2])

    ydf.tickers <- temp.data$df.tickers %>%
      filter(ticker == input$selecty)

    xdf.tickers <- temp.data$df.tickers %>%
      filter(ticker == input$selectx)

    temp.df <- tibble(
      y = ydf.tickers[,6],
      x = xdf.tickers[,6],
      date = ydf.tickers[,7]
    )
  })

  output$plot <- renderPlot({
    temp.df <- data()
    ggplot(temp.df, aes(date, y)) +
      geom_line(alpha = 0.8, linetype = 1, color = 'black') +
      geom_line(aes(y = x), linetype = 1, color = 'blue',
                alpha = 0.8) +
      labs(x = 'date', y = 'price',
           title = 'Prices Evolution')
  })

  output$plot1 <- renderPlot({
    temp.df <- data()
    lm <- lm(y ~ x, data = temp.df)
    ur <- ur.df(lm$residuals)
    plot(ur)
  })

  output$text <- renderPrint({
    temp.df <- data()
    lm <- lm(y ~ x, data = temp.df)
    ur <- ur.df(lm$residuals)
    adf <- adf.test(lm$residuals)
    print(ur@cval)
    print(ur@teststat)
    print(adf$statistic)
    print(adf$p.value)
  })
}

# run the app
shinyApp(ui = ui, server = server)
