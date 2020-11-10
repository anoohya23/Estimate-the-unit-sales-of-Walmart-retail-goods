
library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(vroom)
library(lubridate)
library(stringr)
library(plotly)
library(mgcv)
library(shiny)


# Load data
#path <- "../input/m5-forecasting-accuracy/"
train <- vroom(str_c('E:/Shiny Data/sales_train_validation.csv'), delim=',', col_types=cols())
calendar <- read_csv(str_c('E:/Shiny Data/calendar.csv'), col_types=cols())
prices <- vroom(str_c('E:/Shiny Data/sell_prices.csv'), delim=",", col_types=cols())


# Variables used to communicate with UI.
store_names <- sort(unique(train$store_id))
store_codes <- as.list(seq(length(store_names)))
names(store_codes) <- store_names

dept_names <- sort(unique(train$dept_id))
dept_codes <- as.list(seq(length(dept_names)))
names(dept_codes) <- dept_names

item_names <- NULL
item_codes <- NULL

dept_and_item <- unique(train[, c('dept_id', 'item_id')])


# A utility function for converting wide-format data to long-format data,
# Basically same as that from https://www.kaggle.com/headsortails/back-to-predict-the-future-interactive-m5-eda
extract_ts <- function(df){
  if(nrow(df) == 0)
    return(NULL)
  
  min_date <- date("2011-01-29")
  df %>%
    select(item_id, dept_id, cat_id, store_id, state_id, starts_with("d_")) %>%
    pivot_longer(starts_with("d_"), names_to='date', values_to='sales') %>%
    mutate(date=as.integer(str_remove(date, 'd_'))) %>%
    mutate(date=min_date + date - 1)
}
df


# A Shiny UI
ui <- fluidPage(
  tags$script("
    Shiny.addCustomMessageHandler('resetValue', function(variableName) {
      Shiny.onInputChange(variableName, null);
    });
    "),
  titlePanel('Dashboard for M5 Forecasting'),
  
  sidebarLayout(
    sidebarPanel(
      h3('Where'),
      fluidRow(
        column(12,
               checkboxGroupInput('store',
                                  'Store',
                                  choices=store_codes,
                                  selected=1,
                                  inline=TRUE)
        )
      ),
      fluidRow(
        column(6,
               'Reset:',
               actionButton('all', 'All', width='100%'),
               actionButton('none', 'None', width='100%')
        ),
        column(6,
               'By state:',
               actionButton('CA', 'CA', width='100%'),
               actionButton('TX', 'TX', width='100%'),
               actionButton('WI', 'WI', width='100%')
        )
      ),
      
      h3('What'),
      fluidRow(
        column(12,
               selectInput('dept',
                           'Dept.',
                           choices=dept_codes,
                           selected=1),
               uiOutput('item'),
               radioButtons('weekday',
                            'Weekday',
                            choices=list('All'=1,
                                         'Mon-Fri'=2,
                                         'Sat-Sun'=3),
                            selected=1)
        )
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          'Sales by stores',
          checkboxInput('hide_real', 'Hide real sales', value=FALSE),
          checkboxInput('draw_smooth', 'Draw smoothed sales', value=FALSE),
          sliderInput('smooth_level', 'Confidence level', 0.5, 0.9995, value=0.95),
          checkboxInput('draw_price', 'Draw prices', value=FALSE),
          plotlyOutput(outputId='sales'),
          plotlyOutput(outputId='price')
        ),
        tabPanel(
          'Auto-correlations',
          sliderInput('lag_max', 'Maximum lag', 1, 400, value=100),
          plotOutput(outputId='acf')
        )
      )
    )
  )
)


# A Shiny server function
server <- function(input, output, session) {
  ## Observers for multiple selection of stores
  observeEvent(input$all, {
    updateCheckboxGroupInput(session,
                             'store',
                             'Store',
                             choices=store_codes,
                             inline=TRUE,
                             selected=seq(length(store_codes)))
  })
  
  observeEvent(input$none, {
    updateCheckboxGroupInput(session,
                             'store',
                             'Store',
                             choices=store_codes,
                             inline=TRUE,
                             selected=NULL)
  })
  
  observeEvent(input$CA, {
    updateCheckboxGroupInput(session,
                             'store',
                             'Store',
                             choices=store_codes,
                             inline=TRUE,
                             selected=1:4)
  })
  
  observeEvent(input$TX, {
    updateCheckboxGroupInput(session,
                             'store',
                             'Store',
                             choices=store_codes,
                             inline=TRUE,
                             selected=5:7)
  })
  
  observeEvent(input$WI, {
    updateCheckboxGroupInput(session,
                             'store',
                             'Store',
                             choices=store_codes,
                             inline=TRUE,
                             selected=8:10)
  })
  
  
  ## An observer to generate the selectInput for items dinamically.
  observeEvent(input$dept, {
    output$item <- renderUI({
      dept_i <- as.integer(input$dept)
      dept_name <- dept_names[dept_i]
      
      item_names <<- c('Dept_total',
                       sort(unique(dept_and_item[dept_and_item$dept_id %in% dept_name, ]$item_id)))
      item_codes <<- as.list(seq(length(item_names)))
      names(item_codes) <<- item_names
      
      selectInput('item',
                  'Item',
                  choices=item_codes,
                  selected=1)
    })
    
    session$sendCustomMessage(type='resetValue', message='item')
  })
  
  
  ## Reactive data
  salesTS <- reactive({
    store_i <- as.integer(input$store)
    store <- store_names[store_i]
    
    dept_i <- as.integer(input$dept)
    dept <- dept_names[dept_i]
    
    item_i <- as.integer(input$item)
    item <- item_names[item_i]
    
    if (length(item) != 1) return(NULL)
    
    ts <- if (item == 'Dept_total') {
      train %>%
        filter((store_id %in% store) & (dept_id == dept)) %>%
        mutate(item_id='Dept_total') %>%
        group_by(item_id, dept_id, cat_id, store_id, state_id) %>%
        summarise_if(is.numeric, sum) %>%
        extract_ts
    } else {
      train %>%
        select(-id) %>%
        filter((store_id %in% store) & (item_id == item)) %>%
        extract_ts
    }
    
    if (is.null(ts))
      return(NULL)
    
    ts <- left_join(ts, calendar, by='date')
    
    if (input$weekday == 2) {
      ts <- filter(ts, wday > 2)
    } else if (input$weekday == 3) {
      ts <- filter(ts, wday <= 2)
    }
    
    ts
  })
  
  priceTS <- reactive({
    if (!input$draw_price)
      return(NULL)
    
    store_i <- as.integer(input$store)
    store <- store_names[store_i]
    
    dept_i <- as.integer(input$dept)
    dept <- dept_names[dept_i]
    
    item_i <- as.integer(input$item)
    item <- item_names[item_i]
    
    if (length(item) != 1)
      return(NULL)
    
    ts <- if (item == 'Dept_total') {
      prices %>%
        filter((store_id %in% store) & (str_detect(item_id, dept))) %>%
        mutate(item_id='Dept_total') %>%
        group_by(store_id, item_id, wm_yr_wk) %>%
        mutate(sell_price=mean(sell_price[!is.na(sell_price)])) %>%
        ungroup()
    } else {
      prices %>%
        filter((store_id %in% store) & (item_id == item))
    }
    
    ts <- full_join(ts, calendar, by=c('wm_yr_wk'))
    
    ts
  })
  
  
  ## Plotting
  output$sales <- renderPlotly({
    if (is.null(salesTS())) return(NULL)
    
    sales <- salesTS()
    
    plt1 <- sales %>%
      ggplot() +
      theme_minimal() +
      labs(x="Date", y="Sales")
    
    if (!input$hide_real)
      plt1 <- plt1 + geom_line(aes(x=date, y=sales, col=store_id))
    
    if (input$draw_smooth) {
      x <- as.integer(sales$date - min(sales$date))
      y <- sales$sales
      model <- gam(y ~ s(x, bs='cs'),
                   data=data.frame(x=x, y=y),
                   family='poisson')
      yhat <- predict(model, type='response')
      
      level <- input$smooth_level
      plt1 <- plt1 +
        geom_line(aes(x=date, y=yhat), alpha=0.5) +
        geom_ribbon(aes(x=date,
                        ymin=qpois(1 - level, yhat),
                        ymax=qpois(level, yhat)),
                    alpha=0.2)
    }
    
    ggplotly(plt1, dynamicTicks=TRUE)
  })
  
  output$price <- renderPlotly({
    prices <- priceTS()
    if (is.null(prices))
      return(NULL)
    
    plt1 <- prices %>%
      ggplot(aes(x=date, y=sell_price, col=store_id)) +
      geom_line() +
      theme_minimal() +
      labs(x="Date", y="Price")
    
    ggplotly(plt1, dynamicTicks=TRUE)
  })
  
  output$acf <- renderPlot({
    if (is.null(salesTS())) return(NULL)
    
    sales_by_stores <- salesTS() %>%
      group_by(date, store_id) %>%
      summarise(sales=sum(sales)) %>%
      spread(store_id, sales)
    
    sales_by_stores$date <- NULL
    
    acf(sales_by_stores, lag.max=input$lag_max)
  })
}

shinyApp(ui=ui, server=server)