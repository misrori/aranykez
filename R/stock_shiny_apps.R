#' enter pressed
#' @export
enter <- '
$(document).on("keyup", function(e) {
  if(e.keyCode == 13){
    Shiny.onInputChange("enterpressed", Math.random());
  }
});
'

#' shiny show one plot of stock market
#' @export
#' @import shiny
#' @import shinythemes
#' @importFrom stats setNames
#' @import shinycssloaders
shiny_one_stock <- function() {
  ui <- fluidPage(theme = shinytheme("cerulean"), title = 'Stock browser',
    div(uiOutput('ticker_out'),align='center'),
    div(uiOutput('linkbtn_out'), align='center'),
    withSpinner( plotOutput('stock_plot',height = '900'), type = 1,color = '#FFD700' )
  )
  server <- function(input, output, session) {
    output$ticker_out <- renderUI({
      selectInput('ticker', label = 'Select a company', setNames(data_all_stock[!grepl('.', data_all_stock$name, fixed = T)]$name, paste0(data_all_stock[!grepl('.', data_all_stock$name, fixed = T)]$description, ' (', data_all_stock[!grepl('.', data_all_stock$name, fixed = T)]$name, ')' ) ), selected = 'AAPL')
    })
    output$stock_plot <- renderPlot({
      tryCatch({
        stock_show_one_plot(ticker = input$ticker)
      }, error=function(x){
        return(NULL)
      })
    })
    output$linkbtn_out <- renderUI({
      # link <- sprintf('<a href="https://www.tradingview.com/chart?symbol=%s" target="_blank" class="btn btn-primary">%s</a>',val, val)
      link <- paste0("https://www.tradingview.com/chart?symbol=", input$ticker)
      shiny::actionButton(inputId='ab1', label="Go to tradingview",
                          icon = icon("chart-line"),
                          onclick =paste0("window.open('", link, "', '_blank')"))
    })

  }
  shinyApp(ui, server)
}


#' Fiter and show stock market
#' @import shiny
#' @import shinyWidgets
#' @import shinydashboard
#' @import shinyalert
#' @import data.table
#' @import shinycssloaders
#' @export
shiny_stock_browser <- function() {

ui <-
  dashboardPage(
    dashboardHeader(
      title = 'Multiple stock browser with filter'
    ),
    dashboardSidebar(
      sidebarMenu(id='all_sidebar',
                  menuItem("Filter", tabName = "filter", icon = icon("dashboard")),
                  menuItem("Base plot", icon = icon("th"), tabName = "baseplot")
      )
    ),
    dashboardBody(
      tags$script(enter),
      tabItems(
        tabItem(tabName = "filter",
                uiOutput('secindout')
        ),

        tabItem(tabName = "baseplot",
           div(uiOutput('ticker_list'), align='center'),
           div(uiOutput('linkbtn_out'), align='center'),
           withSpinner( plotOutput('stock_plot',height = '900'), type = 1,color = '#FFD700' )

        )
      )
    )
  )




# Define server logic required to draw a histogram
server <- function(input, output, session) {
  rv <- reactiveValues('tocheck'=NULL)

  output$secindout <- renderUI({
    wellPanel(
      selectInput(paste0('filter_','sector'), label = paste0('Filter by ','sector' ),choices = unique(data_all_stock[['sector']]), selected = unique(data_all_stock[['sector']]), multiple = T ),
      selectInput(paste0('filter_','industry'), label = paste0('Filter by ','industry' ),choices = unique(data_all_stock[['industry']]), selected = unique(data_all_stock[['industry']]), multiple = T ),
      sliderInput(paste0('filter_','rsi'), label = paste0('RSI'), min = 0, max =100, value = c(0, 100) ),
      div(actionButton('go', 'Go'),align="center")

    )
  })

  output$go_out<- renderUI({
    div(actionButton('gofiltersecind', 'Go'), align='center')
  })

  observeEvent(input$filter_sector,{
    updateSelectInput(session, inputId ='filter_industry', choices = unique(data_all_stock[sector%in%input$filter_sector]$industry), selected = unique(data_all_stock[sector%in%input$filter_sector]$industry) )
  })

  observeEvent(input$go,{
    df<- data_all_stock

    df <- df[sector%in%input$filter_sector & industry%in%input$filter_industry &
               RSI>=min(input$filter_rsi) & RSI<=max(input$filter_rsi) ,]

    setorder(df, -market_cap_basic)
    if (nrow(df)==0) {
      shinyalert(title = '',text = paste0('No match') , type = 'info' )

    }else{
      shinyalert(title = 'Találatok',text = paste0('Találatok száma: ', length(unique(df$ticker) )), type = 'info' )
    }

    rv$tocheck <-df

    output$ticker_list <- renderUI({
      selectInput('ticker', label = 'Select a company', setNames(df[!grepl('.', df$name, fixed = T)]$name, paste0(df[!grepl('.', df$name, fixed = T)]$description, ' (', df[!grepl('.', df$name, fixed = T)]$name, ')' ) ))
    })
    updateTabItems(session, inputId = 'all_sidebar', selected = 'baseplot'  )

  })

  output$linkbtn_out <- renderUI({
    # link <- sprintf('<a href="https://www.tradingview.com/chart?symbol=%s" target="_blank" class="btn btn-primary">%s</a>',val, val)
    link <- paste0("https://www.tradingview.com/chart?symbol=", input$ticker)
    shiny::actionButton(inputId='ab1', label="Go to tradingview",
                        icon = icon("chart-line"),
                        onclick =paste0("window.open('", link, "', '_blank')"))
  })

  observeEvent(input$enterpressed, {
    updateSelectInput(session, 'ticker',selected = rv$tocheck$name[ which(rv$tocheck$name==input$ticker)+1]  )
  })

  output$stock_plot <- renderPlot({
    tryCatch({
      stock_show_one_plot(input$ticker)
    }, error=function(x){
      return(NULL)
    })

  })

}

# Run the application
shinyApp(ui = ui, server = server)

}



