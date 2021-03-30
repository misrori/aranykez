#' shiny show one plot of crypto market
#' @export
#' @import shiny
#' @import shinythemes
#' @importFrom stats setNames
#' @import shinycssloaders
shiny_one_crypto <- function() {
  ui <- fluidPage(theme = shinytheme("cerulean"), title = 'Stock browser',
                  div(uiOutput('ticker_out'),align='center'),
                  div(uiOutput('linkbtn_out'), align='center'),
                  withSpinner( plotOutput('stock_plot',height = '900'), type = 1,color = '#FFD700' )
  )
  server <- function(input, output, session) {
    output$ticker_out <- renderUI({
      selectizeInput('ticker', label = 'Select a company', setNames( data_all_crypto$slug, paste0(data_all_crypto$name, ' (', data_all_crypto$symbol, ')' ) ), selected = 'AAPL')
    })
    output$stock_plot <- renderPlot({
      tryCatch({
        crypto_show_one_ticker(input$ticker)
      }, error=function(x){
        return(NULL)
      })
    })
    output$linkbtn_out <- renderUI({
      # link <- sprintf('<a href="https://www.tradingview.com/chart?symbol=%s" target="_blank" class="btn btn-primary">%s</a>',val, val)
      link <- paste0("https://www.coingecko.com/hu/coins/", input$ticker)
      shiny::actionButton(inputId='ab1', label="Go to coingecko",
                          icon = icon("chart-line"),
                          onclick =paste0("window.open('", link, "', '_blank')"))
    })
  }
  shinyApp(ui, server)
}




