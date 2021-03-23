
#' shiny try
#' @export
#' @import shiny
#' @import shinythemes
#' @importFrom stats setNames
#' @import shinycssloaders
shiny_one_stock <- function() {
  ui <- fluidPage(theme = shinytheme("cerulean"), title = 'Stock browser',
    div(uiOutput('ticker_out'),align='center'),
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
  }
  shinyApp(ui, server)
}
