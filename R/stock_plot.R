#' Plot one stock
#' @export
#' @param ticker The ticker id of the company
#' @param start_date start date of the plot
#' @param end_date end date of the plot
#' @param local_point_days numbar of days to became a local point
#' @param plot_title if you want to set the title
#' @param plot_subtitle if you want to set the plot subtitle
#' @param fullhd if you want to save as a full hd png
#' @import ggplot2
#' @importFrom tidyquant geom_candlestick
#' @importFrom ggrepel geom_label_repel
#' @importFrom grid grid.newpage grid.draw
#' @importFrom gridExtra grid.arrange
stock_show_one_plot <- function(ticker, start_date = (Sys.Date()-500), end_date=Sys.Date(), local_point_days= 20, plot_title= NULL, plot_subtitle=NULL, fullhd=FALSE) {

  # ticker <- 'RIDE'
  t <- utility_ad_local_min_max(stock_get_one_ticker(ticker = ticker, start_date =start_date , end_date = end_date),number_of_days = local_point_days)


  if (is.null(plot_title)) {
    sec_loc <- paste0('Sector location: ', which(data_all_stock[sector==data_all_stock[name==ticker]$sector,]$name==ticker) ,'/', length(data_all_stock[sector==data_all_stock[name==ticker]$sector,]$name)  )
    ind_loc <- paste0('Industry location: ', which(data_all_stock[industry==data_all_stock[name==ticker]$industry,]$name==ticker), '/', length(data_all_stock[industry==data_all_stock[name==ticker]$industry,]$name)    )

    my_title<-  paste0(data_all_stock[name==ticker]$description, ' (', ticker,  ') ', ' | $', round(data_all_stock[name==ticker]$close, 2),
                       ' | ',  data_all_stock[name==ticker]$sector, ' | ', data_all_stock[name==ticker]$industry )
  }else{
    my_title <- plot_title
  }

  if (is.null(plot_subtitle) ) {
    nb_imp <- ifelse(is.na(data_all_stock[name==ticker]$number_of_employees[1]), 'Unknown', format(as.numeric(data_all_stock[name==ticker]$number_of_employees[1]) , big.mark=',') )

    my_sub_title <-  paste0('Performance: | week: ', round(data_all_stock[name==ticker]$Perf.W, 2),  '% | ',
                            'month: ', round(data_all_stock[name==ticker]$Perf.1M, 2),  '% | ',
                            '6 months: ', round(data_all_stock[name==ticker]$Perf.6M, 2),  '% | ',
                            '1 year: ', round(data_all_stock[name==ticker]$Perf.Y, 2),  '%' ,'\n',
                            'Market cap: $', format(round(as.numeric(data_all_stock[name==ticker]$market_cap_basic/1000000), 2),big.mark = ','), ' Million ',
                            ' | Number of employees: ', nb_imp ,'\n',
                            sec_loc, ', ',ind_loc)
  }else{
    my_sub_title <- plot_subtitle
  }

  p1 <-   ggplot(t, aes(x = date, y = close, label=message)) +
    geom_candlestick(aes(open = open, high = high, low = low, close = close),colour_up = 'green') +theme_minimal()+
    geom_line(aes(y = ma_200_value), color = "red") +
    geom_line(aes(y = ma_50_value), color = "orange") +
    labs(title= my_title , x= '', y="$",
         subtitle =my_sub_title )+
    theme_bw()+
    geom_label_repel( box.padding = 0.5, max.overlaps = Inf )




  p2 <- ggplot(t, aes(x=date, y=rsi)) + geom_line()+
    geom_line(aes(y = 70), color = "black", linetype = "dashed") +
    geom_line(aes(y = 30), color = "black", linetype = "dashed") +theme_minimal()+
    labs( x= '', y="RSI")+
    theme_bw()
    # theme(axis.title = element_text(size=20))+
    # theme(axis.text=element_text(size=20))

  if (fullhd) {
    p1 <- p1+
    theme(plot.title = element_text(size=30))+
    theme(plot.subtitle = element_text(size=26))+
    theme(axis.text=element_text(size=20))+
    theme(axis.title = element_text(size=20))+
    geom_label_repel( size=6.5  )

    p2 <- p2+
      theme(axis.title = element_text(size=20))+
      theme(axis.text=element_text(size=20))
  }




  grid.newpage()
  grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2)))
  p<- grid.arrange(p1, p2, ncol = 1, heights = c(3, 1.2))
  return(p)
}


#' Compare multiple stock prices
#' @import data.table
#' @import plotly
#' @param  tickers list of the company tickers
#' @param start_date startdate of the data
#' @export
show_percent_change_of_stocks <- function(tickers, start_date ) {
  df <- rbindlist(lapply(tickers, stock_get_one_ticker, start_date=start_date, ad_local_text = T, ad_percent_change =T ))
  return(plot_ly(df, x=~date, y=~percent_change, color=~ticker, type = 'scatter', mode = 'lines', text=paste('Open: ', df$open, '\n', 'Close: ', df$close,'\n',
                                                                                                             'Low: ', df$low, '\n', 'High: ', df$high,'\n', 'Change: ', round(df$percent_change, 2),'%' )) %>%
           layout(yaxis = list(title='', ticksuffix = "%"), xaxis= list(title='')))
}







