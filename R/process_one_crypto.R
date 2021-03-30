#' Get info of a crypto token
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET content
#' @export
crypto_get_info_of_token <- function(ticker_slung) {

  res <- httr::GET(url = paste0('https://api.coingecko.com/api/v3/coins/', ticker_slung))
  t <- fromJSON(content(res, 'text'))
  return(t)
}

#' Get info of a crypto token
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET content
#' @importFrom  data.table data.table
#' @importFrom TTR RSI
#' @importFrom pracma movavg
#' @export
crypto_get_ticker_history  <- function(ticker_slung) {

  headers = c(
    `authority` = 'web-api.coinmarketcap.com',
    `accept` = 'application/json, text/plain, */*',
    `origin` = 'https://coinmarketcap.com',
    `user-agent` = 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/78.0.3904.108 Safari/537.36',
    `sec-fetch-site` = 'same-site',
    `sec-fetch-mode` = 'cors',
    `referer` = 'https://coinmarketcap.com/',
    `accept-encoding` = 'gzip, deflate',
    `accept-language` = 'hu-HU,hu;q=0.9,en-US;q=0.8,en;q=0.7'
  )

  params = list(
    `convert` = 'USD',
    `slug` = ticker_slung,
    `time_end` = Sys.Date(),
    `time_start` = Sys.Date()-700
  )

  res <- httr::GET(url = 'https://web-api.coinmarketcap.com/v1/cryptocurrency/ohlcv/historical', httr::add_headers(.headers=headers), query = params)

  t <- fromJSON(content(res, 'text'))
  pusd <-t$data$quotes$quote$USD
  pusd$date <- as.Date(substring(pusd$timestamp, 1,10))

  if (nrow(pusd)>50) {
    pusd$sma50 <- movavg(pusd$close, 50,type = "s")
  }else{
    pusd$sma50 <- min(pusd$min)*0.98
  }

  if (nrow(pusd)>200) {
    pusd$sma200 <- movavg(pusd$close, 200,type = "s")
  }else{
    pusd$sma200 <- min(pusd$min)*0.98
  }

  if (nrow(pusd)>15) {
    pusd$rsi <- RSI(pusd$close, n = 14)
  }else{
    pusd$rsi<- 30
  }
  pusd <- tail(pusd, 400)

  return(pusd)
}


#' Plot one crypto
#' @export
#' @param ticker_slung The crypto id on coingecko
#' @param just_show if just show the plot
#' @import ggplot2
#' @importFrom tidyquant geom_candlestick
#' @importFrom ggrepel geom_label_repel
#' @importFrom grid grid.newpage grid.draw
#' @importFrom gridExtra grid.arrange
crypto_show_one_ticker <- function(ticker_slung, just_show=T) {

  adat <- crypto_get_ticker_history(ticker_slung )
  adat <- utility_ad_local_min_max(adat, 15)

  info <- crypto_get_info_of_token(ticker_slung)

  if (length(info$error)>0) {
    my_title <- ticker_slung
    my_subt <- ''
  }else{
  market_cap <- paste0('$', format( info$market_data$market_cap$usd /1000000, big.mark = ',', digits = 2, nsmall = 2), ' Million')
  price <-   info$market_data$current_price$usd
  price <- ifelse(price>1, round(price, 2),round(price, 4) )
  price <-paste0('$', format( price , big.mark = ',', digits = 2, nsmall = 2))
  coinmarket_rank <- paste0('Coinmarket rank: ', info$market_cap_rank, '.')
  curculating_suply <- paste0( round(((info$market_data$circulating_supply / info$market_data$total_supply )*100 )  ,2 ),'%')
  curculating_suply <-  ifelse(curculating_suply=='%', '', curculating_suply)
  tags <- paste(info$categories, collapse = ' # ')
  daily_volume <- paste0('$', format(info$market_data$total_volume$usd/1000000, big.mark = ',', digits = 2, nsmall = 2), ' Million')
  my_title <- paste0(info$name, '(', info$symbol, ')', ' | ', price  ,' | Market cap: ',  market_cap )
  my_subt <- paste0('Performance |',
                    'Day: ', round(  info$market_data$price_change_percentage_24h , 2), '% |  ',
                    'Week: ', round( info$market_data$price_change_percentage_7d , 2), '% ',
                    'Month: ', round( info$market_data$price_change_percentage_30d , 2), '% ',  "\n",
                    'Circulating Supply: ',curculating_suply, ' | Daily volume: ',daily_volume, '\n',
                    'Tags: ', tags, ' | ', coinmarket_rank)

  }

  p1 <-   ggplot(adat, aes(x = date, y = close, label=message)) +
    geom_candlestick(aes(open = open, high = high, low = low, close = close),colour_up = 'green') +theme_minimal()+
    geom_line(aes(y = sma200), color = "red") +
    geom_line(aes(y = sma50), color = "orange") +
    theme_bw()+
    labs( x= '', y='USD', title = my_title, subtitle = my_subt)+
    geom_label_repel(  max.overlaps = Inf )


  p2 <- ggplot(adat, aes(x=date, y=rsi)) + geom_line()+
    geom_line(aes(y = 70), color = "black", linetype = "dashed") +
    geom_line(aes(y = 30), color = "black", linetype = "dashed") +
    theme_bw()+
    labs( x= '', y="RSI")

  grid.newpage()
  grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2)))
  p<- grid.arrange(p1, p2, ncol = 1, heights = c(3, 1.2))
  if (just_show==T) {
    return(p)
  }

}
